(ns plauna.application-test
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [clojure.java.io :as io]
            [plauna.interfaces :as int]
            [taoensso.telemere :as t]
            [plauna.core.email :refer :all]
            [plauna.database :as db]
            [plauna.analysis :an al]
            [plauna.client.parser :as imap-parser]
            [plauna.files :as files]
            [plauna.application :as app])
  (:import [plauna.database SqliteDB]
           [plauna.analysis BasicAnalyzer ]))

(t/set-ns-filter! {:disallow "plauna.*"})

(t/set-min-level! :error)

(defn setup-clean-db [f]
  (swap! files/plauna-config (fn [_] {:data-folder "tmp/"}))
  (files/check-and-create-database-file)
  (db/create-db)
  (alter-var-root #'db/batch-size (fn [_] 2))
  (f)
  (files/delete-database-file))

(use-fixtures :each setup-clean-db)

(defn fake-connection-with-config [config]
  (defrecord TestConnection [config] int/IMAPConnection
             (connect [_])
             (monitor-folders [_]))
  (->TestConnection config))

(deftest basic-auth
  (let [database (reify int/DB (fetch-connection [_ id] {:id id :auth-type "basic"}))
        connection (reify int/IMAPConnection
                     (connect [_])
                     (monitor-folders [_]))
        context {:db database :client connection}]
    (is (= {:result :ok} (app/connect-to-client connection context))  "Basic authentication calls email-client's login method and returns ok")))

(deftest basic-auth-2
  (let [database (reify int/DB (fetch-connection [_ id] {:id id}))
        connection (reify int/IMAPConnection
                     (connect [_])
                     (monitor-folders [_]))
        context {:db database :client connection}]
    (is (= {:result :ok} (app/connect-to-client connection context))  "If no auth-type is defined, fall back on basic auth and return ok")))

(deftest oauth2-auth
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] {:id id}))
        connection (fake-connection-with-config {:auth-type "oauth2" :auth-provider 2})
        context {:db database :client connection}]
    (is (= {:result :redirect, :provider {:id 2}}
           (app/connect-to-client connection context))
        "auth-type 'oauth2' with auth provider but no token data returns a :redirect with the provider")))

(deftest oauth2-auth-2
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] {:access-token "not empty" :refresh-token "not empty"})
                   (fetch-auth-provider [_ id] {:id id}))
        connection (fake-connection-with-config {:auth-type "oauth2" :auth-provider 2})
        context {:db database :client connection}]
    (is (= {:result :ok}
           (app/connect-to-client connection context))
        "auth-type 'oauth2' with auth provider and token data calls client login and returns ok")))

(deftest oauth2-auth-3
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] nil))
        connection (fake-connection-with-config {:auth-type "oauth2" :auth-provider 2})
        context {:db database :client connection}]
    (is (= :error (:result (app/connect-to-client connection context))))
    "auth-type 'oauth2' with no auth provider returns an error"))

(deftest oauth2-auth-4
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] {:access-token "not empty"})
                   (fetch-auth-provider [_ id] {:id id}))
        connection (fake-connection-with-config {:auth-type "oauth2" :auth-provider 2})
        context {:db database :client connection}]
    (is (= {:result :redirect, :provider {:id 2}}
           (app/connect-to-client connection context))
        "auth-type 'oauth2' with auth provider and access token but no refresh token calls client login and returns ok")))

(deftest emails-query-filter-wo-search
  (let [query (atom "")
        database (reify int/DB
                   (fetch-categories [_] {})
                   (fetch-emails [_ _ important-query]
                     (swap! query (fn [_] important-query))
                     {:total 10 :size 1 :page 1}))]
    (app/fetch-emails {:db database} {:filter "enriched-only" :size 1})
    (is (= @query {:where [:and [:<> :metadata.category nil] [:<> :metadata.language nil]], :order-by [[:date :desc]]}))
    (app/fetch-emails {:db database} {:filter "without-category" :size 1})
    (is (= @query {:where [:= :metadata.category nil] :order-by [[:date :desc]]}))
    (app/fetch-emails {:db database} {:size 1})
    (is (= {:order-by [[:date :desc]]} @query))))

(deftest emails-query-search-wo-filter
  (let [query (atom "")
        database (reify int/DB
                   (fetch-categories [_] {})
                   (fetch-emails [_ _ important-query]
                     (swap! query (fn [_] important-query))
                     {:total 10 :size 1 :page 1}))]
    (app/fetch-emails {:db database} {:search-field "subject" :search-text "test text" :size 1})
    (is (= {:where [:or [:like :headers.subject "%test text%"] [:like :bodies.content "%test text%"] [:like :contacts.name "%test text%"] [:like :contacts.address "%test text%"]], :order-by [[:date :desc]]} @query))))

(deftest emails-query-search-filter
  (let [query (atom "")
        database (reify int/DB
                   (fetch-categories [_] {})
                   (fetch-emails [_ _ important-query]
                     (swap! query (fn [_] important-query))
                     {:total 10 :size 1 :page 1}))]
    (app/fetch-emails {:db database} {:filter "enriched-only" :search-field "subject" :search-text "test text" :size 1})
    (is (= {:where [:and [:and [:<> :metadata.category nil] [:<> :metadata.language nil]] [:or [:like :headers.subject "%test text%"] [:like :bodies.content "%test text%"] [:like :contacts.name "%test text%"] [:like :contacts.address "%test text%"]]], :order-by [[:date :desc]]} @query))))

(deftest create-a-category
  (let [db-called (atom false)
        client-called (atom false)
        database (reify int/DB (save-category [_ _] (swap! db-called (fn [_] true))))
        client (reify int/EmailClient
                 (connections [_] {"does not matter" "some-data"})
                 (create-category-directories! [_ _ _] (swap! client-called (fn [_] true))))]
    (app/create-new-category! {:db database :client client} "test")
    (is (= true @db-called))
    (is (= true @client-called)))
  "Creating a new category makes correct database and client calls")

(def db-instance (SqliteDB.))

(def al-instance (BasicAnalyzer.))

(deftest recategorize-new-email
  (let [fake-conn {:id "test-conn-id" :context {:db db-instance :analyzer al-instance}}
        test-email (construct-enriched-email
                    (construct-email {:message-id "test" :date 0 :subject "Test" :in-reply-to nil :mime-type "text/plain"}
                                     [{:message-id "test" :mime-type "text/plain" :charset "fake" :transfer-encoding "fake" :content "Test" :sanitized-content "Test"}]
                                     [{:type :sender :message-id "test" :name "fake" :address "fake" :contact-key "fake"} {:type :receiver :message-id "test" :name "fake" :address "fake" :contact-key "fake"}])
                    nil nil "test-conn-id")]
    (.save-category db-instance "test-cat")
    (.save-category db-instance "another-cat") ;; assuming this one has id 2
    (testing "When there is no such email in the db"
      (app/recategorize-email test-email 1 fake-conn)
      (let [email-in-db (.fetch-email db-instance "test")
            metadata (:metadata email-in-db)]
        (is (= "test-conn-id" (:connection-id metadata)))
        (is (= "test-cat" (:category metadata)))
        (is (= 1.0 (:category-confidence metadata)))))
    (testing "When when email already exists in the db"
      (app/recategorize-email test-email 2 fake-conn)
      (let [email-in-db (.fetch-email db-instance "test")
            metadata (:metadata email-in-db)]
        (is (= "test-conn-id" (:connection-id metadata)))
        (is (= "another-cat" (:category metadata)))
        (is (= 1.0 (:category-confidence metadata)))))))
