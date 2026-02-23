(ns plauna.application-test
  (:require [clojure.test :refer :all]
            [plauna.interfaces :as int]
            [plauna.application :as app]))

(deftest basic-auth
  (let [database (reify int/DB (fetch-connection [_ id] {:id id :auth-type "basic"}))
        client (reify int/EmailClient (start-monitor [_ config]))
        context {:db database :client client}]
    (is (= {:result :ok} (app/connect-to-client context "abc"))  "Basic authentication calls email-client's login method and returns ok")))

(deftest basic-auth-2
  (let [database (reify int/DB (fetch-connection [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config]))
        context {:db database :client client}]
    (is (= {:result :ok} (app/connect-to-client context "abc"))  "If no auth-type is defined, fall back on basic auth and return ok")))

(deftest oauth2-auth
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config]))
        context {:db database :client client}]
    (is (= {:result :redirect, :provider {:id 2}}
           (app/connect-to-client context "abc"))
        "auth-type 'oauth2' with auth provider but no token data returns a :redirect with the provider")))

(deftest oauth2-auth-2
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] {:access-token "not empty" :refresh-token "not empty"})
                   (fetch-auth-provider [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config]))
        context {:db database :client client}]
    (is (= {:result :ok}
           (app/connect-to-client context "abc"))
        "auth-type 'oauth2' with auth provider and token data calls client login and returns ok")))

(deftest oauth2-auth-3
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] nil))
        client (reify int/EmailClient (start-monitor [_ config]))
        context {:db database :client client}]
    (is (= :error (:result (app/connect-to-client context "abc"))))
    "auth-type 'oauth2' with no auth provider returns an errorq"))

(deftest oauth2-auth-4
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] {:access-token "not empty"})
                   (fetch-auth-provider [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config]))
        context {:db database :client client}]
    (is (= {:result :redirect, :provider {:id 2}}
           (app/connect-to-client context "abc"))
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
    (is (= {:where [:like :headers.subject "%test text%"] :order-by [[:date :desc]]} @query))))

(deftest emails-query-search-filter
  (let [query (atom "")
        database (reify int/DB
                   (fetch-categories [_] {})
                   (fetch-emails [_ _ important-query]
                     (swap! query (fn [_] important-query))
                     {:total 10 :size 1 :page 1}))]
    (app/fetch-emails {:db database} {:filter "enriched-only" :search-field "subject" :search-text "test text" :size 1})
    (is (= {:where [:and [:and [:<> :metadata.category nil] [:<> :metadata.language nil]] [:like :headers.subject "%test text%"]] :order-by [[:date :desc]]} @query))))

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

(deftest move-email-without-connections
  (let [client (reify int/EmailClient
                 (connections [_] {})
                 (connection-id-for-email [_ _ _] nil))
        test-result (app/move-email-to-category {} "test" {:client client})]
    (is (= :error (:result test-result))))
  "If there are no connections, just moving email returns an error result.")

(deftest move-email-with-guessed-connection-id-success
  (let [client (reify int/EmailClient
                 (connections [_] {"test" {}})
                 (connection-id-for-email [_ _ _] "test")
                 (move-email-between-categories [_ _ _ _ _] true))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :ok (:result test-result))))
  "Successfully moving an email with a guessed connection id returns result :ok")

(deftest move-email-with-guessed-connection-id-error
  (let [client (reify int/EmailClient
                 (connections [_] {"test" {}})
                 (connection-id-for-email [_ _ _] "test")
                 (move-email-between-categories [_ _ _ _ _] false))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :error (:result test-result))))
  "Unsuccessfully moving an email with a guessed connection id returns result :error")

(deftest move-email-without-guessed-connection-id-success
  (let [client (reify int/EmailClient
                 (connections [_] {"test1" {:config {:id "test1"}} "test2" {:config {:id "test2"}}})
                 (connection-id-for-email [_ _ _] nil)
                 (move-email-between-categories [_ id _ _ _] (= id "test2")))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :ok (:result test-result))))
  "Successfully moving an email without guessed connection id returns result :ok even if the process failed in some other connection.")

(deftest move-email-without-guessed-connection-id-error
  (let [client (reify int/EmailClient
                 (connections [_] {"test1" {:config {:id "test1"}} "test2" {:config {:id "test2"}}})
                 (connection-id-for-email [_ _ _] nil)
                 (move-email-between-categories [_ id _ _ _] (= id "test3")))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :error (:result test-result))))
  "Unsuccessfully moving an email without guessed connection id returns result :error")
