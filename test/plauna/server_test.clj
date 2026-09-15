(ns plauna.server-test
  (:require [cheshire.core :refer [parse-string]]
            [plauna.server :as sut]
            [clojure.test :as t]
            [plauna.client :as client]
            [plauna.files :as files]
            [plauna.interfaces :as int]
            [plauna.database :as db]
            [ring.mock.request :as mock]
            [plauna.client.mock-server :as ms]
            [taoensso.telemere :as tel])
  (:import [org.mockito Mockito]
           [org.mockito.stubbing Answer]
           [plauna.interfaces IMAPConnection DB Analyzer]
           [plauna.database SqliteDB]))

(tel/set-min-level! :error)
(tel/set-ns-filter! {:disallow "com.icegreen.greenmail.*"})

(def ^:dynamic *context* {})

(defn mock-analyzer []
  (proxy [Analyzer] []
    (enrich_email [email] (-> (assoc-in email [:metadata :category] "test")
                              (assoc-in [:metadata :category-id] 1)))))

(defn setup-clean-db [f]
  (swap! files/plauna-config (fn [_] {:data-folder "tmp/"}))
  (files/check-and-create-database-file)
  (db/create-db)
  (alter-var-root #'db/batch-size (fn [_] 2))
  (binding [*context* {:db (new SqliteDB)
                       :analyzer (mock-analyzer)}]
    (f))
  (files/delete-database-file))

(t/use-fixtures :each setup-clean-db)

(def api-endpoint "/api")

(defn connections-api
  ([] (str api-endpoint "/admin/connections"))
  ([id] (str api-endpoint "/admin/connections/" id)))

(defn fcmap-api [id] (str (connections-api id) "/categories"))

(defn controls-api [id] (str (connections-api id) "/controls"))

(t/deftest calling-connections-returns-expected-data
  (let [base-connection-data
        {"imap"
         {"host" "imap.test.com"
          "user" "test-user"
          "secret" "1234"
          "folder" ""
          "security" "ssl"
          "port" ""
          "debug" false
          "check-ssl-certs" true
          "auth-type" "basic"
          "connected" true
          "id" "c4aaaf19-c259-3694-9d50-31ecbdcea869"
          "auth-provider" nil
          "auth-providers" []}
         "folders" ["INBOX" "newsletter" "spam"]
         "categories" [{"id" 1 "name" "news"} {"id" 2 "name" "misc"}]
         "folder-category-map" {}}
        mock-conn (Mockito/mock IMAPConnection)
        update-called (atom false)
        db ^DB  (:db *context*)]
    (-> (Mockito/doReturn true)
        (.when mock-conn)
        (.connected?))
    (-> (Mockito/doAnswer
         (reify Answer
           (answer [_ _](reset! update-called true))))
        (.when mock-conn)
        (.update-config (Mockito/any)))
    (-> (Mockito/doReturn ["INBOX" "newsletter" "spam"])
        (.when mock-conn)
        (.list-folders))
    (int/save-connection db
                         {:host "imap.test.com"
                          :user "test-user"
                          :secret "1234"
                          :folder ""
                          :security "ssl"
                          :port ""
                          :debug false
                          :check-ssl-certs true
                          :auth-type "basic"
                          :connected true
                          :id "c4aaaf19-c259-3694-9d50-31ecbdcea869"
                          :auth-provider nil})
    (int/save-category db "news")
    (int/save-category db "misc")
    (with-redefs [client/get-connection (fn [_] mock-conn)]
      (let [handler (sut/app {:db db})]
        (t/testing "/connections/:id - Happy path"
          (t/is (= base-connection-data
                   (parse-string (:body (handler (mock/request :get (connections-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))))))))

        (t/testing "/connections - Happy path"
          (t/is (= [{"host" "imap.test.com"
                     "user" "test-user"
                     "secret" "1234"
                     "folder" ""
                     "security" "ssl"
                     "port" ""
                     "debug" false
                     "check-ssl-certs" true
                     "auth-type" "basic"
                     "connected" true
                     "id" "c4aaaf19-c259-3694-9d50-31ecbdcea869"
                     "auth-provider" nil}]
                   (parse-string (:body (handler (mock/request :get (connections-api))))))))

        (t/testing "/connections/:id - Not found"
          (t/is (= 404
                   (:status (handler (mock/request :get (connections-api "c4aaaf19-c259-3694-9d50-31ecbdcea666")))))))

        (t/testing "/connections/:id/categories - creates a new folder category map"
          (t/is (= 200
                   (:status (handler
                             (-> (mock/request :post (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                                 (mock/json-body {:folder "newsletter" :category-id 1}))))))
          (t/is (= {"newsletter" {"id" 1
                                  "connection-id" "c4aaaf19-c259-3694-9d50-31ecbdcea869"
                                  "folder" "newsletter"
                                  "category-id" 1}}
                   (get (parse-string (:body (handler (mock/request :get (connections-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))))) "folder-category-map"))))

        (t/testing "/connections/:id/categories - change mapping by using different category-id on folder"
          (t/is (= 200
                   (:status (handler
                             (-> (mock/request :put (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                                 (mock/json-body {:folder "newsletter" :category-id 2 :id 1}))))))
          (t/is (= {"newsletter"
                    {"id" 1
                     "connection-id" "c4aaaf19-c259-3694-9d50-31ecbdcea869"
                     "folder" "newsletter"
                     "category-id" 2}}
                   (get (parse-string (:body (handler (mock/request :get (connections-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))))) "folder-category-map"))))

        (t/testing "/connections/:id/categories - put fails if id is not passed"
          (t/is (= 400
                   (:status (handler
                             (-> (mock/request :put (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                                 (mock/json-body {:folder "newsletter" :category-id 1})))))))

        (t/testing "/connections/:id/categories - put fails if id is not present in the db"
          (t/is (= 404
                   (:status (handler
                             (-> (mock/request :put (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                                 (mock/json-body {:folder "newsletter" :category-id 1 :id 30})))))))

        (t/testing "/connections/:id/categories - put fails if connection with connection-id does not exist"
          (t/is (= 404
                   (:status (handler
                             (-> (mock/request :put (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea866"))
                                 (mock/json-body {:folder "newsletter" :category-id 1 :id 1})))))))

        (t/testing "/connections/:id/categories - put fails if category does not exist"
          (t/is (= 404
                   (:status (handler
                             (-> (mock/request :put (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                                 (mock/json-body {:folder "newsletter" :category-id 77 :id 1})))))))

        (t/testing "/connections/:id/categories - put fails if folder does not exist"
          (t/is (= 404
                   (:status (handler
                             (-> (mock/request :put (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                                 (mock/json-body {:folder "news" :category-id 1 :id 1})))))))

        (t/testing "/connections/:id/categories - put - call update config on a successful config update"
          ;; set to false because other test might have triggered update-config
          (reset! update-called false)
          (t/is (= 200
                   (:status (handler
                             (-> (mock/request :put (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                                 (mock/json-body {:folder "newsletter" :category-id 2 :id 1}))))))
          (t/is (= true @update-called )))

        (t/testing "/connections/:id/categories - do not call update config on an erroneous config update"
          ;; set to false because other test might have triggered update-config
          (reset! update-called false)
          (t/is (= 404
                   (:status (handler
                             (-> (mock/request :put (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                                 (mock/json-body {:folder "news" :category-id 2 :id 1}))))))
          (t/is (= false @update-called )))

        (t/testing "/connections/:id/categories - delete works"
          (t/is (= 200
                   (:status (handler
                             (-> (mock/request :delete (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                                 (mock/json-body {:id 1}))))))
          (t/is (= {}
                   (get (parse-string (:body (handler (mock/request :get (connections-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))))) "folder-category-map"))))

        (t/testing "/connections/:id/categories - post - call update config on a successful config update"
          ;; set to false because other test might have triggered update-config
          (reset! update-called false)
          (t/is (= 200
                   (:status (handler
                             (-> (mock/request :post (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                                 (mock/json-body {:folder "newsletter" :category-id 2 :id 1}))))))
          (t/is (= true @update-called )))

        (t/testing "/connections/:id/categories - post do not call update config on an erroneous config update"
          ;; set to false because other test might have triggered update-config
          (reset! update-called false)
          (t/is (= 404
                   (:status (handler
                             (-> (mock/request :post (fcmap-api "c4aaaf19-c259-3694-9d50-31ecbdcea868"))
                                 (mock/json-body {:folder "news" :category-id 2 :id 1}))))))
          (t/is (= false @update-called )))
        ))))

(t/deftest parse-emails-with-categorization-fail1
  (let [db ^DB  (:db *context*)
        handler (sut/app {:db db})]
    (int/save-connection db {:host "imap.test.com"
                             :user "test-user"
                             :secret "1234"
                             :folder ""
                             :security "ssl"
                             :port ""
                             :debug false
                             :check-ssl-certs true
                             :auth-type "basic"
                             :id "c4aaaf19-c259-3694-9d50-31ecbdcea869"
                             :auth-provider nil
                             :auth-providers []})
    (int/save-category db "test")
    (int/save-folder-category-map db {:category-id 1 :folder "test"})

    (t/testing
     "Server returns the correct response because connection is not initialized"
      ;; FIXME Test edge cases with wrong input
      (t/is (= 404
               (:status (handler
                         (-> (mock/request :post (controls-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                             (mock/json-body {:operation "parse" :parse-settings {:move true :folder "INBOX" :category ""}})))))))))

(t/deftest parse-emails-with-categorization
  (let [db ^DB  (:db *context*)
        handler (sut/app {:db db})]
    (int/save-connection db {:host "localhost"
                             :user "test-user"
                             :secret "secret"
                             :folder ""
                             :security "plain"
                             :port "3143"
                             :debug false
                             :check-ssl-certs true
                             :auth-type "basic"
                             :id "c4aaaf19-c259-3694-9d50-31ecbdcea869"
                             :auth-provider nil
                             :auth-providers []})
    (int/save-category db "test")
    (int/save-folder-category-map db {:category-id 1 :folder "test" :connection-id "c4aaaf19-c259-3694-9d50-31ecbdcea869"})
    (ms/start-server)
    (ms/create-folder "test")
    (ms/send-email-to-inbox)
    (client/start-imap-connections *context*)

    (t/testing
        "Server returns the correct success response"
      (t/is (= 200
               (:status (handler
                         (-> (mock/request :post (controls-api "c4aaaf19-c259-3694-9d50-31ecbdcea869"))
                             (mock/json-body {:operation "parse" :parse-settings {:move true :folder "INBOX" :category ""}})))))))
    (Thread/sleep 500)
    (t/testing
        "Email is actually moved"
      (let [connection (client/get-connection "c4aaaf19-c259-3694-9d50-31ecbdcea869")]
        (t/is (= 0 (:message-count (.no-of-messages-in-folder connection "INBOX"))))
        (t/is (= 1 (:message-count (.no-of-messages-in-folder connection "test"))))
        (.disconnect-and-stop-monitoring connection)))
    (ms/stop-server)))
