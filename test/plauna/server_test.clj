(ns plauna.server-test
  (:require [cheshire.core :refer [parse-string]]
            [plauna.server :as sut]
            [clojure.test :as t]
            [plauna.client :as client]
            [plauna.files :as files]
            [plauna.interfaces :as int]
            [plauna.database :as db]
            [ring.mock.request :as mock]
            [taoensso.telemere :as tel])
  (:import [org.mockito Mockito]
           [org.mockito.stubbing Answer]
           [plauna.interfaces IMAPConnection DB]
           [plauna.database SqliteDB]))

(tel/set-min-level! :error)

(def ^:dynamic *context* {})

(defn setup-clean-db [f]
  (swap! files/plauna-config (fn [_] {:data-folder "tmp/"}))
  (files/check-and-create-database-file)
  (db/create-db)
  (alter-var-root #'db/batch-size (fn [_] 2))
  (binding [*context* {:db (new SqliteDB)}]
    (f))
  (files/delete-database-file))

(t/use-fixtures :each setup-clean-db)

(def api-endpoint "/api")

(defn connections-api
  ([] (str api-endpoint "/admin/connections"))
  ([id] (str api-endpoint "/admin/connections/" id)))

(defn fcmap-api [id] (str (connections-api id) "/categories"))

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

