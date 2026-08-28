(ns plauna.server-test
  (:require [cheshire.core :refer [parse-string]]
            [plauna.server :as sut]
            [clojure.test :as t]
            [plauna.client :as client]
            [plauna.database :as db]
            [ring.mock.request :as mock])
  (:import [org.mockito Mockito]
           [plauna.interfaces IMAPConnection DB]))

(defn mock-db []
  (proxy [DB][]
      (fetch_connection [id] {:host "imap.test.com"
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
      (fetch_auth_providers [] [])
      (fetch_categories [] [{:id 1 :name "news"} {:id 2 :name "misc"}])
      (fetch_folder_category_maps [id] {:connection-id "c4aaaf19-c259-3694-9d50-31ecbdcea869" :folder "newsletter" :category-id 1})))

(t/deftest calling-connections-returns-expected-data
  (let [mock-conn (Mockito/mock IMAPConnection)]
    (-> (Mockito/doReturn true)
        (.when mock-conn)
        (.connected?))
    (-> (Mockito/doReturn ["INBOX" "newsletter" "spam"])
        (.when mock-conn)
        (.list-folders))
    (with-redefs [client/get-connection (fn [_] mock-conn)]
      (let [handler (sut/app {:db (mock-db)})]
        (t/is (= {"imap"
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
                  "folder-category-map" {"connection-id" "c4aaaf19-c259-3694-9d50-31ecbdcea869"
                                         "folder" "newsletter"
                                         "category-id" 1}}
                 (parse-string (:body (handler (mock/request :get "/api/admin/connections/c4aaaf19-c259-3694-9d50-31ecbdcea869"))))))))))



