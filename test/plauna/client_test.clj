(ns plauna.client-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async]
            [plauna.client :as client :refer [->ConnectionData]]
            [plauna.core.email :as core-email :refer [->Email ->Participant]]
            [taoensso.telemere :as t]
            [plauna.core.events :as events])
  (:import (java.util Properties)
           (jakarta.mail Session)))

(t/set-min-level! :error)

(deftest ssl-properties-set-correctly
  (let [session ^Session (client/config->session {:security "ssl" :port 993})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.ssl.enable", "true")
                              (.setProperty "mail.imap.port", "993")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (is (= expected-properties (.getProperties session)))))

(deftest starttls-properties-set-correctly
  (let [session ^Session (client/config->session {:security "starttls" :port 143})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.starttls.enable", "true")
                              (.setProperty "mail.imap.port", "143")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (is (= expected-properties (.getProperties session)))))

(deftest plain-text-properties-set-correctly
  (let [session ^Session (client/config->session {:security "plain" :port 143})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.port", "143")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (is (= expected-properties (.getProperties session)))))

(deftest empty-values-return-ssl
  (let [session ^Session (client/config->session {})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.ssl.enable", "true")
                              (.setProperty "mail.imap.port", "993")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (is (= expected-properties (.getProperties session)))))

(deftest non-compliant-security-values-return-ssl
  (let [session ^Session (client/config->session {:security "does-not-exist"})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.ssl.enable", "true")
                              (.setProperty "mail.imap.port" "993")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (is (= expected-properties (.getProperties session)))))

(deftest debug-false-on-default
  (let [session ^Session (client/config->session {})]
    (is (= false (.getDebug session)))))

(deftest debug-can-be-set-true
  (let [session ^Session (client/config->session {:debug true})]
    (is (= true (.getDebug session)))))

(deftest set-cert-checks-to-false
  (let [session ^Session (client/config->session {:security "ssl" :check-ssl-certs false})]
    (is (= "*" (.getProperty session "mail.imap.ssl.trust")))))

(deftest id-creating
  (let [config {:host "imap.testmail.com" :user "test@testmail.com" :secret "12345" :folder "Inbox" :debug false}
        same-config-different-secret-and-debug {:host "imap.testmail.com" :user "test@testmail.com" :secret "67890" :folder "Inbox" :debug true}
        config-but-different-folder {:host "imap.testmail.com" :user "test@testmail.com" :secret "12345" :folder "Otherbox" :debug false}
        expected-uuid "2d9f0ab8-00f2-3600-b6b9-24054e1e3337"]
    (is (= expected-uuid (client/id-from-config config)))
    (is (= expected-uuid (client/id-from-config same-config-different-secret-and-debug)))
    (is (not (= expected-uuid (client/id-from-config config-but-different-folder))))))

(deftest adding-to-connections
  (let [test-config {:id "test-id" :host "imap.testmail.com" :user "test@testmail.com" :secret "12345" :folder "Inbox" :debug false :security "starttls"}
        test-con-data (client/->ConnectionData test-config nil nil nil nil nil)]
    (client/add-to-connections test-con-data)
    (is (= (get @client/connections "test-id") test-con-data))))

(deftest connection-id-from-email-success-single-connection
  (let [email (->Email nil nil
                       [(->Participant "test@test.com" nil nil :receiver nil)
                        (->Participant "nope@test.com" nil nil :sender nil)])
        connection-data (->ConnectionData {:user "test@test.com" :id "correct-id"} nil nil nil nil nil)]
    (is (= "correct-id" (client/connection-id-for-email [connection-data] email)))))

(deftest connection-id-from-email-success-multiple-connections
  (let [email (->Email nil nil
                       [(->Participant "test@test.com" nil nil :receiver nil)
                        (->Participant "nope@test.com" nil nil :sender nil)])
        connection-data (->ConnectionData {:user "test@test.com" :id "correct-id"} nil nil nil nil nil)
        connection-data2 (->ConnectionData {:user "nope@test.com" :id "wrong-id"} nil nil nil nil nil)]
    (is (= "correct-id" (client/connection-id-for-email [connection-data connection-data2] email)))))

(deftest connection-id-from-email-fail
  (let [email (->Email nil nil
                       [(->Participant "test@test.com" nil nil :receiver nil)
                        (->Participant "nope@test.com" nil nil :sender nil)])
        connection-data (->ConnectionData {:user "test2@test.com" :id "some-id"} nil nil nil nil nil)]
    (is (nil? (client/connection-id-for-email {"correct-id" connection-data} email)))))

(deftest folder-monitor-tests
  (let [null (client/monitor-folder-name nil)
        empty (client/monitor-folder-name "")
        valid (client/monitor-folder-name "MyInbox")]
    (is (= null "INBOX"))
    (is (= empty "INBOX"))
    (is (= valid "MyInbox"))))

(deftest create-category-does-nothing-when-not-connected
  (let [create-folder-called (atom nil)]
    (with-redefs [client/connected? (fn [_] false)
                  client/create-folders (fn [_ category] (swap! create-folder-called (fn [_] category)))]
      (client/create-category-folders! {:store nil} "my-cat")
      (is (= nil @create-folder-called)))))

(deftest create-category-calls-function-when-connected
  (let [create-folder-called (atom nil)]
    (with-redefs [client/connected? (fn [_] true)
                  client/create-folders (fn [_ category] (swap! create-folder-called (fn [_] category)))]
      (client/create-category-folders! {:store nil} "my-cat")
      (is (= "my-cat" @create-folder-called)))))

(deftest connection-id-for-email-happy-path
  (let [test-email (->Email nil nil
                            [(->Participant "find@me.com" nil nil :receiver nil)
                             (->Participant "nope@test.com" nil nil :sender nil)])
        test-connection1 (->ConnectionData {:id "id1" :user "find@me.com"} nil nil nil nil nil)
        test-connection2 (->ConnectionData {:id "id2" :user "wrong@one.com"} nil nil nil nil nil)
        result (client/connection-id-for-email [test-connection1 test-connection2] test-email)]
    (is (= "id1" result)))
  "Given a vector of ConnectionData, the function finds the correct ConnectionData id.")

(deftest connection-id-for-email-sad-path
  (let [test-email (->Email nil nil
                            [(->Participant "cannotfind@me.com" nil nil :receiver nil)
                             (->Participant "nope@test.com" nil nil :sender nil)])
        test-connection1 (->ConnectionData {:id "id1" :user "find@me.com"} nil nil nil nil nil)
        test-connection2 (->ConnectionData {:id "id2" :user "wrong@one.com"} nil nil nil nil nil)
        result (client/connection-id-for-email [test-connection1 test-connection2] test-email)]
    (is (nil? result)))
  "Given a vector of ConnectionData, the function returns nil if it cannot find the correct id.")

(deftest move-emails-test-no-connection
  (with-redefs [client/connected? (fn [_] false)
                client/connection-data-from-id (fn [_] {})]
    (is (false? (client/move-messages-by-id-between-category-folders "fake" "does-no-exist" "test" "test2" {}))))
  "If the store is not connected, return false immediately.")
