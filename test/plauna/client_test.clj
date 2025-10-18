(ns plauna.client-test
  (:require [clojure.test :refer :all]
            [plauna.client :as client])
  (:import (java.util Properties)
           (jakarta.mail Session)
           (org.mockito Mockito)
           (org.mockito ArgumentMatchers)))

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

(deftest folder-throws-exception-on-move
  (with-redefs [client/reconnect (fn [_] true)
                client/move-messages-by-id-between-category-folders (fn [_ _ _ _] true)]
    (let [test-config {:id "test-id" :host "imap.testmail.com" :user "test@testmail.com" :secret "12345" :folder "Inbox" :debug false :security "starttls"}
          store (Mockito/mock org.eclipse.angus.mail.imap.IMAPStore)
          folder (Mockito/mock org.eclipse.angus.mail.imap.IMAPFolder)
          idle-manager (Mockito/mock org.eclipse.angus.mail.imap.IdleManager)
          message (Mockito/mock org.eclipse.angus.mail.imap.IMAPMessage)
          conn-data (client/->ConnectionData test-config store folder idle-manager [:move] nil)
          mock-event {:payload {:metadata {:category "yes"}}
                      :options {:move true
                                :connection-id "test-id"
                                :folder folder
                                :message message}}]
      (.thenReturn (Mockito/when (.getDefaultFolder store)) (Mockito/mock org.eclipse.angus.mail.imap.IMAPFolder))
      (.thenThrow (Mockito/when (.moveMessages folder (ArgumentMatchers/any) (ArgumentMatchers/any))) jakarta.mail.FolderClosedException) nil
      (client/add-to-connections conn-data)
      (is (= (client/handle-incoming-events mock-event) true)))))
