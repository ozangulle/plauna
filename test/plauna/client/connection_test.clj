(ns plauna.client.connection-test
  (:require [plauna.client.connection :as sut]
            [clojure.test :as t]
            [clojure.core.async :as async]
            [plauna.interfaces :as int])
  (:import [org.eclipse.angus.mail.imap IdleManager IMAPStore]
           [java.util Properties]
           [plauna.interfaces DB]
           [jakarta.mail Store URLName Session Folder Message Flags$Flag AuthenticationFailedException]))

(defn mock-store [connect-fn connected-fn]
  (let [session (Session/getInstance (new Properties))
        url (new URLName "test.com")]
      (proxy [Store] [session url]
        (isConnected [this] (connected-fn))
        (connect [host user secret] (connect-fn host user secret)))))

(defn mock-db [oauth-token-fn]
  (reify DB
    (fetch-oauth-token-data [_ id] (oauth-token-fn id))))

(t/deftest no-auth-type-uses-non-oauth2-login
  (let [called-connect (atom false)]
    (with-redefs [sut/connection-config->store
                  (fn [_]
                    (mock-store
                     (fn [host user secret]
                       (t/is (and (= host "test-host.com") (= user "test-user") (= secret "test-secret")))
                       (reset! called-connect true)) ()))]
      (let [config {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
            context {}
            connection (sut/create-connection config context (async/chan))]
        (.connect connection)
        (t/is (true? @called-connect))))))

(t/deftest auth-type-oauth2
  (let [called-connect (atom false)
        called-refresh-fn (atom false)
        db (mock-db (fn [_] {:access-token "test-access-token"}))]
    (with-redefs [sut/connection-config->store
                  (fn [_]
                    (mock-store
                     (fn [host user secret]
                       (t/is (and (= host "test-host.com") (= user "test-user") (= secret "test-access-token")))
                       (reset! called-connect true)) ()))
                  sut/refresh-access-token (fn [_] (reset! called-refresh-fn true))]
      (let [config {:id "test-id" :auth-type "oauth2" :host "test-host.com" :user "test-user" :secret "test-secret"}
            context {:db db}
            connection (sut/create-connection config context (async/chan))]
        (.connect connection)
        (t/is (true? @called-refresh-fn))
        (t/is (true? @called-connect))))))
