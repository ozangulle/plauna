(ns plauna.client.connection-test
  (:require [plauna.client.connection :as sut]
            [clojure.test :as t]
            [clojure.core.async :as async]
            [plauna.interfaces :as int])
  (:import [org.eclipse.angus.mail.imap IdleManager IMAPStore IMAPFolder]
           [java.util Properties]
           [plauna.interfaces DB]
           [org.mockito Mockito]
           [org.mockito.stubbing Answer]
           [jakarta.mail Store URLName Session Folder Message Flags$Flag AuthenticationFailedException]))

(defn mock-store [function-map]
  (let [session (Session/getInstance (new Properties))
        url (new URLName "test.com")]
    (proxy [Store] [session url]
      (isConnected [] ((function-map :connected-fn)))
      (connect [host user secret] ((function-map :connect-fn) host user secret))
      (close [] ((function-map :disconnect-fn)))
      (removeConnectionListener [] ((function-map :rcl-fn)))
      (getFolder [folder-name] ((function-map :get-folder-fn) folder-name)))))

(defn mock-db [oauth-token-fn]
  (reify DB
    (fetch-oauth-token-data [_ id] (oauth-token-fn id))))

(t/testing "Authentication Tests"
  (t/deftest no-auth-type-uses-non-oauth2-login
    (let [called-connect (atom false)]
      (with-redefs [sut/connection-config->store
                    (fn [_]
                      (mock-store
                       {:connect-fn
                        (fn [host user secret]
                          (t/is (and (= host "test-host.com") (= user "test-user") (= secret "test-secret")))
                          (reset! called-connect true))}))]
        (let [config {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
              context {}
              connection (sut/create-connection config context)]
          (.connect connection)
          (t/is (true? @called-connect))))))

  (t/deftest auth-type-oauth2
    (let [called-connect (atom false)
          called-refresh-fn (atom false)
          db (mock-db (fn [_] {:access-token "test-access-token"}))]
      (with-redefs [sut/connection-config->store
                    (fn [_]
                      (mock-store
                       {:connect-fn
                        (fn [host user secret]
                          (t/is (and (= host "test-host.com") (= user "test-user") (= secret "test-access-token")))
                          (reset! called-connect true))}))
                    sut/refresh-access-token (fn [_] (reset! called-refresh-fn true))]
        (let [config {:id "test-id" :auth-type "oauth2" :host "test-host.com" :user "test-user" :secret "test-secret"}
              context {:db db}
              connection (sut/create-connection config context)]
          (.connect connection)
          (t/is (true? @called-refresh-fn))
          (t/is (true? @called-connect)))))))

(t/testing "Disconnection Tests"
  (t/deftest test-disconnect
    (let [called-disconnect (atom false)
          called-connected (atom false)
          folder (Mockito/mock IMAPFolder)
          idle-manager (Mockito/mock IdleManager)
          remove-listener-calls (atom 0)]
      (-> (Mockito/doNothing)
          (.when folder)
          (.addMessageCountListener (Mockito/any)))
      (-> (Mockito/doAnswer
           (reify Answer
             (answer [_ _] (swap! remove-listener-calls inc) true)))
          (.when folder)
          (.removeFolderListener (Mockito/any)))
      (-> (Mockito/doNothing)
          (.when idle-manager)
          (.watch (Mockito/any)))
      (with-redefs [sut/create-idle-manager (fn [_] idle-manager)
                    sut/connection-config->store
                    (fn [_] (mock-store
                             {:disconnect-fn (fn [] (reset! called-disconnect true))
                              :connected-fn (fn [] (reset! called-connected true) true)
                              :get-folder-fn (fn [_] folder)}))]
        (let [config {:id "test-id" :auth-type "oauth2" :host "test-host.com" :user "test-user" :secret "test-secret"}
              context {}
              connection (sut/create-connection config context)]
          (.connect connection)
          (.monitor-folders connection)
          (.disconnect-and-stop-monitoring connection)
          (t/is (true? @called-connected))
          (t/is (true? @called-disconnect))
          (t/is (= 1 @remove-listener-calls))))))

  (t/deftest test-disconnect-without-connection
    (let [called-disconnect (atom false)
          called-connected (atom true)]
      (with-redefs [sut/connection-config->store
                    (fn [_] (mock-store
                             {:disconnect-fn (fn [_] (reset! called-disconnect true))
                              :connected-fn (fn [] (reset! called-connected true) false)}))]
        (let [config {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
              context {}
              connection (sut/create-connection config context)]
          (.disconnect-and-stop-monitoring connection)
          (t/is (true? @called-connected))
          (t/is (false? @called-disconnect)))))))

(t/testing "Monitoring when not logged in returns false"
  (t/deftest monitor-without-connection
    (let [folder (Mockito/mock IMAPFolder)
          idle-manager (Mockito/mock IdleManager)]
      (-> (Mockito/doNothing)
          (.when folder)
          (.addMessageCountListener (Mockito/any)))
      (-> (Mockito/doNothing)
          (.when idle-manager)
          (.watch (Mockito/any)))
      (with-redefs [sut/create-idle-manager (fn [_] idle-manager)
                    sut/connection-config->store
                    (fn [_] (mock-store
                             {:connected-fn (fn [] false)
                              :get-folder-fn (fn [_] folder)}))]
        (let [config {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
              context {}
              connection (sut/create-connection config context)]
          (.connect connection)
          (t/is (false? (.monitor-folders connection))))))))

(t/testing "Monitoring when logged in returns true"
  (t/deftest monitor-without-connection
    (let [folder (Mockito/mock IMAPFolder)
          idle-manager (Mockito/mock IdleManager)]
      (-> (Mockito/doNothing)
          (.when folder)
          (.addMessageCountListener (Mockito/any)))
      (-> (Mockito/doNothing)
          (.when idle-manager)
          (.watch (Mockito/any)))
      (with-redefs [sut/create-idle-manager (fn [_] idle-manager)
                    sut/connection-config->store
                    (fn [_] (mock-store
                             {:connected-fn (fn [] true)
                              :get-folder-fn (fn [_] folder)}))]
        (let [config {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
              context {}
              connection (sut/create-connection config context)]
          (.connect connection)
          (t/is (true? (.monitor-folders connection))))))))

(t/testing "Monitoring when logged in calls health-check-funtion"
  (t/deftest monitor-without-connection
    (let [connected-calls (atom 0)
          folder-open-calls (atom 0)
          folder (Mockito/mock IMAPFolder)
          idle-manager (Mockito/mock IdleManager)]
      (-> (Mockito/doNothing)
          (.when folder)
          (.addMessageCountListener (Mockito/any)))
      (-> (Mockito/doAnswer
             (reify Answer
               (answer [_ _] (swap! folder-open-calls inc) true)))
        (.when folder)
        (.isOpen))
      (-> (Mockito/doNothing)
          (.when idle-manager)
          (.watch (Mockito/any)))
      (with-redefs [sut/create-idle-manager (fn [_] idle-manager)
                    sut/health-check-interval 50
                    sut/connection-config->store
                    (fn [_] (mock-store
                             {:disconnect-fn (fn [])
                              :connected-fn (fn [] (swap! connected-calls inc) true)
                              :get-folder-fn (fn [_] folder)}))]
        (let [config {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
              context {}
              connection (sut/create-connection config context)]
          (.connect connection)
          (.monitor-folders connection)
          (Thread/sleep 300)
          (t/is (< 3 @connected-calls))
          (t/is (< 3 @folder-open-calls))
          (.disconnect-and-stop-monitoring connection))))))

(t/testing "Health checks stop when store is disconnected"
  (t/deftest health-checks-stop-when-disconnected
    (let [connected-calls (atom 0)
          disconnected-calls (atom 0)
          health-check-calls (atom 0)
          folder-open-calls (atom 0)
          folder (Mockito/mock IMAPFolder)
          idle-manager (Mockito/mock IdleManager)
          health-check-fn sut/health-check-imap-folder-pairs]
      (-> (Mockito/doNothing)
          (.when folder)
          (.addMessageCountListener (Mockito/any)))
      (-> (Mockito/doAnswer
           (reify Answer
             (answer [_ _] (swap! folder-open-calls inc) true)))
          (.when folder)
          (.isOpen))
      (-> (Mockito/doNothing)
          (.when idle-manager)
          (.watch (Mockito/any)))
      (with-redefs [sut/health-check-imap-folder-pairs (fn [connection] (health-check-fn connection) (swap! health-check-calls inc))
                    sut/create-idle-manager (fn [_] idle-manager)
                    sut/health-check-interval 50
                    sut/connection-config->store
                    (fn [_] (mock-store
                             {:connected-fn (fn [] (swap! connected-calls inc) (< @connected-calls 2))
                              :disconnect-fn (fn [] (swap! disconnected-calls inc) true)
                              :get-folder-fn (fn [_] folder)}))]
        (let [config {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
              context {}
              connection (sut/create-connection config context)]
          (.connect connection)
          (.monitor-folders connection)
          (Thread/sleep 70)
          (t/is (= 1 @disconnected-calls))
          (t/is (= 1 @health-check-calls))
          (.disconnect-and-stop-monitoring connection))))))

(t/testing "Close connection during health checks causes a reconnection"
  (t/deftest close-connection-during-health-check-reconnects
    (let [connected-calls (atom 0)
          connect-calls (atom 0)
          disconnected-calls (atom 0)
          health-check-calls (atom 0)
          folder (Mockito/mock IMAPFolder)
          idle-manager (Mockito/mock IdleManager)
          health-check-fn sut/health-check-imap-folder-pairs]
      (-> (Mockito/doNothing)
          (.when folder)
          (.addMessageCountListener (Mockito/any)))
      (-> (Mockito/doAnswer
           (reify Answer
             (answer [_ _] true)))
          (.when folder)
          (.isOpen))
      (-> (Mockito/doNothing)
          (.when idle-manager)
          (.watch (Mockito/any)))
      (with-redefs [sut/health-check-imap-folder-pairs (fn [connection] (health-check-fn connection) (swap! health-check-calls inc))
                    sut/create-idle-manager (fn [_] idle-manager)
                    sut/health-check-interval 50
                    sut/connection-config->store
                    (fn [_] (mock-store
                             {:connect-fn (fn [_ _ _] (swap! connect-calls inc) true)
                              :connected-fn (fn [] (swap! connected-calls inc) (< @connected-calls 2))
                              :disconnect-fn (fn [] (swap! disconnected-calls inc) true)
                              :get-folder-fn (fn [_] folder)}))]
        (let [config {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
              context {}
              connection (sut/create-connection config context)]
          (.connect connection)
          (.monitor-folders connection)
          (Thread/sleep 70)
          (t/is (= 2 @connect-calls))
          (t/is (= 1 @disconnected-calls))
          (t/is (= 1 @health-check-calls))
          (.disconnect-and-stop-monitoring connection))))))

(t/testing "Close folder during health checks causes an attempt to reopen the folder"
  (t/deftest close-connection-during-health-check-reopens
    (let [folder-open-calls (atom 0)
          get-folder-called (atom 0)
          folder (Mockito/mock IMAPFolder)
          idle-manager (Mockito/mock IdleManager)]
      (-> (Mockito/doNothing)
          (.when folder)
          (.addMessageCountListener (Mockito/any)))
      (-> (Mockito/doAnswer
           (reify Answer
             (answer [_ _] (swap! folder-open-calls inc) (< @folder-open-calls 2))))
          (.when folder)
          (.isOpen))
      (-> (Mockito/doNothing)
          (.when idle-manager)
          (.watch (Mockito/any)))
      (with-redefs [sut/create-idle-manager (fn [_] idle-manager)
                    sut/health-check-interval 50
                    sut/connection-config->store
                    (fn [_] (mock-store
                             {:connect-fn (fn [_ _ _] true)
                              :connected-fn (fn [] true)
                              :disconnect-fn (fn [] true true)
                              :get-folder-fn (fn [_] (swap! get-folder-called inc) folder)}))]
        (let [config {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
              context {}
              connection (sut/create-connection config context)]
          (.connect connection)
          (.monitor-folders connection)
          (Thread/sleep 70)
          (t/is (= 2 @get-folder-called))
          (.disconnect-and-stop-monitoring connection))))))
