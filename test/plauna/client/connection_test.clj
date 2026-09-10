(ns plauna.client.connection-test
  (:require [plauna.client.connection :as sut]
            [plauna.client.mock-server :as ms]
            [clojure.test :as t]
            [clojure.core.async :as async]
            [plauna.interfaces :as int]
            [plauna.files :as files]
            [plauna.database :as db]
            [plauna.application :as app]
            [taoensso.telemere :as tel])
  (:import [org.eclipse.angus.mail.imap IdleManager IMAPStore IMAPFolder]
           [java.util Properties]
           [plauna.interfaces DB]
           [org.mockito Mockito]
           [org.mockito.stubbing Answer]
           [plauna.database SqliteDB]
           [jakarta.mail Store URLName Session Folder Message Flags$Flag AuthenticationFailedException]))

(tel/set-min-level! :error)

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

(t/deftest no-auth-type-uses-non-oauth2-login
  (ms/start-server)
  (let [config {:imap {:id "test-id" :host "localhost" :user "test-user" :secret "secret" :port "3143" :security "plain"}}
        context {}
        connection (sut/create-connection config context)]
    (.connect connection)
    (t/is (true? (.connected? connection)))
    (ms/stop-server)))

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
      (let [config {:imap {:id "test-id" :auth-type "oauth2" :host "test-host.com" :user "test-user" :secret "test-secret"}}
            context {:db db}
            connection (sut/create-connection config context)]
        (.connect connection)
        (t/is (true? @called-refresh-fn))
        (t/is (true? @called-connect))))))

(t/testing "Disconnection Tests"
  (t/deftest test-disconnect
    (ms/start-server)
    (let [config {:imap {:id "test-id" :host "localhost" :user "test-user" :secret "secret" :port "3143" :security "plain"}}
          context {}
          connection (sut/create-connection config context)]
      (.connect connection)
      (t/is (true? (.connected? connection)))
      (.disconnect-and-stop-monitoring connection)
      (t/is (false? (.connected? connection))))
    (ms/stop-server))

  (t/deftest test-disconnect-without-connection
    (ms/start-server)
    (let [config {:imap {:id "test-id" :host "localhost" :user "test-user" :secret "secret" :port "3143" :security "plain"}}
          context {}
          connection (sut/create-connection config context)]
      (.disconnect-and-stop-monitoring connection)
      (t/is (false? (.connected? connection))))
    (ms/stop-server)))

(t/testing "Monitoring when not logged in returns false"
  (t/deftest monitor-without-connection
    (ms/start-server)
    (let [config {:imap {:id "test-id" :host "localhost" :user "test-user" :secret "secret" :port "3143" :security "plain"}}
          context {}
          connection (sut/create-connection config context)]
      (t/is (false? (.monitor-folders connection))))
    (ms/stop-server)))

(t/testing "Monitoring when logged in returns true"
  (t/deftest monitor-without-connection
    (ms/start-server)
    (let [config {:imap {:id "test-id" :host "localhost" :user "test-user" :secret "secret" :port "3143" :security "plain"}}
          context {}
          connection (sut/create-connection config context)]
      (.connect connection)
      (t/is (true? (.monitor-folders connection))))
    (ms/stop-server)))

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
        (let [config {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}}
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
                             {:connected-fn (fn [] (swap! connected-calls inc) (not (= @connected-calls 2)))
                              :disconnect-fn (fn [] (swap! disconnected-calls inc) true)
                              :get-folder-fn (fn [_] folder)}))]
        (let [config {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}}
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
                              :connected-fn (fn [] (swap! connected-calls inc) (not (= @connected-calls 2)))
                              :disconnect-fn (fn [] (swap! disconnected-calls inc) true)
                              :get-folder-fn (fn [_] folder)}))]
        (let [config {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}}
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
        (let [config {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}}
              context {}
              connection (sut/create-connection config context)]
          (.connect connection)
          (.monitor-folders connection)
          (Thread/sleep 70)
          (t/is (= 2 @folder-open-calls))
          (t/is (= 1 @get-folder-called))
          (.disconnect-and-stop-monitoring connection))))))

(t/testing "watch-folder throwing an exception even when folder .isOpen is true during health checks causes an attempt to reopen the folder"
  (t/deftest close-connection-during-health-check-reopens
    (let [watch-folder-called (atom 0)
          restart-monitoring-called (atom 0)
          folder (Mockito/mock IMAPFolder)
          idle-manager (Mockito/mock IdleManager)]
      (-> (Mockito/doNothing)
          (.when folder)
          (.addMessageCountListener (Mockito/any)))
      (-> (Mockito/doAnswer
           (reify Answer
             (answer [_ _] true)))
          (.when folder)
          (.isOpen))
      (-> (Mockito/doAnswer
           (reify Answer
             (answer [_ _] (swap! watch-folder-called inc) (if (not (= 2 @watch-folder-called)) true (throw (ex-info "Nasty Runtime Exception" {}))))))
          (.when idle-manager)
          (.watch (Mockito/any)))
      (with-redefs [sut/create-idle-manager (fn [_] idle-manager)
                    sut/restart-monitoring (fn [_] (swap! restart-monitoring-called inc))
                    sut/health-check-interval 50
                    sut/connection-config->store
                    (fn [_] (mock-store
                             {:connect-fn (fn [_ _ _] true)
                              :connected-fn (fn [] true)
                              :disconnect-fn (fn [] true true)
                              :get-folder-fn (fn [_]  folder)}))]
        (let [config {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}}
              context {}
              connection (sut/create-connection config context)]
          (.connect connection)
          (.monitor-folders connection)
          (Thread/sleep 70)
          (t/is (= 1 @restart-monitoring-called))
          (.disconnect-and-stop-monitoring connection))))))

(t/deftest fcmap->folder-configuration-correct
  (let [idle-manager (Mockito/mock IdleManager)]
    (with-redefs [sut/create-idle-manager (fn [_] idle-manager)
                  sut/connection-config->store
                  (fn [_] (mock-store
                           {:connect-fn (fn [_ _ _] true)
                            :connected-fn (fn [] true)
                            :disconnect-fn (fn [] true)}))]
      (let [config {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
                    :categories []
                    :folder-category-map {}}
            context {}
            connection (sut/create-connection config context)]
        (t/testing "Inbox is always attached even if fcmap is empty"
          (t/is (= 1 (count @(:folders connection))))
          (t/is (= :inbox (:type (first @(:folders connection))))))))))

(t/deftest fcmap->folder-configuration-correct-2
  (let [idle-manager (Mockito/mock IdleManager)]
    (with-redefs [sut/create-idle-manager (fn [_] idle-manager)
                  sut/connection-config->store
                  (fn [_] (mock-store
                           {:connect-fn (fn [_ _ _] true)
                            :connected-fn (fn [] true)
                            :disconnect-fn (fn [] true)}))]
      (let [config {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
                    :categories [{:id 1 :name "news"}]
                    :folder-category-map {"Newsletters" {:id 1 :folder "Newsletters" :category-id 1}}}
            context {}
            connection (sut/create-connection config context)]
        (t/testing "fcmap is converted correctly to FolderConfig"
          (t/is (= 2 (count @(:folders connection))))
          (t/is (= "Newsletters" (:name (first (filterv #(not (= :inbox (:type %))) @(:folders connection))))))
          (t/is (= 1 (:category (first (filterv #(not (= :inbox (:type %))) @(:folders connection)))))))))))

(t/deftest update-config-test
  (let [idle-manager (Mockito/mock IdleManager)]
    (with-redefs [sut/create-idle-manager (fn [_] idle-manager)
                  sut/connection-config->store
                  (fn [_] (mock-store
                           {:connect-fn (fn [_ _ _] true)
                            :connected-fn (fn [] true)
                            :disconnect-fn (fn [] true)}))]
      (let [config {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
                    :categories [{:id 1 :name "news"}]
                    :folder-category-map {"Newsletters" {:id 1 :folder "Newsletters" :category-id 1}}}
            config-to-update {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "new-secret"}
                         :categories [{:id 1 :name "news"}]
                         :folder-category-map {"Newsletters" {:id 1 :folder "Newsletters" :category-id 2}
                                               "Some Other" {:id 2 :folder "Some Other" :category-id 1}}}
            context {}
            connection (sut/create-connection config context)]
        (t/testing "Update config works"
          (t/is (= config (.config connection)))
          (.update-config connection config-to-update)
          (t/is (= config-to-update (.config connection))))))))

;; FIXME this tests the internals of IMAPConnection
(t/deftest update-folder-configs-test
  (let [idle-manager (Mockito/mock IdleManager)]
    (with-redefs [sut/create-idle-manager (fn [_] idle-manager)
                  sut/connection-config->store
                  (fn [_] (mock-store
                           {:connect-fn (fn [_ _ _] true)
                            :connected-fn (fn [] true)
                            :disconnect-fn (fn [] true)}))]
      (let [config {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
                    :categories [{:id 1 :name "news"}]
                    :folder-category-map {"Newsletters" {:id 1 :folder "Newsletters" :category-id 1}}}
            config-to-update {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "new-secret"}
                              :categories [{:id 1 :name "news"}]
                              :folder-category-map {"Newsletters" {:id 1 :folder "Newsletters" :category-id 2}
                                                    "Some Other" {:id 2 :folder "Some Other" :category-id 1}}}
            context {}
            connection (sut/create-connection config context)]
        (t/testing "Update config also update the correct folder configs"
          (t/is (= 2 (count @(:folders connection))))
          (.update-config connection config-to-update)
          (t/is (= 3 (count @(:folders connection)))))))))

(comment
  ;; TODO find a way to make this test work
  (t/deftest correct-number-of-folder-listener-pairs
    (let [ listeners (atom [])
          folder (Mockito/mock IMAPFolder)
          message-count-event
          (proxy [jakarta.mail.event.MessageCountEvent] [folder 0 false []]
            (getMessages [] []))
          idle-manager (Mockito/mock IdleManager)]
      (-> (Mockito/doAnswer
           (reify Answer
             (answer [_ invocation]
               (let [adapter (aget (.getArguments invocation) 0)]
                 (swap! listeners conj adapter)
                 nil))))
          (.when folder)
          (.addMessageCountListener (Mockito/any)))
      (-> (Mockito/doReturn true)
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
                              :connected-fn (fn [] true)
                              :get-folder-fn (fn [_] folder)}))]
        (let [config {:imap {:id "test-id" :host "test-host.com" :user "test-user" :secret "test-secret"}
                      :categories [{:id 1 :name "news"}]
                      :folder-category-map {"Newsletters "{:id 1 :folder "Newsletters" :category-id 1}}}
              context {}
              connection (sut/create-connection config context)]
          (.connect connection)
          (.monitor-folders connection)
          (println (first @listeners))
          (.messagesAdded (first @listeners) message-count-event)
          (t/is (= 2 (count (:folder-listener-pairs (deref(:state connection))))))
          (.disconnect-and-stop-monitoring connection))))))

(t/testing "Inbox to Category Folder move"
  (t/deftest inbox-receive
    (let [called-recategorize-email (atom 0)]
      (ms/start-server)
      (ms/create-folder "test")
      (with-redefs [app/handle-incoming-imap-email (fn [_ _] {:category-id 1 :category "test" :result :ok})
                    app/recategorize-email (fn [_ _ _] (swap! called-recategorize-email inc))]
        (let [config {:imap {:id "test-id" :host "localhost" :user "test-user" :secret "secret" :port "3143" :security "plain"}
                      :categories [{:id 1 :name "test"}]
                      :folder-category-map {"test" {:id 1 :folder "test" :category-id 1}}}
              db ^DB  (:db *context*)
              connection (sut/create-connection config *context*)]
          (int/save-category db "test")
          (.connect connection)
          (.monitor-folders connection)
          (ms/send-email-to-folder "INBOX")
          (Thread/sleep 400)
          (t/is (some? (.nth-message-in-folder connection "test" 1)))
          (t/is (= 0 @called-recategorize-email))
          (.disconnect-and-stop-monitoring connection))))
    (ms/stop-server)))
