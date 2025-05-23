(ns plauna.client
  (:require
   [clojure.core.async :as async]
   [plauna.database :as db]
   [plauna.core.events :as events]
   [plauna.preferences :as p]
   [clojure.string :as s]
   [taoensso.telemere :as t]
   [plauna.messaging :as messaging])
  (:import
   (clojure.lang PersistentVector)
   (jakarta.mail Store Session Folder Message Flags$Flag)
   (org.eclipse.angus.mail.imap IMAPFolder IMAPMessage)
   (jakarta.mail.event MessageCountAdapter MessageCountEvent)
   (jakarta.mail.search MessageIDTerm)
   (java.io ByteArrayOutputStream)
   (java.lang AutoCloseable)
   (java.util Properties)
   (java.util.concurrent Executors)
   (org.eclipse.angus.mail.imap IdleManager IMAPStore)
   (java.util.concurrent Executors TimeUnit ScheduledExecutorService)))

; Names
; Config without secret -> identifier
; Watching folder -> FolderMonitor
; Periodic checks: Health checks
; idling -> monitoring

(set! *warn-on-reflection* true)

(defonce executor-service (Executors/newSingleThreadScheduledExecutor))

(defonce idle-manager (atom nil))

(defonce parent-folder-name "Categories")

(defonce connections (atom {}))

(defonce health-checks (atom {}))

;; id -> listener
(defonce message-count-listeners (atom {}))

(defn default-port-for-security [security]
  (if (= security :ssl) 993 143))

(defn security [connection-config]
  (let [security (get connection-config :security :ssl)]
    (if (some #(= security %) [:ssl :starttls :plain])
      security
      :ssl)))

(defn port [connection-config]
  (str (get connection-config :port (default-port-for-security (security connection-config)))))

(defn check-ssl-certs? [connection-config] (get connection-config :check-ssl-certs true))

(defn default-imap-properties ^Properties [connection-config]
  (doto (new Properties)
    (.setProperty "mail.imap.port" (port connection-config))
    (.setProperty "mail.imap.usesocketchannels" "true")
    (.setProperty "mail.imap.timeout" "5000")
    (.setProperty "mail.imap.partialfetch" "false")
    (.setProperty "mail.imap.fetchsize" "1048576")))

(defn security-properties [connection-config]
  (let [security-key (security connection-config)]
    (fn [^Properties properties]
      (cond (= security-key :ssl) (doto properties (.setProperty "mail.imap.ssl.enable", "true"))
            (= security-key :starttls) (doto properties (.setProperty "mail.imap.starttls.enable", "true"))
            (= security-key :plain) properties
            :else (doto properties (.setProperty "mail.imap.ssl.enable", "true"))))))

(defn certification-check-properties [connection-config]
  (if (not (check-ssl-certs? connection-config))
    (fn [^Properties properties] (doto properties (.setProperty "mail.imap.ssl.trust", "*")))
    (fn [^Properties properties] properties)))

(defn find-by-id-in-watchers [id]
  (get @connections id))

(defn set-debug-mode [connection-config]
  (let [debug? (get connection-config :debug false)]
    (fn [^Session session]
      (if debug? (doto session (.setDebug true)) session))))

(defn config->session [connection-config]
  (-> (default-imap-properties connection-config)
      ((security-properties connection-config))
      ((certification-check-properties connection-config))
      Session/getInstance
      ((set-debug-mode connection-config))))

(defn connection-config->store [connection-config]
  (let [session ^Session (config->session connection-config)]
    (if (= security :ssl)
      (.getStore session "imaps")
      (.getStore session "imap"))))

(defn login [connection-config]
  (let [store ^Store (connection-config->store connection-config)]
    (.connect store (:host connection-config) (:user connection-config) (:secret connection-config))
    store))

(defn folder-separator [^Store store] (.getSeparator (.getDefaultFolder store)))

(defn read-all-emails [id ^String folder-name options]
  (t/log! :info ["Read all e-mails from:" folder-name "with options:" options])
  (async/go (let [store (:store (:monitor (find-by-id-in-watchers id)))
                  folder ^Folder (doto (.getFolder ^Store store folder-name) (.open Folder/READ_WRITE))]
              (vec (doseq [message-num (range 1 (inc (.getMessageCount ^Folder folder)))
                           :let [message ^Message (.getMessage ^Folder folder message-num)]]
                     (with-open [os (ByteArrayOutputStream.)]
                       (.writeTo message os)
                       (async/>!! @messaging/main-chan (events/create-event :received-email
                                                                            (.toByteArray os)
                                                                            {:enrich true :move (:move options) :id id :folder folder :original-folder folder-name :message message}))))))))

(defn create-folder [^Store store ^String folder-name result-map]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (if (not (.exists folder))
      (do (.create folder Folder/HOLDS_MESSAGES)
          (conj result-map {folder-name :created}))
      (conj result-map {folder-name :already-exists}))))

(defn structured-folder-name [store lower-case-folder-name]
  (str parent-folder-name (folder-separator store) (s/capitalize lower-case-folder-name)))

(defn copy-message [^Message message ^Folder source-folder ^Folder target-folder]
  (try
    (.setPeek ^IMAPMessage message true)
    (.copyMessages source-folder (into-array Message [message]) target-folder)
    (t/log! :debug ["Copied" message])
    (.setFlag message Flags$Flag/DELETED true)
    (t/log! :debug ["Set DELETED flag for" message])
    (.expunge source-folder)
    (t/log! :debug ["Expunged source folder"])
    (catch Exception e (t/log! {:level :error :error e} ["There was an error copying and deleting the message" message]))))

(defn move-message [id ^Message message ^Folder source-folder ^String target-name]
  (let [connection (get @connections id)
        store (:store (:monitor connection))
        capabilities ^PersistentVector (:capabilities connection)
        target-folder ^IMAPFolder (.getFolder ^Store store ^String (structured-folder-name store target-name))]
    (if (.contains capabilities :move)
      (do (.setPeek ^IMAPMessage message true)
          (.moveMessages ^IMAPFolder source-folder (into-array Message [message]) target-folder))
      (do (t/log! :debug "Server does not support the IMAP MOVE command. Using copy and delete as fallback.")
          (copy-message message source-folder target-folder)))))

(defn client-event-loop
  "Listens to :enriched-email

  Options:
  :move - boolean

  If :move is true, move the e-mail to the corresponding category folder."
  [publisher]
  (let [local-chan (async/chan)]
    (async/sub publisher :enriched-email local-chan)
    (async/go-loop [event (async/<! local-chan)]
      (when (some? event)
        (when (and (true? (:move (:options event))) (some? (:category (:metadata (:payload event)))))
          (let [category-name (get-in event [:payload :metadata :category])]
            (when (some? category-name)
              (t/log! :info (str "Moving email: " (get-in event [:payload :header :subject]) " categorized as: " (get-in event [:payload :metadata :category])))
              (try (move-message (get-in event [:options :id]) (get-in event [:options :message]) (get-in event [:options :folder]) category-name)
                   (catch Exception e (t/log! {:level :error :error e} (.getMessage e)))
                   (finally (do (t/log! :debug ["Continue watching folder" (get-in event [:options :original-folder])])
                                (let [^Folder folder (get-in event [:options :folder])
                                      ^IdleManager im @idle-manager]
                                  (.watch im folder))))))))
        (recur (async/<! local-chan))))))

(defn create-folders
  ([store folder-names]
   (create-folders store folder-names {}))
  ([store folder-names result-map]
   (if (empty? folder-names)
     result-map
     (let [result (create-folder store (structured-folder-name store (first folder-names)) result-map)]
       (recur store (rest folder-names) result)))))

(defn create-imap-directories! [store]
  (let [category-names (mapv :name (db/get-categories))
        result (create-folders store category-names)]
    (t/log! :info ["Creating directories from category names" category-names])
    (t/log! {:level :info
             :data  {:result result}}
            "Created the directories.")))

(defn check-necessary-capabilities [^IMAPStore store]
  (when (not (.hasCapability store "MOVE"))
    (t/log! :warn ["The IMAP server" (.getURLName store) "does not have the capability: MOVE"])))

(defrecord FolderMonitor [^Store store ^Folder folder listen-channel]
  AutoCloseable
  (close [this]
    (.stop ^IdleManager @idle-manager)
    (when (.isOpen ^Folder (.folder this))
      (.close ^Folder (.folder this)))
    (.close store)))

(defn monitor-with-new-folder [connection-data folder]
  (->FolderMonitor (:store (:monitor connection-data)) folder (:listen-channel (:monitor connection-data))))

(defn capability-name [^IMAPStore store ^String cap-name]
  (when (.hasCapability store cap-name)
    (keyword (clojure.string/lower-case cap-name))))

(defn connection-object [^FolderMonitor monitor config capabilities]
  {:monitor monitor
   :config config
   :capabilities capabilities})

(defn swap-connection-with-new-monitor [identifier new-monitor]
  (let [old-connection (get @connections identifier)
        old-config (:config old-connection)
        old-capabilities (:capabilities old-connection)] ; assuming the capabilities won't change on reconnect
    (swap! connections assoc identifier (connection-object new-monitor old-config old-capabilities))))

(defn connection-object-with-capabilities [^FolderMonitor monitor config]
  (let [store (:store monitor)]
    (connection-object monitor config (filterv some? (mapv #(capability-name store %) ["MOVE"])))))

(defn message-count-adapter [id folder folder-name]
  (proxy [MessageCountAdapter] []
    (messagesAdded [^MessageCountEvent event]
      (t/log! :info "Received new message event.")
      (doseq [message ^IMAPMessage (.getMessages event)]
        (t/log! :debug ["Processing message:" message])
        (.setPeek ^IMAPMessage message true)
        (with-open [os ^OutputStream (ByteArrayOutputStream.)]
          (.writeTo ^IMAPMessage message os)
          (async/>!! @messaging/main-chan (events/create-event :received-email (.toByteArray os) {:enrich true :move true :id id :folder folder :original-folder folder-name :message message}))))
      (try
        (t/log! :debug ["Handled messagesAdded event. Resuming to watch the folder" folder-name])
        (.watch ^IdleManager @idle-manager folder)
        (catch Exception e
          (t/log! {:level :error :error e} (.getMessage e)))))))

(defn start-monitoring [id ^Store store ^String folder-name]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (when (not (.isOpen folder))
      (.open folder Folder/READ_WRITE))
    (let [listener (message-count-adapter id folder folder-name)]
      (swap! message-count-listeners conj {id listener})
      (.addMessageCountListener folder listener))
    (try
      (.watch ^IdleManager @idle-manager folder)
      (t/log! :info ["Started monitoring for" folder-name "in" (.getURLName store)])
      (catch Exception e
        (t/log! {:level :error :error e} (.getMessage e))))
    folder))

(defn stop-monitoring [id]
  (let [{monitor :monitor} (get @connections id)
        listener (get @message-count-listeners id)]
    (t/log! :debug ["Removing message count listener from folder" (:folder monitor)])
    (.removeMessageCountListener ^IMAPFolder (:folder monitor) listener)))

(defn swap-new-period-check [identifier future]
  (swap! health-checks (fn [futures new-future] (conj futures {identifier new-future})) future))

(defn start-monitoring-and-change-state [identifier connection-data]
  (let [folder (start-monitoring identifier (:store (:monitor connection-data)) (:folder (:config connection-data)))
        new-monitor (monitor-with-new-folder connection-data folder)]
    (swap-connection-with-new-monitor identifier new-monitor)))

(defn reconnect-to-store [identifier]
  (let [connection-data (get @connections identifier)
        store ^Store (:store (:monitor connection-data))]
    (t/log! :debug "Closing store.")
    (.close store)
    (try
      (t/log! :debug "Connecting to store.")
      (.connect store)
      (t/log! :debug "Starting to idle.")
      (start-monitoring-and-change-state identifier connection-data)
      (catch Exception e (t/log! {:level :error :error e} (.getMessage e))))))

(defn check-connection [identifier]
  (let [{monitor :monitor} (get @connections identifier)
        store (:store monitor)]
    (if (.isConnected ^Store store)
      (t/log! :debug "Store is still connected.")
      (do (t/log! :warn "Connection lost. Reconnecting to email server...")
          (reconnect-to-store identifier)))))

(defn check-folder [identifier]
  (let [{monitor :monitor} (get @connections identifier)
        folder (:folder monitor)]
    (if (.isOpen ^Folder folder)
      (t/log! :debug "Folder is still open.")
      (do (t/log! :info "Folder is closed. Reconnecting.")
          (reconnect-to-store identifier)))))

(defn create-idle-manager [session]
  (when (nil? @idle-manager)
    (reset! idle-manager (IdleManager. session (Executors/newCachedThreadPool)))))

(defn health-check-for-identifier [identifier]
  (let [scheduled-future (.scheduleAtFixedRate ^ScheduledExecutorService executor-service
                                               #(do
                                                  (try
                                                    (t/log! :debug ["Checking if the connection for id" identifier "is open"])
                                                    (check-connection identifier)
                                                    (t/log! :debug ["Checking if the folder for id" identifier "is open"])
                                                    (check-folder identifier)
                                                    (let [{monitor :monitor} (get @connections identifier)
                                                          ^Folder folder (:folder monitor)
                                                          ^Store store (:store monitor)
                                                          ^IdleManager im @idle-manager]
                                                      (t/log! :debug ["Resuming to watch folder:" (.getURLName store) "-" (.getFullName folder)])
                                                      (.watch im (:folder monitor)))
                                                    (catch Exception e (do (t/log! {:level :error :error e} "There was a problem during health check.")
                                                                           (reconnect-to-store identifier)))))
                                               120 (p/client-health-check-interval) TimeUnit/SECONDS)]
    (swap-new-period-check identifier scheduled-future)))

(defn config-id [something]
  (str (hash something)))

(defn connection-config->identifier [connection-config]
  (let [cleaned-config (dissoc connection-config :id)]
    (config-id cleaned-config)))

(defn create-folder-monitor [connection-config channel]
  (create-idle-manager (config->session connection-config))
  (let [store (login connection-config)
        identifier (connection-config->identifier connection-config)
        folder (start-monitoring identifier store (:folder connection-config))]
    (swap! connections (fn [subs new-data]
                         (conj subs {identifier new-data}))
           (connection-object-with-capabilities (->FolderMonitor store folder channel) connection-config))
    (health-check-for-identifier identifier)
    store))

(defn connect-using-id [id]
  (let [connection (get @connections id)
        listen-channel (:listen-channel (get @connections (:config connection)))]
    (create-folder-monitor (:config connection) listen-channel)))

(defn monitor->map [monitor]
  (let [store ^Store (-> monitor :store)
        folder ^IMAPFolder (-> monitor :folder)]
    {:connected (.isConnected ^Store store)
     :folder    (.isOpen ^IMAPFolder folder)}))

(defn folders-in-store [^Store store]
  (.list (.getDefaultFolder store) "*"))

(defn inbox-or-category-folder-name [^Store store ^String folder-name]
  (if (nil? folder-name) "INBOX" (structured-folder-name store folder-name)))

(defn move-messages-by-id-between-category-folders [^String id ^Store store message-id ^String source-name ^String target-name]
  (let [{config :config} (get @connections id)
        source-folder-name (inbox-or-category-folder-name store source-name)
        target-folder-name (inbox-or-category-folder-name store target-name)]
    (with-open [target-folder ^IMAPFolder (doto (.getFolder ^Store store ^String target-folder-name) (.open Folder/READ_WRITE))
                source-folder ^AutoCloseable (doto (.getFolder ^Store store ^String source-folder-name) (.open Folder/READ_WRITE))]
      (let [found-messages (.search source-folder (MessageIDTerm. message-id))]
        (t/log! :debug ["Found" (count found-messages) "messages when searched for the message-id:" message-id])
        (when (seq found-messages)
          (stop-monitoring id)
          (t/log! :debug ["Moving e-mail from" source-folder-name "to" target-folder-name])
          (.moveMessages ^IMAPFolder source-folder (into-array Message found-messages) target-folder))))
    (start-monitoring id store (:folder config))))

