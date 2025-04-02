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
   (jakarta.mail Store Session Folder Message)
   (org.eclipse.angus.mail.imap IMAPFolder IMAPMessage)
   (jakarta.mail.event MessageCountAdapter MessageCountEvent)
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

(defonce watchers (atom {}))

(defonce health-checks (atom {}))

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
  (filter #(= id (:id (first %))) @watchers))

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
  (async/go (let [store (:store (second (first (find-by-id-in-watchers id))))
                  folder ^Folder (doto (.getFolder ^Store store folder-name) (.open Folder/READ_WRITE))]
              (vec (doseq [message-num (range 1 (inc (.getMessageCount ^Folder folder)))
                           :let [message ^Message (.getMessage ^Folder folder message-num)]]
                     (with-open [os (ByteArrayOutputStream.)]
                       (.writeTo message os)
                       (async/>!! @messaging/main-chan (events/create-event :received-email (.toByteArray os) {:enrich true :move (:move options) :store store :folder folder :original-folder folder-name :message message}))))))))

(defn stop-subscription [host user folder-name]
  (let [key {:host host :user user :folder-name folder-name}
        connection-and-idle-manager (get @watchers key)]
    (cond (some? connection-and-idle-manager)
          (.stop ^IdleManager (:idle-manager connection-and-idle-manager)))
    (swap! watchers (fn [subs sub] (dissoc subs sub)) key)))

(defn create-folder [^Store store ^String folder-name result-map]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (if (not (.exists folder))
      (do (.create folder Folder/HOLDS_MESSAGES)
          (conj result-map {folder-name :created}))
      (conj result-map {folder-name :already-exists}))))

(defn structured-folder-name [store lower-case-folder-name]
  (str parent-folder-name (folder-separator store) (s/capitalize lower-case-folder-name)))

(defn move-message-from-folder-by-id [^Store store ^Message message ^Folder source-folder ^String target-name]
  (let [target-folder ^IMAPFolder (.getFolder ^Store store ^String (structured-folder-name store target-name))]
    (.moveMessages ^IMAPFolder source-folder (into-array Message [message]) target-folder)))

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
              (try (move-message-from-folder-by-id (get-in event [:options :store]) (get-in event [:options :message]) (get-in event [:options :folder]) category-name)
                   (catch Exception e (t/log! {:level :error :error e} (.getMessage e)))))))
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

(defn monitor-with-new-folder [monitor folder]
  (->FolderMonitor (:store monitor) folder (:listen-channel monitor)))

(defn message-count-adapter [store folder folder-name]
  (proxy [MessageCountAdapter] []
    (messagesAdded [^MessageCountEvent event]
      (t/log! :info "Received new message event.")
      (doseq [message ^IMAPMessage (.getMessages event)]
        (t/log! :debug ["Processing message:" (bean message)])
        (.setPeek ^IMAPMessage message true)
        (with-open [os ^OutputStream (ByteArrayOutputStream.)]
          (.writeTo ^IMAPMessage message os)
          (async/>!! @messaging/main-chan (events/create-event :received-email (.toByteArray os) {:enrich true :move true :store store :folder folder :original-folder folder-name :message message}))
          (try
            (.watch ^IdleManager @idle-manager folder)
            (catch Exception e
              (t/log! {:level :error :error e} (.getMessage e)))))))))

(defn start-monitoring [^Store store ^String folder-name]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (when (not (.isOpen folder))
      (.open folder Folder/READ_WRITE))
    (.addMessageCountListener folder (message-count-adapter store folder folder-name))
    (try
      (.watch ^IdleManager @idle-manager folder)
      (t/log! :info ["Started monitoring for" folder-name "in" (.getURLName store)])
      (catch Exception e
        (t/log! {:level :error :error e} (.getMessage e))))
    folder))

(defn swap-new-monitor [identifier monitor]
  (swap! watchers assoc identifier monitor))

(defn swap-new-period-check [identifier future]
  (swap! health-checks (fn [futures new-future] (conj futures {identifier new-future})) future))

(defn start-monitoring-and-change-state [identifier monitor]
  (let [folder (start-monitoring (:store monitor) (:folder identifier))
        new-monitor (monitor-with-new-folder monitor folder)]
    (swap-new-monitor identifier new-monitor)))

(defn reconnect-to-store [identifier]
  (let [monitor (get @watchers identifier)
        store ^Store (:store monitor)]
    (t/log! :debug "Closing store.")
    (.close store)
    (try
      (t/log! :debug "Connecting to store.")
      (.connect store)
      (t/log! :debug "Starting to idle.")
      (start-monitoring-and-change-state identifier monitor)
      (catch Exception e
        (do (t/log! {:level :error :error e} (.getMessage e))
            (Thread/sleep 5000)
            (reconnect-to-store identifier))))))

(defn check-connection [identifier]
  (let [monitor (get @watchers identifier)
        store (:store monitor)]
    (if (.isConnected ^Store store)
      (t/log! :debug "Store is still connected.")
      (do (t/log! :warn "Connection lost. Reconnecting to email server...")
          (reconnect-to-store identifier)))))

(defn check-folder [identifier]
  (let [monitor (get @watchers identifier)
        folder (:folder monitor)]
    (if (.isOpen ^Folder folder)
      (t/log! :debug "Folder is still open.")
      (do (t/log! :info "Folder is closed. Reconnecting.")
          (reconnect-to-store identifier)))))

(defn create-idle-manager [session]
  (if (nil? @idle-manager)
    (let [es (Executors/newCachedThreadPool)]
      (reset! idle-manager (IdleManager. session es)))
    nil))

(defn health-check-for-identifier [identifier]
  (let [scheduled-future (.scheduleAtFixedRate ^ScheduledExecutorService executor-service
                                               #(do
                                                  (check-connection identifier)
                                                  (check-folder identifier))
                                               60 (p/client-health-check-interval) TimeUnit/SECONDS)]
    (swap-new-period-check identifier scheduled-future)))

(defn config-id [something]
  (str (hash something)))

(defn connection-config->identifier [connection-config]
  (let [cleaned-config (dissoc connection-config :id)]
    (-> connection-config (assoc :id (config-id cleaned-config)))))

(defn identifier->connection-config [id]
  (first (filter #(= id (:id %)) (keys @watchers))))

(defn create-folder-monitor [connection-config channel]
  (let [store (login connection-config)
        _ (create-idle-manager (config->session connection-config))
        identifier (connection-config->identifier connection-config)
        folder (start-monitoring store (:folder connection-config))]
    (swap! watchers (fn [subs new-data]
                      (conj subs {identifier new-data}))
           (->FolderMonitor store folder channel))
    (health-check-for-identifier identifier)
    store))

(defn connect-using-id [id]
  (let [config (identifier->connection-config id)
        listen-channel (:listen-channel (get @watchers config))]
    (create-folder-monitor config listen-channel)))

(defn monitor->map [monitor]
  (let [store ^Store (-> monitor :store)
        folder ^IMAPFolder (-> monitor :folder)]
    {:connected (.isConnected ^Store store)
     :folder    (.isOpen ^IMAPFolder folder)}))

(defn folders-in-store [^Store store]
  (.list (.getDefaultFolder store) "*"))
