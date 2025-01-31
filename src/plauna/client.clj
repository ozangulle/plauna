(ns plauna.client
  (:require
   [clojure.core.async :as async]
   [plauna.database :as db]
   [plauna.core.events :as events]
   [clojure.string :as s]
   [taoensso.telemere :as t]
   [plauna.messaging :as messaging])
  (:import
   (jakarta.mail Store Session Folder Message)
   (org.eclipse.angus.mail.imap IMAPFolder IMAPMessage)
   (jakarta.mail.search SearchTerm)
   (jakarta.mail.event MessageCountAdapter MessageCountEvent)
   (java.io ByteArrayOutputStream)
   (java.lang AutoCloseable)
   (java.util Properties)
   (java.util.concurrent Executors)
   (org.eclipse.angus.mail.imap IdleManager)
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

(defn properties ^Properties []
  (doto (new Properties)
    (.setProperty "mail.imap.ssl.enable", "true")
    (.setProperty "mail.imaps.usesocketchannels" "true")
    (.setProperty "mail.imaps.timeout" "5000")
    (.setProperty "mail.imaps.partialfetch" "false")
    (.setProperty "mail.imap.fetchsize" "1048576")))

(defn find-by-id-in-watchers [id]
  (filter #(= id (:id (first %))) @watchers))

(defn message-id-search-term [message-id]
  (proxy [SearchTerm] [] (match [^Message message]
                           (= message-id (first (.getHeader message "Message-ID"))))))

(defn connect [connection-config]
  (let [session (Session/getInstance (properties))
        store (.getStore session "imaps")]
    (.connect store (:host connection-config) (:user connection-config) (:secret connection-config))
    store))

(defn folder-separator [^Store store] (.getSeparator (.getDefaultFolder store)))

(defn read-all-emails [id ^String folder-name options]
  (async/go (let [store (:store (second (first (find-by-id-in-watchers id))))
                  inbox ^Folder (doto (.getFolder ^Store store folder-name) (.open Folder/READ_WRITE))]
              (vec (doseq [message-num (range 1 (inc (.getMessageCount ^Folder inbox)))
                           :let [message ^Message (.getMessage ^Folder inbox message-num)]]
                     (with-open [os (ByteArrayOutputStream.)]
                       (.writeTo message os)
                       (async/>!! @messaging/main-chan {:type :received-email :options {:enrich true :refolder (:refolder options) :store store :original-folder folder-name} :payload (.toByteArray os)})))))))

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

(defn move-messages-by-id [^Store store message-id ^String source-name ^String target-name]
  (with-open [target-folder ^IMAPFolder (doto (.getFolder ^Store store ^String (structured-folder-name store target-name)) (.open Folder/READ_WRITE))
              source-folder ^AutoCloseable (doto (.getFolder store source-name) (.open Folder/READ_WRITE))]
    (.moveMessages ^IMAPFolder source-folder (into-array Message (.search source-folder (message-id-search-term message-id))) target-folder)))

(defn client-loop
  "Listens to :enriched-email

  Options:
  :refolder - boolean

  If :refolder is true, move the e-mail to the corresponding category folder."
  [publisher]
  (let [local-chan (async/chan)]
    (async/sub publisher :enriched-email local-chan)
    (async/go-loop [event (async/<! local-chan)]
      (when (some? event)
        (when (true? (:refolder (:options event)))
          (t/log! :info (str "Moving email: " (-> event :payload :header :subject) " categorized as: " (-> event :payload :metadata :category)))
          (let [message-id (-> event :payload :header :message-id)
                category-name (-> event :payload :metadata :category)]
            (when (some? category-name)
              (move-messages-by-id (-> event :options :store) message-id (-> event :options :original-folder) category-name)))))
      (recur (async/<! local-chan)))))

(defn create-folders
  ([store folder-names]
   (create-folders store folder-names {}))
  ([store folder-names result-map]
   (if (empty? folder-names)
     result-map
     (let [result (create-folder store (structured-folder-name store (first folder-names)) result-map)]
       (recur store (rest folder-names) result)))))

(defn setup-folders! [connection-config category-names]
  (with-open [store ^Store (connect connection-config)]
    (create-folders store category-names)))

(defn initialize-client-setup! [connection-config]
  (let [category-names (mapv :name (db/get-categories))]
    (t/log! :info ["Creating directories from category names" category-names])
    (t/log! {:level :info
             :data  {:result (setup-folders! connection-config category-names)}}
            "Created the directories.")))

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
        (t/log! :debug "Processing message.")
        (.setPeek ^IMAPMessage message true)
        (with-open [os ^OutputStream (ByteArrayOutputStream.)]
          (t/log! :debug "Created output stream")
          (.writeTo ^IMAPMessage message os)
          (t/log! :debug "Wrote to output stream")
          (async/>!! @messaging/main-chan (events/create-event :received-email (.toByteArray os) {:enrich true :refolder true :store store :original-folder folder-name :message message}))
          (try
            (.watch ^IdleManager @idle-manager folder)
            (catch Exception e
              (t/log! :error (.getMessage e)))))))))

(defn start-monitoring [^Store store ^String folder-name]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (when (not (.isOpen folder))
      (.open folder Folder/READ_WRITE))
    (.addMessageCountListener folder (message-count-adapter store folder folder-name))
    (try
      (.watch ^IdleManager @idle-manager folder)
      (t/log! :info ["Started monitoring for" folder-name "in" (.getURLName store)])
      (catch Exception e
        (t/log! :error (.getMessage e))))
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
        (do (t/log! :error (.getMessage e))
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
  (let [es (Executors/newCachedThreadPool)]
    (reset! idle-manager (IdleManager. session es))))

(defn health-check-for-identifier [identifier]
  (let [scheduled-future (.scheduleAtFixedRate ^ScheduledExecutorService executor-service
                                               #(do
                                                  (check-connection identifier)
                                                  (check-folder identifier))
                                               1 1 TimeUnit/MINUTES)]
    (swap-new-period-check identifier scheduled-future)))

(defn config-id [something]
  (str (hash something)))

(defn connection-config->identifier [connection-config]
  (let [cleaned-config (dissoc connection-config :id)]
    (-> connection-config (assoc :id (config-id cleaned-config)))))

(defn identifier->connection-config [id]
  (first (filter #(= id (:id %)) (keys @watchers))))

(defn create-folder-monitor [connection-config channel]
  (let [store (connect connection-config)
        _ (create-idle-manager (Session/getInstance (properties)))
        identifier (connection-config->identifier connection-config)
        folder (start-monitoring store (:folder connection-config))]
    (swap! watchers (fn [subs new-data]
                      (conj subs {identifier new-data}))
           (->FolderMonitor store folder channel))
    (health-check-for-identifier identifier)))

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
