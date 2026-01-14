(ns plauna.client
  (:require
   [clojure.core.async :as async]
   [plauna.core.events :as events]
   [plauna.preferences :as p]
   [plauna.database :as db]
   [plauna.client.oauth :as oauth]
   [clojure.string :as s]
   [taoensso.telemere :as t]
   [plauna.interfaces :as int]
   [plauna.messaging :as messaging])
  (:import
   (clojure.lang PersistentVector)
   (jakarta.mail Store Session Folder Message Flags$Flag AuthenticationFailedException)
   (org.eclipse.angus.mail.imap IMAPFolder IMAPMessage)
   (jakarta.mail.event MessageCountAdapter MessageCountEvent MessageCountListener)
   (jakarta.mail.search MessageIDTerm)
   (java.io ByteArrayOutputStream)
   (java.lang AutoCloseable)
   (java.util Properties UUID)
   (java.util.concurrent Executors)
   (org.eclipse.angus.mail.imap IdleManager IMAPStore)
   (java.util.concurrent Executors TimeUnit ScheduledExecutorService ScheduledFuture)))

(set! *warn-on-reflection* true)

(defonce executor-service (Executors/newSingleThreadScheduledExecutor))

(defonce parent-folder-name "Categories")

(defonce connections (atom {}))

(defonce health-checks (atom {}))

(declare connect)

(declare reconnect)

(declare parse-all-in-folder)

(declare start-monitoring)

(declare stop-monitoring)

(declare schedule-health-checks)

(defn default-port-for-security [security]
  (if (= security "ssl") 993 143))

(defn oauth2? [connection-config] (= "oauth2" (:auth-type connection-config)))

(defn security [connection-config]
  (let [security (get connection-config :security "ssl")]
    (if (some #(= security %) ["ssl" "starttls" "plain"])
      security
      "ssl")))

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

(defn oauth-properties [connection-config]
  (fn [^Properties properties]
    (if (oauth2? connection-config)
      (doto properties (.setProperty "mail.imap.auth.mechanisms" "XOAUTH2"))
      properties)))

(defn security-properties [connection-config]
  (let [security-key (security connection-config)]
    (fn [^Properties properties]
      (cond (= security-key "ssl") (doto properties (.setProperty "mail.imap.ssl.enable" "true"))
            (= security-key "starttls") (doto properties (.setProperty "mail.imap.starttls.enable" "true"))
            (= security-key "plain") properties
            :else (doto properties (.setProperty "mail.imap.ssl.enable" "true"))))))

(defn certification-check-properties [connection-config]
  (if (not (check-ssl-certs? connection-config))
    (fn [^Properties properties] (doto properties (.setProperty "mail.imap.ssl.trust" "*")))
    (fn [^Properties properties] properties)))

(defn set-debug-mode [connection-config]
  (let [debug? (get connection-config :debug false)]
    (fn [^Session session]
      (if debug? (doto session (.setDebug true)) session))))

(defn config->session [connection-config]
  (-> (default-imap-properties connection-config)
      ((security-properties connection-config))
      ((oauth-properties connection-config))
      ((certification-check-properties connection-config))
      Session/getInstance
      ((set-debug-mode connection-config))))

(defn connection-config->store [connection-config]
  (let [session ^Session (config->session connection-config)]
    (if (= security "ssl")
      (.getStore session "imaps")
      (.getStore session "imap"))))

(defn login
  ([connection-config ^Store store]
   (if (oauth2? connection-config)
     (let [tokens (db/get-oauth-tokens (:id connection-config))]
       (.connect store (:host connection-config) (:user connection-config) (:access-token tokens)))
     (.connect store (:host connection-config) (:user connection-config) (:secret connection-config)))
   store)
  ([connection-config]
   (login connection-config (connection-config->store connection-config))))

(defn folder-separator [^Store store] (.getSeparator (.getDefaultFolder store)))

(defn create-folder [^Store store ^String folder-name result-map]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (if (not (.exists folder))
      (do (.create folder Folder/HOLDS_MESSAGES)
          (conj result-map {folder-name :created}))
      (conj result-map {folder-name :already-exists}))))

(defn structured-folder-name [store lower-case-folder-name]
  (str parent-folder-name (folder-separator store) (s/capitalize lower-case-folder-name)))

(defn create-folders
  ([store folder-names]
   (create-folders store folder-names {}))
  ([store folder-names result-map]
   (if (empty? folder-names)
     result-map
     (let [result (create-folder store (structured-folder-name store (first folder-names)) result-map)]
       (recur store (rest folder-names) result)))))

(defn swap-new-period-check [identifier future]
  (swap! health-checks (fn [futures new-future] (conj futures {identifier new-future})) future))

;; Primitives

(defn clean-config [config]
  (-> (dissoc config :secret)
      (dissoc :debug)))

(defn id-from-config [config]
  (str (UUID/nameUUIDFromBytes (.getBytes ^String (str (hash (clean-config config)))))))

(defrecord ConnectionData [config ^Store store ^Folder folder ^IdleManager idle-manager capabilities ^MessageCountListener message-count-listener]
  AutoCloseable
  (close [this]
    (t/log! :info "Closing the idle manager, removing from health checks, closing the folder and the store.")
    (.stop idle-manager)
    (stop-monitoring this)
    (swap! health-checks dissoc (:id config))
    (when (.isOpen folder)
      (.close folder))
    (.close store)))

(defn get-connections [] (vals @connections))

(defn connection-data-from-id ^ConnectionData [id]
  (get @connections id))

(defn add-to-connections [^ConnectionData connection-data]
  (swap! connections conj {(:id (.config connection-data)) connection-data}))

;; Calls

(defn capability-name [^IMAPStore store ^String cap-name]
  (when (.hasCapability store cap-name)
    (keyword (clojure.string/lower-case cap-name))))

(defn capabilities [^Store store]
  (filterv some? (mapv #(capability-name store %) ["MOVE"])))

(defn start-idling-for-id [id]
  (let [^ConnectionData connection-data (connection-data-from-id id)]
    (t/log! :debug ["Starting to idle for id:" id "using connection-data" connection-data])
    (.watch ^IdleManager (.idle-manager connection-data) (.folder connection-data))))

(defn message-count-listener [connection-id folder folder-name]
  (proxy [MessageCountAdapter] []
    (messagesAdded [^MessageCountEvent event]
      (t/log! :info "Received new message event.")
      (doseq [message ^IMAPMessage (.getMessages event)]
        (t/log! :debug ["Processing message:" message])
        (.setPeek ^IMAPMessage message true)
        (with-open [os ^OutputStream (ByteArrayOutputStream.)]
          (.writeTo ^IMAPMessage message os)
          (async/>!! @messaging/main-chan (events/create-event :received-email (.toByteArray os) {:enrich true :move true :connection-id connection-id :folder folder :original-folder folder-name :message message})))
        (let [conn-data ^ConnectionData (connection-data-from-id connection-id)]
          (t/log! :debug ["Idling on the folder" folder-name "while waiting for new messages."])
          (.watch ^IdleManager (.idle-manager conn-data) (.folder conn-data)))))))

(defn open-folder-in-store [^Store store ^String folder-name]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (when (not (.isOpen folder))
      (.open folder Folder/READ_WRITE))
    folder))

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

(defn move-message [connection-id ^Message message ^Folder source-folder ^String target-name]
  (let [connection-data (connection-data-from-id connection-id)
        store (:store connection-data)
        capabilities ^PersistentVector (:capabilities connection-data)
        target-folder ^IMAPFolder (.getFolder ^Store store ^String (structured-folder-name store target-name))]
    (if (.contains capabilities :move)
      (do (t/log! :debug ["Moving message from" source-folder "to" target-folder])
          (.setPeek ^IMAPMessage message true)
          (.moveMessages ^IMAPFolder source-folder (into-array Message [message]) target-folder))
      (do (t/log! :debug "Server does not support the IMAP MOVE command. Using copy and delete as fallback.")
          (copy-message message source-folder target-folder)))))

(defn monitor->map [monitor]
  (if (nil? monitor)
    {:connected false :folder-open false}
    (let [store ^Store (-> monitor :store)
          folder ^IMAPFolder (-> monitor :folder)]
      {:connected (.isConnected ^Store store)
       :folder-open    (.isOpen ^IMAPFolder folder)})))

(defn folders-in-store [^Store store]
  (.list (.getDefaultFolder store) "*"))

(defn inbox-or-category-folder-name [^Store store ^String folder-name default]
  (if (nil? folder-name) default (structured-folder-name store folder-name)))

(defn connected? [^ConnectionData connection-data] (.isConnected ^Store (:store connection-data)))

(defn move-messages-by-id-between-category-folders [^String id message-id ^String source-name ^String target-name]
  (let [^ConnectionData connection-data (connection-data-from-id id)]
    (if (connected? connection-data)
      (let [^Store store (:store connection-data)
            ^String source-folder-name (inbox-or-category-folder-name store source-name (-> connection-data :config :folder))
            ^String target-folder-name (inbox-or-category-folder-name store target-name (-> connection-data :config :folder))]
        (with-open [^IMAPFolder target-folder (doto (.getFolder store target-folder-name) (.open Folder/READ_WRITE))
                    ^IMAPFolder source-folder (doto (.getFolder store source-folder-name) (.open Folder/READ_WRITE))]
          (let [found-messages (.search source-folder (MessageIDTerm. message-id))]
            (t/log! :debug ["Found" (count found-messages) "messages when searched for the message-id:" message-id])
            (when (seq found-messages)
              (if (= target-folder-name (:folder (:config connection-data)))
                (do
                  (stop-monitoring connection-data)
                  (t/log! :debug ["Moving e-mail from" source-folder-name "to" target-folder-name])
                  (.moveMessages source-folder (into-array Message found-messages) target-folder)
                  (start-monitoring connection-data))
                (do
                  (t/log! :debug ["Moving e-mail from" source-folder-name "to" target-folder-name])
                  (.moveMessages source-folder (into-array Message found-messages) target-folder)))))))
      (t/log! :info ["IMAP store in connection" (:id (:config connection-data)) "is not connected. Cancelling the move attempt."]))))

(defn refresh-access-token [connection-config]
  (let [provider (db/get-auth-provider (:auth-provider connection-config))
        token-data (db/get-oauth-tokens (:id connection-config))
        new-access-token (try (oauth/exchange-refresh-token-for-access-token provider (:refresh-token token-data)) (catch Exception e (t/log! :error e)))]
    (if (some? new-access-token)
      (db/update-access-token (:id connection-config) new-access-token)
      (do (t/log! :info ["Data for new access token was nil. Deleting the access token data in the database. The user will need to log in manually again."])
          (db/delete-access-token (:id connection-config))))))

(defn monitor-folder-name [folder-name]
  (if (or (nil? folder-name) (s/blank? folder-name)) "INBOX" folder-name))

;; Public Interface

(defn construct-connection-data [connection-config]
  (let [idle-manager (IdleManager. (config->session connection-config) (Executors/newCachedThreadPool))
        store (login connection-config)
        id (:id connection-config)
        folder-name-to-monitor (monitor-folder-name (:folder connection-config))
        folder (open-folder-in-store store folder-name-to-monitor)
        listener (message-count-listener id folder folder-name-to-monitor)
        connection-data (->ConnectionData connection-config store folder idle-manager (capabilities store) listener)]
    (add-to-connections connection-data)
    connection-data))

(defn disconnect [^AutoCloseable connection-data] (.close connection-data))

(defn reconnect [^ConnectionData connection-data]
  (try
    (t/log! :info ["Trying to reconnect to" (-> connection-data .config :host) "as" (-> connection-data .config :user)])
    (when (oauth2? (.config connection-data)) (refresh-access-token (.config connection-data)))
    (login (.config connection-data) (.store connection-data))
    (catch AuthenticationFailedException e (t/log! :error e))))

(defn start-monitoring [connection-data]
  (try
    (.addMessageCountListener ^IMAPFolder (:folder connection-data) (message-count-listener (:id (:config connection-data)) (:folder connection-data) (-> connection-data :config :folder)))
    (t/log! :info ["Started monitoring for" (:folder (:config connection-data)) "in" (.getURLName ^Store (:store connection-data))])
    (.watch ^IdleManager (:idle-manager connection-data) ^Folder (:folder connection-data))
    (catch Exception e
      (t/log! {:level :error :error e} (.getMessage e))))
  connection-data)

(defn stop-monitoring [connection-data]
  (t/log! :info ["Removing message count listener from folder" (-> connection-data :config :folder)])
  (let [connection-id (:id (:config connection-data))
        sf ^ScheduledFuture (get @health-checks connection-id)]
    (when (some? sf) (.cancel sf true)))
  (.removeMessageCountListener ^IMAPFolder (:folder connection-data) (:message-count-listener connection-data))
  connection-data)

(defn create-category-folders! [connection-data categories]
  (let [store ^Store (:store connection-data)
        result (create-folders store categories)]
    (t/log! :info ["Creating directories from category names" categories])
    (t/log! {:level :info
             :data  {:result result}}
            "Created the directories.")
    connection-data))

(defn schedule-health-checks [^ConnectionData connection-data]
  (let [^Store store (:store connection-data)
        ^Folder folder (:folder connection-data)
        config (:config connection-data)
        scheduled-future (.scheduleAtFixedRate ^ScheduledExecutorService executor-service
                                               #(do
                                                  (try
                                                    (t/log! :debug ["Checking if the connection for" (:user config) "is open"])
                                                    (if (.isConnected store)
                                                      (t/log! :debug "Store is still connected.")
                                                      (do
                                                        (t/log! :warn "Connection lost. Reconnecting to email server...")
                                                        (reconnect connection-data)))
                                                    (t/log! :debug ["Checking if the folder " (:folder config) "is open"])
                                                    (if (.isOpen folder)
                                                      (t/log! :debug "Folder is still open.")
                                                      (do (t/log! :info "Folder is closed. Opening it again.")
                                                          (.open folder Folder/READ_WRITE)))
                                                    (t/log! :debug "Idling and waiting for messages after a health check.")
                                                    (start-idling-for-id (:id config))
                                                    (catch Exception e
                                                      (t/log! {:level :error :error e} "There was an error during health check. The connection is probably in a broken state."))))
                                               120 (p/client-health-check-interval) TimeUnit/SECONDS)]
    (swap-new-period-check (:id config) scheduled-future)
    connection-data))

(defn folder-from-connection [connection-data folder-name]
  (let [^Store store (:store connection-data)]
    (open-folder-in-store store folder-name)))

(defn id-from-connection [connection-data] (get-in connection-data [:config :id]))

(defn doto-message->byte-array [^Message message do-func & args]
  (with-open [os (ByteArrayOutputStream.)]
    (.writeTo message os)
    (apply do-func (.toByteArray os) args)))

(defn loop-over-messages-in-folder [^Folder folder body]
  (doseq [message-num (range 1 (inc (.getMessageCount folder)))
          :let [^Message message (.getMessage folder message-num)]]
    (t/log! :debug ["Reading message number" message-num "from" (.getName ^Folder folder)])
    (body message)))

(defn put-to-main-chan [event] (async/>!! @messaging/main-chan event))

(defn parse-all-in-folder
  "Read all emails from a folder, convert them to outputstream and send them via :received-email channels"
  [connection-data folder-name move?]
  (t/log! :info ["Read all e-mails from:" folder-name "with move option:" move?])
  (let [folder (folder-from-connection  connection-data folder-name)
        connection-id (id-from-connection connection-data)
        transport-function (fn [byte-array options-seq] (put-to-main-chan (events/create-event :received-email byte-array options-seq)))]
    (async/go (loop-over-messages-in-folder folder (fn [message] (doto-message->byte-array
                                                                  message
                                                                  transport-function
                                                                  {:enrich true :move move? :connection-id connection-id :folder folder}))))))

(defmulti connect :auth-type)

(defmethod connect "oauth2" [connection-config]
  (refresh-access-token connection-config)
  (try (-> (construct-connection-data connection-config)
           start-monitoring
           schedule-health-checks)
       (catch AuthenticationFailedException e (t/log! :error e))))

(defmethod connect :default [connection-config]
  (-> (construct-connection-data connection-config)
      start-monitoring
      schedule-health-checks))

(defmulti handle-incoming-events :type)

(defmethod handle-incoming-events :enriched-email [event]
  (when (and (true? (:move (:options event))) (some? (:category (:metadata (:payload event)))))
    (let [category-name (get-in event [:payload :metadata :category])]
      (when (some? category-name)
        (t/log! :info (str "Moving email: " (get-in event [:payload :header :subject]) " categorized as: " (get-in event [:payload :metadata :category])))
        (let [connection-id (get-in event [:options :connection-id])
              message-id (get-in event [:payload :header :message-id])
              ^IMAPFolder folder (get-in event [:options :folder])
              ;; Message classes are not thread safe. Fetch it again. Get the first one found
              ^IMAPMessage message (aget (.search folder (MessageIDTerm. message-id)) 0)]
          (try (move-message connection-id message folder category-name)
               (catch jakarta.mail.FolderClosedException e
                 (do (t/log! {:level :error :error e})
                     (let [folder-name (.getName folder)
                           message-id (.getMessageID message)]
                       (t/log! :debug ["Lost connection to folder" folder-name "with connection-id" connection-id ". Trying to reconnect and move the message again."])
                       (reconnect (connection-data-from-id connection-id))
                       (move-messages-by-id-between-category-folders connection-id message-id folder-name category-name))))
               (catch Exception e (t/log! {:level :error :error e} (.getMessage e)))
               (finally (start-idling-for-id connection-id)))
          connection-id)))))

(defn connection-id-for-email
  "Tries to find out the id of the connection the email belongs to. Returns nil if no active connection is found."
  [connection-data-vec email]
  (loop  [connections connection-data-vec
          result nil]
    (if (or (some? result) (nil? (seq connections)))
      result
      (let [^ConnectionData connection (first (vals connections))
            recipients (filterv #(= :receiver (:type %)) (:participants email))
            connection-user (get-in connection [:config :user])
            match (get (filterv (fn [sender] (= (:address sender) connection-user)) recipients) 0)]
        (if (some? match) (recur (rest connections) (get-in connection [:config :id])) (recur (rest connections) nil))))))

(defrecord ImapClient []
  int/EmailClient
  (start-monitor [_ config] (connect config)))

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
        (t/log! :debug ["Client event loop processing the event:" event])
        (handle-incoming-events event)
        (recur (async/<! local-chan))))))
