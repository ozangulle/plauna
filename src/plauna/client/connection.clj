(ns plauna.client.connection
  (:require
   [clojure.string :as s]
   [plauna.application :as app]
   [plauna.client.oauth :as oauth]
   [plauna.client.parser :as parser]
   [plauna.client.session :as session]
   [plauna.database :as db]
   [plauna.interfaces :as int]
   [taoensso.telemere :as t]
   [clojure.core.async :as async]
   [plauna.core.email :as core-email])
  (:import
   (clojure.lang PersistentVector)
   (plauna.interfaces IMAPConnection)
   (jakarta.mail Store Session Folder Message Flags$Flag AuthenticationFailedException)
   (org.eclipse.angus.mail.imap IMAPFolder IMAPMessage)
   (jakarta.mail.event MessageCountAdapter MessageCountEvent ConnectionAdapter)
   (jakarta.mail.search MessageIDTerm)
   (java.util UUID)
   (java.util.concurrent Executors)
   (org.eclipse.angus.mail.imap IdleManager IMAPStore)
   (java.util.concurrent Executors)))

(defonce executor-service (Executors/newVirtualThreadPerTaskExecutor))

(defonce parent-folder-name "Categories")

(def health-check-interval 120000)

(def reconnection-wait-time 60000)

(defn folder-separator [^Store store] (.getSeparator (.getDefaultFolder store)))

(defn structured-folder-name [store lower-case-folder-name]
  (str parent-folder-name (folder-separator store) (s/capitalize lower-case-folder-name)))

(defn- connection-config->store [connection-config]
  (let [session ^Session (session/config->session connection-config)]
    (.getStore session "imap")))

(defn- capability-name [^IMAPStore store ^String cap-name]
  (when (.hasCapability store cap-name)
    (keyword (clojure.string/lower-case cap-name))))

(defn- capabilities [^Store store]
  (filterv some? (mapv #(capability-name store %) ["MOVE"])))

(defn- clean-config [config]
  (-> (dissoc config :secret)
      (dissoc :debug)))

;; TODO not necessary anymore. Assign this id only during creation and then just refer to it. No need for this to be deterministic
(defn id-from-config [config]
  (str (UUID/nameUUIDFromBytes (.getBytes ^String (str (hash (clean-config config)))))))

(defn- set-message-as-peek [^IMAPMessage message] (.setPeek message true))

(defn- set-messages-as-peek [messages] (doseq [message messages] (set-message-as-peek message)))

(defn- open-folder-in-store [^Store store ^String folder-name]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (when (not (.isOpen folder))
      (.open folder Folder/READ_WRITE))
    folder))

(defn- refresh-access-token [connection]
  (let [connection-config (:imap (:config connection))
        provider (db/get-auth-provider (:auth-provider connection-config))
        token-data (db/get-oauth-tokens (:id connection-config))
        new-access-token (try (oauth/exchange-refresh-token-for-access-token provider (:refresh-token token-data)) (catch Exception e (t/log! :error e)))]
    (if (some? new-access-token)
      (db/update-access-token (:id connection-config) new-access-token)
      (do (t/log! :info ["Data for new access token was nil. Deleting the access token data in the database. The user will need to log in manually again."])
          (db/delete-access-token (:id connection-config))))))

(defonce state-types {:store Store})

(defmacro get-state
  [connection key]
  (let [tag (get state-types key)
        form `(get (deref (get ~connection :state)) ~key)]
    (with-meta form
      (merge (meta form)
             {:tag tag}))))

(defmulti connect-imap (fn [connection] (-> connection :config :imap :auth-type)))

(defmethod connect-imap "oauth2" [connection]
  (refresh-access-token connection)
  (try
    (let [{:keys [db]} (:context connection)
          connection-config (:imap (:config connection))
          tokens (int/fetch-oauth-token-data db (:id connection-config))]
      (.connect (get-state connection :store) (:host connection-config) (:user connection-config) (:access-token tokens)))
       (catch AuthenticationFailedException e
         (t/log! :error e))
       (catch Exception e
         (t/log! :error e))))

(defmethod connect-imap :default [connection]
  (try
    (let [connection-config (:imap (:config connection))]
      (.connect (get-state connection :store) (:host connection-config) (:user connection-config) (:secret connection-config)))
    (catch AuthenticationFailedException e
      (t/log! :error e))
    (catch Exception e
      (t/log! :error e))))

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

(defmacro try-log-restart [connection & form]
  `(try ~@form
        (catch java.lang.Exception ex#
          (t/log! :error ex#)
          (restart-monitoring ~connection))))

(defn inbox-or-category-folder-name [^Store store ^String folder-name default]
  (let [real-default (if (s/blank? default) "INBOX" default)]
    (if (nil? folder-name) real-default (structured-folder-name store folder-name))))

(defn move-message-from-folder-to-folder-name
  "Find the proper location for the email and move it there. Returns the name of the folder to which the email was moved."
  [connection ^Message message ^Folder source-folder ^String target-name]
  (let [store (get-state connection :store)
        capabilities ^PersistentVector (capabilities store)
        structured-folder (inbox-or-category-folder-name store target-name "")
        target-folder ^IMAPFolder (.getFolder ^Store store ^String structured-folder)]
    (if (.contains capabilities :move)
      (do (t/log! :debug ["Moving message from" source-folder "to" target-folder])
          (.setPeek ^IMAPMessage message true)
          (.moveMessages ^IMAPFolder source-folder (into-array Message [message]) target-folder)
          structured-folder)
      (do (t/log! :debug "Server does not support the IMAP MOVE command. Using copy and delete as fallback.")
          (copy-message message source-folder target-folder)
          structured-folder))))

(defmulti handle-move-email (fn [_ _ source-folder _] (type source-folder)))

(defmethod handle-move-email IMAPFolder [connection message source-folder target-name]
  (move-message-from-folder-to-folder-name connection message source-folder target-name))

(defmethod handle-move-email java.lang.String [connection message source-name target-name]
  (move-message-from-folder-to-folder-name connection message (open-folder-in-store (:store connection) source-name) target-name))

(defrecord FolderConfig [name type category])

(defn- watch-folder [connection ^IMAPFolder folder]
  (t/log! :debug ["Starting to watch" (.getName folder)])
  (.watch ^IdleManager (:idle-manager connection) folder))

(defmulti message-count-listener (fn [folder-config _ _ ] (nil? (:category folder-config))))

(defmethod message-count-listener
  true
  [_ imap-folder connection]
  (proxy [MessageCountAdapter] []
    (messagesAdded [^MessageCountEvent event]
      (t/log! :debug "Received new message event.")
      (doseq [message ^IMAPMessage (.getMessages event)]
        (t/log! :debug ["Processing message:" message])
        (.setPeek ^IMAPMessage message true)
        (try
          (let [parsed-email (parser/message->email message)
                process (app/handle-incoming-imap-email parsed-email connection)]
            (if (= :error (:result process))
              (t/log! :error ["An error occured while handling incoming message" (:exception process)])
              (let [category (:category process)]
                                        ; FIXME correct category name here
                (if (some? category)
                  (move-message-from-folder-to-folder-name connection message imap-folder category)
                  (t/log! :debug ["Email" (core-email/message-id parsed-email) "was not categorized. Not moving the message."])))))
          (finally (watch-folder connection imap-folder)))))))

(defmethod message-count-listener
  false
  [folder-config imap-folder connection]
  (proxy [MessageCountAdapter] []
    (messagesAdded [^MessageCountEvent event]
      (t/log! :debug "Received new message event.")
      (doseq [message ^IMAPMessage (.getMessages event)]
        (t/log! :debug ["Processing message:" message])
        (.setPeek ^IMAPMessage message true)
        (try
          (let [parsed-email (parser/message->email message)]
            (app/recategorize-email parsed-email (.category folder-config) (:context connection)))
          (finally (watch-folder connection imap-folder)))))))

(defn- remove-all-folder-listeners [folder-listener-pairs]
  (doseq [pair folder-listener-pairs]
    (let [folder ^IMAPFolder (first pair)
          listener ^MessageCountAdapter (second pair)]
      (.removeFolderListener folder listener))))

(defn stop-health-checks [connection]
  (when-let [health-chan (get-state connection :health-chan)]
    (async/close! health-chan)))

(defn- close-and-clean-up [connection]
  (stop-health-checks connection)
  (remove-all-folder-listeners (get-state connection :folder-listener-pairs))
  (.close (get-state connection :store)))

(defn- restart-monitoring [connection]
  (close-and-clean-up connection)
  (.connect connection)
  (if (.connected? connection)
    (.monitor-folders connection)
    (do (t/log! :error ["Reconnection failed. Waiting" reconnection-wait-time "milliseconds before retrying"])
        (recur connection))))

(defn- folders->folder-message-count-listeners
  "Registers a MessageCountListener on the folder.
  Returns a vector with the imap folder at first position and the listener at the second"
  [connection]
  (doall
   (try-log-restart
       connection
       (for [folder-config (:folders connection)]
         (let [imap-folder ^IMAPFolder (open-folder-in-store (get-state connection :store) (:name folder-config))
               listener (message-count-listener folder-config imap-folder connection)
               folder-listener (.addMessageCountListener ^IMAPFolder imap-folder listener)]
           (t/log! :info ["Started monitoring for" (:name folder-config) "in" (.getURLName (get-state connection :store))])
           (watch-folder connection imap-folder)
           [imap-folder folder-listener])))))

(defn health-check-imap-folder-pairs [connection]
  (let [folder-listener-pairs (:folder-listener-pairs (deref (:state connection)))]
    (doseq [pair folder-listener-pairs]
      (let [folder ^IMAPFolder (first pair)
            store (get-state connection :store)]
        (if (.isConnected ^Store store)
          (try-log-restart
           connection
           (if (.isOpen folder)
             (do
               (t/log! :debug [(.getName folder) "is open"])
               (watch-folder connection folder))
             (do
               (t/log! :debug [(.getName folder) "is closed. Trying to open."])
               (.open folder Folder/READ_WRITE)
               (watch-folder connection folder))))
          (do
            (t/log! :info ["Store" (.getURLName ^Store store) "is closed. Plauna will try to clean up the connection, reconnect and start monitoring the necessary folders."])
            (restart-monitoring connection)))))))

(defn- move-messages-by-id-between-category-folders
  "Return true if the message could be moved. False if not."
  [^IMAPConnection connection  ^String message-id ^String source-name ^String target-name]
  (if (.connected? connection)
    (let [^Store store (get-state connection :store)
          ^String source-folder-name (inbox-or-category-folder-name store source-name (-> connection :config :folder))
          ^String target-folder-name (inbox-or-category-folder-name store target-name (-> connection :config :folder))]
      (if (= (:folder (:imap (:config connection))) target-folder-name)
        (do (t/log! :error ["Moving emails to" (:folder (:imap (:config connection))) "is not supported because this is the main Inbox folder."])
            false)
        (with-open [^IMAPFolder target-folder (open-folder-in-store store target-folder-name)
                    ^IMAPFolder source-folder (open-folder-in-store store source-folder-name)]
          (let [found-messages (.search source-folder (MessageIDTerm. message-id))]
            (t/log! :debug ["Found" (count found-messages) "messages when searched for the message-id:" message-id])
            (if (some? (seq found-messages))
              (do
                (set-messages-as-peek found-messages)
                (t/log! :debug ["Moving e-mail from" source-folder-name "to" target-folder-name])
                (.moveMessages source-folder (into-array Message found-messages) target-folder)
                true)
              (do (t/log! :info ["No messages found in" source-folder-name "in store" (.getURLName store)])
                  false))))))
    (do
      (t/log! :info ["IMAP store in connection" (:id (:imap (:config connection))) "is not connected. Cancelling the move attempt."])
      false)))

(defrecord Connection [id config folders ^IdleManager idle-manager context state]
  int/IMAPConnection

  (connect [this] (connect-imap this))

  (connected? [this] (.isConnected (get-state this :store)))

  (list-folders [this] (.list (.getDefaultFolder (get-state this :store)) "*"))

  (no-of-messages-in-folder [this folder-name]
    (let [folder ^Folder (open-folder-in-store (get-state this :store) folder-name)]
      {:message-count (.getMessageCount folder)
       :connection-id id
       :folder folder}))

  (nth-message-in-folder [this folder-name n]
    (let [folder (open-folder-in-store (get-state this :store) folder-name)
          message (.getMessage ^IMAPFolder folder n)]
      (set-message-as-peek message)
      (t/log! :debug ["Reading message number" n "from" (.getName ^IMAPFolder folder)])
      {:email (parser/message->email message)
       :message message}))

  (move-message [this message source-folder-name target-folder-name]
    (handle-move-email this message source-folder-name target-folder-name))

  (move-email-by-id [this message-id source-name target-name] (move-messages-by-id-between-category-folders this message-id source-name target-name))

  (monitor-folders [this] (let [stop-chan (async/chan)]
                            (swap! state conj {:health-chan stop-chan
                                               :folder-listener-pairs (folders->folder-message-count-listeners this)})
                            (async/go-loop []
                              (let [[_ ch] (async/alts! [(async/timeout health-check-interval)
                                                         stop-chan])]
                                (if-not (= ch stop-chan)
                                  (do (health-check-imap-folder-pairs this)
                                      (recur))
                                  (t/log! :debug ["Stopping the health checks"])))))
    (if (.connected? this) true false))

  (disconnect-and-stop-monitoring [this]
    (if (.connected? this)
      (close-and-clean-up this)
      (do (stop-health-checks this)
          (t/log! :info ["You are trying to disconnect from the connection with id" (:id this) "but it is not connected."])))))

(defn inbox-folder-name [name]
  (if (or (nil? name) (s/blank? name)) "INBOX" name))

(defn- create-idle-manager [config]
  (IdleManager. (session/config->session config) executor-service))

(defn- inbox-folder-config [config]
  (->FolderConfig (inbox-folder-name (:folder (:imap config))) :inbox nil))

(defn fcmap->folder-config [config]
  (-> (mapv (fn [[_ fcmap]] (->FolderConfig (:folder fcmap) :category (:id (first (filterv #(= (:category-id fcmap) (:id %)) (:categories config)))))) (:folder-category-map config))
      (conj (inbox-folder-config config))))

(defn create-connection
  "Creates the connection record.
  Requires a notification channel as input. Informs its caller via this channel about critical changes (such as disconnections)"
  [config context]
  (let [id (:id (:imap config))
        db (:db context)
        idle-manager (create-idle-manager config)
        store (connection-config->store config)]
    (->Connection id config (fcmap->folder-config config) idle-manager context (atom {:store store}))))
