(ns plauna.client
  (:require
   [plauna.client.connection :as imap-conn]
   [plauna.core.common-records :as records]
   [plauna.interfaces :as int]
   [taoensso.telemere :as t])
  (:import
   [plauna.interfaces IMAPConnection]))

(set! *warn-on-reflection* true)

(defonce connections (atom {}))

(defn get-connection [id] (get @connections id))

(def type-check-imap-connection
  ;; These fields used to optional in the configuration. Now we need to make sure that they are set properly.
  (comp (fn [connection] (update connection :check-ssl-certs #(or (nil? %) (= % true))))
        (fn [connection] (update connection :debug (fn [x] (if (nil? x) false x))))
        (fn [connection] (update connection :security (fn [x] (if (nil? x) "ssl" x))))))

(defn- construct-imap-connection-from-config-file [data-map]
  (cond (and (some? (:host data-map))
             (some? (:user data-map))
             (some? (:secret data-map))
             (some? (:folder data-map)))
        (records/map->ImapConnection (type-check-imap-connection data-map))))

(defn create-connection-from-config-and-start-watching [client-config context]
  (let [config (construct-imap-connection-from-config-file client-config)
        imap-connection (imap-conn/create-connection config context)]
    (swap! connections assoc (:id imap-connection) imap-connection)
    (int/connect imap-connection)
    (int/monitor-folders imap-connection)))

(defn connection-information [id context]
  (let [conn (int/fetch-connection (:db context) id)]
    (if-let [connection (get-connection id)]
      (merge conn {:connected (int/connected? connection)})
      (merge conn {:connected false}))))

(defn connection-folders [connection-config]
  (let [conn (get-connection (:id connection-config))]
    (if (nil? conn)
      []
      (if (true? (int/connected? conn))
        (int/list-folders conn)
        []))))

(defn restructure-fcmaps [fcmaps]
  (reduce (fn [acc fcmp] (assoc acc (:folder fcmp) fcmp)) {} fcmaps))

(defn connection-config
  "Returns nil if the connection is not found."
  [id context]
  (let [db (:db context)
        conn-info (connection-information id context)
        providers (int/fetch-auth-providers db)
        categories (int/fetch-categories db)
        folder-category-map (restructure-fcmaps (int/fetch-folder-category-maps db id))]
    (if (nil? (:host conn-info))
      nil
      {:imap (assoc conn-info :auth-providers providers) :folders (mapv str (connection-folders conn-info)) :categories categories :folder-category-map folder-category-map})))

(defn start-imap-connections
  [context]
  (let [connections-in-db (int/fetch-connections (:db context))]
    (doseq [raw-connection connections-in-db]
      (let [full-config (connection-config (:id raw-connection) context)
            connection ^IMAPConnection (imap-conn/create-connection full-config context)]
        (swap! connections assoc (-> full-config :imap :id) connection)
        (.connect connection)
        (.monitor-folders connection))))
  (t/log! :debug "Listening to new emails from listen-channel"))

(defn edit-fcmap-in-connection
  "Error means an entity could not be found."
  [connection-id fcmap context]
  (let [fcmaps (restructure-fcmaps (int/fetch-folder-category-maps (:db context) connection-id))
        categories (int/fetch-categories (:db context))
        folders (.list-folders ^IMAPConnection (get-connection connection-id))
        found-fcmap (get fcmaps (:folder fcmap))]
    (cond
      (nil? (seq (filter #(= (:category-id fcmap) (:id %)) categories)))
      {:result :error :message "Category could not be found"}
      (not (seq fcmaps))
      {:result :error :message "No folder category map for this connection-id"}
      (nil? found-fcmap)
      {:result :error :message "No folder category map for this folder"}
      (= (:id found-fcmap) (:id fcmap))
      (do (int/save-folder-category-map (:db context) (assoc fcmap :connection-id connection-id))
          {:result :success})
      :else
      {:result :error :message "id could not be found"})))
