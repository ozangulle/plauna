(ns plauna.client
  (:require
   [plauna.imap.connection :as imap-conn]
   [plauna.interfaces :as int]
   [taoensso.telemere :as t])
  (:import
   [plauna.interfaces IMAPConnection]))

(set! *warn-on-reflection* true)

(defonce connections (atom {}))

(defn get-connection [id] (get @connections id))

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
  (reduce (fn [acc fcmp] (assoc acc (:category-id fcmp) fcmp)) {} fcmaps))

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
        found-fcmap (get fcmaps (:category-id fcmap))]
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
