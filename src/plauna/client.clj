(ns plauna.client
  (:require
   [plauna.client.connection :as imap-conn]
   [plauna.core.common-records :as records]
   [plauna.interfaces :as int]))

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

(defn- connection-information [id context]
  (let [conn (int/fetch-connection (:db context) id)]
    (merge conn {:connected (int/connected? (get-connection id))})))

(defn- connection-folders [connection-config]
  (let [conn (get-connection (:id connection-config))]
    (if (true? (int/connected? conn))
      (int/list-folders conn)
      [])))

(defn connection-config [id context]
  (let [db (:db context)
        conn-info (connection-information id context)
        providers (int/fetch-auth-providers db)
        categories (int/fetch-categories db)
        folder-category-map (int/fetch-folder-category-maps db id)]
    {:imap (assoc conn-info :auth-providers providers) :folders (mapv str (connection-folders conn-info)) :categories categories :folder-category-map folder-category-map}))
