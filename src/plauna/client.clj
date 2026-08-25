(ns plauna.client
  (:require
   [clojure.core.async :as async]
   [plauna.client.connection :as imap-conn]
   [plauna.core.common-records :as records]
   [plauna.interfaces :as int]
   [taoensso.telemere :as t]))

(set! *warn-on-reflection* true)

(defonce connections (atom {}))

(defn get-connections [] (vals @connections))

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
        notification-channel (async/chan)
        imap-connection (imap-conn/create-connection config context notification-channel)]
    (swap! connections assoc (:id imap-connection) imap-connection)
    (int/connect imap-connection)
    (int/monitor-folders imap-connection)
    (async/go-loop []
      (let [event (async/<! notification-channel)]
        (t/log! :info ["Connection" (:id imap-connection) "returned the following event:" event])
        (cond (= :disconnected event)
              (do
                (try
                  (let [new-imap-connection ^plauna.interfaces.IMAPConnection (imap-conn/create-connection config context notification-channel)]
                    (swap! connections assoc (:id new-imap-connection) new-imap-connection)
                    (int/connect new-imap-connection)
                    (int/monitor-folders new-imap-connection))
                  (catch Exception e (t/log! :error e)))
                (recur))
              (= :timeout event)
              (do (Thread/sleep 60000)
                  (let [current-connection (get-connection (:id imap-connection))]
                    (int/connect current-connection)
                    (int/monitor-folders current-connection)
                    (recur))))))))

(defn restart-connection
  "Returns true if it was successful or false"
  [id]
  (let [connection ^plauna.interfaces.IMAPConnection (get-connection id)]
                                         (.disconnect-and-stop-monitoring connection)
                                         (let [[v c] (async/alts!! [(:notification-channel connection)
                                                                    (async/timeout 5000)])]
                                           (t/log! :info ["Reconnect operation returned" v "from the channel" c])
                                           (if (= c (:notification-channel connection))
                                             (do (.connect connection)
                                                 true)
                                             false))))
