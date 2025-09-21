(ns plauna.entry
  (:require
   [plauna.files :as files]
   [plauna.server :as server]
   [plauna.client :as client]
   [plauna.messaging :as messaging]
   [plauna.analysis :as analysis]
   [taoensso.telemere :as t]
   [plauna.parser :as parser]
   [plauna.core.events :as events]
   [plauna.preferences :as preferences]
   [plauna.database :as db]
   [plauna.core.email :as core.email])
  (:gen-class))

(defn setup-logging []
  (t/set-min-level! :info)
  ;; jetty is very noisy. Disable all jetty logs.
  (t/set-ns-filter! {:disallow "org.eclipse.jetty.*"}))

(set! *warn-on-reflection* true)

(def event-register {:enrichment-event-loop (fn [] (analysis/enrichment-event-loop @messaging/main-publisher @messaging/main-chan))
                     :client-event-loop (fn [] (client/client-event-loop @messaging/main-publisher))
                     :database-event-loop (fn [] (db/database-event-loop @messaging/main-publisher))
                     :parser-event-loop (fn [] (parser/parser-event-loop @messaging/main-publisher @messaging/main-chan))})

(defn start-imap-client
  [config]
  (let [connections-in-db (db/get-connections)]
    (if (seq connections-in-db)
      (do (t/log! :debug ["Connections table contains" (count connections-in-db) "connection configuration(s)."])
          (doseq [client-config connections-in-db]
            (-> (client/connect client-config)
                (client/create-category-folders! (mapv :name (db/get-categories))))))
      (do (t/log! :debug "Connections table in the db is empty. Trying to read connections from the config file.")
          (doseq [client-config (:clients (:email config))]
            (t/log! :info ["Adding connection data from the config file to the database. Next time Plauna will use the data from the database."])
            (let [connection-with-id (core.email/construct-imap-connection-from-config-file (conj client-config {:id (client/id-from-config client-config)}))]
              (db/add-connection connection-with-id)
              (-> (client/connect connection-with-id)
                  (client/create-category-folders! (mapv :name (db/get-categories)))))))))
  (t/log! :debug "Listening to new emails from listen-channel"))

(defn -main
  [& args]
  (setup-logging)
  (let [application-config (files/parse-config-from-cli-arguments args)]
    (files/check-and-create-database-file)
    (db/create-db)
    (t/log! :info "Setting log level according to preferences.")
    (t/set-min-level! (preferences/log-level))
    (start-imap-client application-config)
    (events/start-event-loops event-register)
    (server/start-server application-config)))
