(ns plauna.entry
  (:require
   [plauna.analysis :as analysis]
   [plauna.client :as client]
   [plauna.database :as db]
   [plauna.files :as files]
   [plauna.preferences :as preferences]
   [plauna.server :as server]
   [taoensso.telemere :as t])
  (:import
   [plauna.database SqliteDB]
   [plauna.analysis BasicAnalyzer])
  (:gen-class))

(defn setup-logging []
  (t/set-min-level! :info)
  ;; jetty is very noisy. Disable all jetty logs.
  (t/set-ns-filter! {:disallow "org.eclipse.jetty.*"}))

(set! *warn-on-reflection* true)

(defn start-imap-client
  [context]
  (let [connections-in-db (db/get-connections)]
    (doseq [client-config connections-in-db]
      (client/create-connection-from-config-and-start-watching client-config context)))
  (t/log! :debug "Listening to new emails from listen-channel"))

(defn -main
  [& args]
  (setup-logging)
  (let [application-config (files/parse-config-from-cli-arguments args)
        context {:config application-config :db (SqliteDB.) :analyzer (BasicAnalyzer.)}]
    (files/check-and-create-database-file)
    (db/create-db)
    (t/log! :info "Setting log level according to preferences.")
    (t/set-min-level! (preferences/log-level))
    (start-imap-client context)
    (server/start-server context)))

(comment
  (-main)
  (server/start-server {:config {:server {:port 8080}}})
  (server/stop-server)
  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)
  (client/disconnect-all))
