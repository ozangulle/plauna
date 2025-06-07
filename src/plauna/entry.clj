(ns plauna.entry
  (:require
   [plauna.files :as files]
   [plauna.server :as server]
   [plauna.client :as client]
   [plauna.database :as database]
   [plauna.messaging :as messaging]
   [plauna.analysis :as analysis]
   [taoensso.telemere :as t]
   [plauna.parser :as parser]
   [plauna.core.events :as events]
   [plauna.preferences :as preferences])
  (:gen-class))

(defn setup-logging []
  (t/set-min-level! :info)
  ;; jetty is very noisy. Disable all jetty logs.
  (t/set-ns-filter! {:disallow "org.eclipse.jetty.*"}))

(set! *warn-on-reflection* true)

(def event-register {:enrichment-event-loop (fn [] (analysis/enrichment-event-loop @messaging/main-publisher @messaging/main-chan))
                     :client-event-loop (fn [] (client/client-event-loop @messaging/main-publisher))
                     :database-event-loop (fn [] (database/database-event-loop @messaging/main-publisher))
                     :parser-event-loop (fn [] (parser/parser-event-loop @messaging/main-publisher @messaging/main-chan))})

(defn start-imap-client
  [config]
  (doseq [client-config (:clients (:email config))]
    (-> (client/create-imap-monitor client-config)
        (client/create-category-folders! (mapv :name (database/get-categories)))
        (client/start-monitoring)
        (client/schedule-health-checks)))
  (t/log! :debug "Listening to new emails from listen-channel"))

(defn -main
  [& args]
  (setup-logging)
  (let [application-config (files/parse-config-from-cli-arguments args)]
    (files/check-and-create-database-file)
    (database/create-db)
    (t/log! :info "Setting log level according to preferences.")
    (t/set-min-level! (preferences/log-level))
    (start-imap-client application-config)
    (events/start-event-loops event-register)
    (server/start-server application-config)))

(comment (server/stop-server))
