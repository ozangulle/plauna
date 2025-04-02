(ns plauna.entry
  (:require
   [clojure.core.async :refer [chan] :as async]
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
  (t/set-min-level! :slf4j "org.eclipse.jetty.server.*" :error))

(comment (t/set-min-level! :debug))

(set! *warn-on-reflection* true)

(def event-register {:enrichment-event-loop (fn [] (analysis/enrichment-event-loop @messaging/main-publisher @messaging/main-chan))
                     :client-event-loop (fn [] (client/client-event-loop @messaging/main-publisher))
                     :database-event-loop (fn [] (database/database-event-loop @messaging/main-publisher))
                     :parser-event-loop (fn [] (parser/parser-event-loop @messaging/main-publisher @messaging/main-chan))})

(defn start-imap-client
  [config]
  (let [listen-channel (chan 10)]
    (doseq [client-config (:clients (:email config))]
      (let [store (client/create-folder-monitor client-config listen-channel)]
        (client/create-imap-directories! store)
        (client/check-necessary-capabilities store)))
    (t/log! :debug "Listening to new emails from listen-channel")))

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
