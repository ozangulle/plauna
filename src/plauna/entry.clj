(ns plauna.entry
  (:require
   [clojure.string :as s]
   [clojure.core.async :refer [chan] :as async]
   [plauna.files :as files]
   [plauna.server :as server]
   [plauna.client :as client]
   [plauna.database :as database]
   [plauna.messaging :as messaging]
   [plauna.analysis :as analysis]
   [taoensso.telemere :as t]
   [taoensso.telemere.streams :as tstreams]
   [plauna.parser :as parser]
   [plauna.core.events :as events]
   [plauna.preferences :as preferences])
  (:gen-class))

(t/set-min-level! :slf4j "org.eclipse.jetty.server.*" :error)
(tstreams/streams->telemere!)

(set! *warn-on-reflection* true)

(defmulti parse-cli-arg (fn [arg] (first (s/split arg #"="))))
(defmethod parse-cli-arg "--config-file" [arg-string] {:config-file (second (s/split arg-string #"="))})

(def event-register {:enrichment-event-loop (fn [] (analysis/enrichment-event-loop @messaging/main-publisher @messaging/main-chan))
                     :client-event-loop (fn [] (client/client-event-loop @messaging/main-publisher))
                     :database-event-loop (fn [] (database/database-event-loop @messaging/main-publisher))
                     :parser-event-loop (fn [] (parser/parser-event-loop @messaging/main-publisher @messaging/main-chan))})

(defn start-imap-client
  [config]
  (let [listen-channel (chan 10)]
    (doseq [client-config (:clients (:email config))]
      (client/initialize-client-setup! client-config)
      (client/create-folder-monitor client-config listen-channel))
    (t/log! :debug "Listening to new emails from listen-channel")))

(defn -main
  [& args]
  (let [parsed-config (reduce (fn [acc val] (conj acc (parse-cli-arg val))) {} args)]
    (t/set-min-level! (preferences/log-level))
    (files/set-custom-config-location! (:config-file parsed-config))
    (files/check-and-create-database-file)
    (doseq [address (:addresses (:email (files/config)))] (database/add-to-my-addresses address))
    (database/create-db)
    (start-imap-client (files/config))
    (events/start-event-loops event-register)
    (server/start-server (files/config))))
