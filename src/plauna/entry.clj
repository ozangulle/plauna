(ns plauna.entry
  (:require
   [clojure.string :as s]
   [clojure.core.async :refer [chan]]
   [plauna.files :as files]
   [plauna.server :as server]
   [plauna.client :as client]
   [plauna.database :as database]
   [plauna.messaging :as messaging]
   [plauna.analysis :as analysis]
   [taoensso.telemere :as t]
   [taoensso.telemere.streams :as tstreams]
   [plauna.parser :as parser])
  (:gen-class))

(tstreams/streams->telemere!)
(t/set-min-level! :info)
(t/set-min-level! :slf4j "org.eclipse.jetty.server.*" :error)

(set! *warn-on-reflection* true)

(defmulti parse-cli-arg (fn [arg] (first (s/split arg #"="))))
(defmethod parse-cli-arg "--config-file" [arg-string] {:config-file (second (s/split arg-string #"="))})

(defn start-imap-client
  [config]
  (let [listen-channel (chan 10)]
    (doseq [client-config (:clients (:email config))]
      (client/initialize-client-setup! client-config)
      (client/create-folder-monitor client-config listen-channel))
    (t/log! :debug "Listening to new emails from listen-channel")))

(defn start-event-loops [main-publisher main-channel]
  (parser/parser-event-loop main-publisher main-channel)
  (database/database-event-loop main-publisher)
  (client/client-event-loop main-publisher)
  (analysis/enrichment-event-loop main-publisher main-channel))

(defn -main
  [& args]
  (let [parsed-config (reduce (fn [acc val] (conj acc (parse-cli-arg val))) {} args)]
    (files/set-custom-config-location! (:config-file parsed-config))
    (files/check-and-create-database-file)
    (doseq [address (:addresses (:email (files/config)))] (database/add-to-my-addresses address))
    (database/create-db)
    (start-imap-client (files/config))
    (start-event-loops @messaging/main-publisher @messaging/main-chan)
    (server/start-server (files/config))))
