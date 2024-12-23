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
   [taoensso.telemere :as telemere]
   [plauna.parser :as parser]
   [clojure.core.async :as async])
  (:gen-class))

(telemere/streams->telemere!)
(telemere/set-min-level! :info)
(telemere/set-min-level! :slf4j "org.eclipse.jetty.server.*" :error)

(set! *warn-on-reflection* true)

(comment
  (require '[clojure.pprint :as pp]
           '[clojure.main :as main])
  (add-tap (bound-fn* pp/pprint))
  ,)

(defmulti parse-cli-arg (fn [arg] (first (s/split arg #"="))))
(defmethod parse-cli-arg "--config-file" [arg-string] {:config-file (second (s/split arg-string #"="))})

(defn check-if-ml-models-exist
  "Checks whether the ML models for categorization exist. Returns true, if they do.
  If it fails to find any models, returns false."
  []
  (let [ml-models-exist (not-empty (files/model-files))]
    (cond
      (nil? ml-models-exist) (do (telemere/log! :error "No ml models exist for categorization. E-mails cannot be categorized.") false)
      :else true)))

(defn start-imap-client
  "Starts the imap clients in order to listen to incoming e-mails, categorize them and refile them.
  It does not start, if 'check-if-ml-models-exist' check fails."
  [config]
  (if (= (check-if-ml-models-exist) true)
    (let [listen-channel (chan 10)]
      (doseq [client-config (:clients (:email config))]
        (client/initialize-client-setup! client-config)
        (client/create-folder-monitor client-config listen-channel))
      (telemere/log! :debug "Listening to new emails from listen-channel"))
    (telemere/log! :info "Not starting imap client due to missing files.")))

(comment
  (parser/listen-to-events @messaging/main-publisher @messaging/main-chan)
  (database/save-email-loop @messaging/main-publisher)
  (client/client-loop @messaging/main-publisher)
  (analysis/enrichment-loop @messaging/main-publisher @messaging/main-chan)
  (async/go (files/read-emails-from-mbox (clojure.java.io/input-stream "/home/ozan/gmail-backups/Archived-003.mbox") @messaging/main-chan))
  (server/start-server {:server {:port 8080}})
  (messaging/restart-main-chan)
  (server/stop-server)
  ,)

(defn -main
  [& args]
  (let [parsed-config (reduce (fn [acc val] (conj acc (parse-cli-arg val))) {} args)]
    (files/set-custom-config-location! (:config-file parsed-config))
    (files/check-and-create-database-file)
    (doseq [address (:addresses (:email (files/config)))] (database/add-to-my-addresses address))
    (database/create-db)
    (start-imap-client (files/config))
    (parser/listen-to-events @messaging/main-publisher @messaging/main-chan)
    (database/save-email-loop @messaging/main-publisher)
    (client/client-loop @messaging/main-publisher)
    (analysis/enrichment-loop @messaging/main-publisher @messaging/main-chan)
    (server/start-server (files/config))))

(comment
    (doseq [address (:addresses (:email (files/config)))] (database/add-to-my-addresses address))
    (-main)
  ,)

