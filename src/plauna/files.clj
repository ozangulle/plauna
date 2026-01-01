(ns plauna.files
  (:require [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [clojure.core.async :as async]
            [clojure.string :as string]
            [taoensso.telemere :as t]
            [plauna.messaging :as messaging]
            [plauna.core.events :as events])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

(def database-file "email.db")

(def plauna-config (atom nil))

(defn expand-home [^String s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (System/getProperty "user.home"))
    s))

(defn config [] @plauna-config)

(defn file-dir []
  (if-let [data-location (:data-folder (config))]
    (let [configured-location (expand-home data-location)]
      configured-location)
    (expand-home "~/.local/state/plauna")))

(defn check-and-create-database-file []
  (let [db-file (io/file (file-dir) database-file)]
    (if (.exists db-file)
      nil
      (do (.mkdirs (io/file (file-dir)))
          (.createNewFile db-file)))))

(defn delete-database-file []
  (let [db-file (io/file (file-dir) database-file)]
    (if (.exists db-file)
      (do (io/delete-file db-file)
          (io/delete-file (str db-file "-shm") true)
          (io/delete-file (str db-file "-wal") true))
      nil)))

(defn path-to-db-file []
  (str (io/file (file-dir) database-file)))

(defn training-file [language]
  (let [file (io/file (file-dir) (str "train-" language ".train"))]
    (if (.exists file)
      file
      (do (.createNewFile file)
          file))))

(defn files-with-type [type]
  (let [type-string (type {:model ".bin" :train ".train"})]
    (->> (filter #(and (.isFile ^File %)
                       (.endsWith (.getName ^File %) type-string)
                       (.startsWith (.getName ^File %) "train"))
                 (file-seq (clojure.java.io/file (file-dir))))
         (map (fn [f] (when (.isFile ^File f)
                        {:file     f
                         :language (subs (. ^File f getName) 6 9)}))))))

(defn training-files [] (files-with-type :train))

(defn model-files [] (files-with-type :model))

(defn model-file "Returns the model file for the language specified."
  [^String language] ^File (io/file (file-dir) (str "train-" language ".bin")))

(defn delete-files-with-type [type]
  (case type
    :model (doseq [file (model-files)] (io/delete-file (:file file)))
    :train (doseq [file (training-files)] (io/delete-file (:file file)))))

(defn write-to-training-file
  [language data]
  (spit (training-file language) data :append true))

(defn email-start? [line]
  (and
   (not (nil? line))
   (string/starts-with? line "From ")))

(defn read-mail-lines
  [fn sq acc]
  (loop [fn fn sq sq acc acc]
    (let [line (first sq)]
      (if (and (email-start? line) (not (nil? (peek acc))))
        (do
          (fn acc)
          (recur fn (rest sq) [line "\r\n"]))
        (if (nil? line)
          (do
            (fn acc)
            nil)
          (recur fn (rest sq) (conj acc line "\r\n")))))))

(defn read-emails-from-mbox
  "Reads the e-mails from an mbox (as input channel) and puts them in a :received-email event as byte arrays.

  Currently always adds the option :enrich"
  [mbox-is channel]
  (t/log! :info ["Starting to read from mbox"])
  (with-open [rdr (clojure.java.io/reader mbox-is)]
    (let [limiter (messaging/channel-limiter :parsed-enrichable-email)]
      (read-mail-lines
       (fn [email-string]
         (async/>!! limiter :token)
         (async/>!! channel
                    ((comp
                      (fn [mail-string] (events/create-event :received-email  mail-string {:enrich true}))
                      #(.getBytes ^String %)
                      #(apply str %)) email-string)))
       (line-seq rdr)
       [])))
  (t/log! :info ["Finished reading mbox."]))

(defn file-exists? [path] (.exists ^File (io/file path)))

(defn config-from-file [path] (if (file-exists? path)
                                (edn/read-string (slurp path))
                                (throw (t/error! (ex-info "Provided config file at does not exist. Exiting application." {:path path})))))

(defn default-config [] {:data-folder "/var/lib/plauna/"
                         :server {:port 8080}})

(def env-var-key-pairs [["--config-file" "CONFIG_FILE"] ["--data-folder" "DATA_FOLDER"] ["--server-port" "SERVER_PORT"]])

(defmulti parse-cli-arg (fn [arg] (first arg)))
(defmethod parse-cli-arg "--config-file" [arg-pair]  {:config-file (second arg-pair)})
(defmethod parse-cli-arg "--data-folder" [arg-pair] {:data-folder (second arg-pair)})
(defmethod parse-cli-arg "--server-port" [arg-pair] {:server {:port (Integer/parseInt (second arg-pair))}})
(defmethod parse-cli-arg :default [arg-pair]
  (t/log! :info ["Received non Plauna specific argument" arg-pair "- Doing nothing."])
  nil)

(defn partition-cli-args [args] (partition 2 args))

(defn aggregate-config-map
  ([acc val] (conj acc val))
  ([val] val))

(defn ^:dynamic system-env [key] (System/getenv key))

(def env-key-pair-transformation
  (comp (map (fn [pair] (if-let [val (system-env (second pair))] [(first pair) val] nil)))
        (filter some?)
        (map parse-cli-arg)))

(defn parse-config-from-cli-arguments [cli-args]
  (let [parsed-config (reduce (fn [acc val] (conj acc (parse-cli-arg val))) {} (partition-cli-args cli-args))
        env-config (transduce env-key-pair-transformation aggregate-config-map {} env-var-key-pairs)]
    (cond
      (some? (:config-file parsed-config))
      (swap! plauna-config (fn [_] (merge (default-config) env-config (config-from-file (:config-file parsed-config)))))
      (some? (:config-file env-config))
      (swap! plauna-config (fn [_] (merge (default-config) env-config (config-from-file (:config-file env-config)))))
      :else
      (swap! plauna-config (fn [_] (merge (default-config) env-config parsed-config))))))
