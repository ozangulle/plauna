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

(def custom-config-location (ref nil))

(def database-file "email.db")

(defn default-config-location [] "~/.config/plauna.edn")

(defn set-custom-config-location! [location]
  (if (some? location)
    (do (t/log! :info ["Setting custom config location to:" location])
        (dosync (alter custom-config-location (fn [_ l] l) location)))
    (t/log! :info ["No config file was supplied. Using the default config location:" (default-config-location)])))

(defn expand-home [^String s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (System/getProperty "user.home"))
    s))

(defn config-location [] (if (some? @custom-config-location)
                           (expand-home @custom-config-location)
                           (expand-home (default-config-location))))

(defn config [] (edn/read-string (slurp (config-location))))

(defn file-dir []
  (let [configured-location (expand-home (:data-folder (config)))]
    (if (some? configured-location)
      configured-location
      (expand-home "~/.local/state/plauna"))))

(defn check-and-create-database-file []
  (let [db-file (io/file (file-dir) database-file)]
    (if (.exists db-file)
      nil
      (do (.mkdirs (io/file (file-dir)))
          (.createNewFile db-file)))))

(defn delete-database-file []
  (let [db-file (io/file (file-dir) database-file)]
    (if (.exists db-file)
      (io/delete-file db-file)
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

(comment
  (t/set-min-level! :debug)
  (read-emails-from-mbox (io/input-stream "/home/ozan/gmail-backups/Archived-003.mbox") @messaging/main-chan)
  (messaging/restart-main-chan))
