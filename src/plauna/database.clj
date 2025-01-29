(ns plauna.database
  (:require [clojure.java.io]
            [clojure.walk :refer [postwalk]]
            [plauna.files :as files]
            [plauna.util.async :as async-utils]
            [plauna.core.email :as core.email]
            [clojure.string :as string]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :refer [as-unqualified-lower-maps as-unqualified-kebab-maps]]
            [honey.sql :as honey]
            [honey.sql.helpers :refer [insert-into upsert values on-conflict do-update-set]]
            [next.jdbc.sql.builder :as builder]
            [plauna.util.page :as page]
            [plauna.database :as db]
            [taoensso.telemere :as t]
            [clojure.core.async :as async])
  (:import (org.flywaydb.core Flyway))
  (:gen-class))

(set! *warn-on-reflection* true)

(defn db []
  {:dbtype "sqlite"
   :dbname (files/path-to-db-file)})

(defn ds [] (jdbc/get-datasource (db)))

(defn flyway []
  (.load (doto (Flyway/configure)
           (.dataSource (ds)))))

(def my-addresses (atom #{}))

(defn add-to-my-addresses [address]
  (swap! my-addresses (fn [cur new] (conj cur new)) address))

(defn create-db []
  (.migrate ^Flyway (flyway)))

(def builder-function {:builder-fn as-unqualified-lower-maps})

(def builder-function-kebab {:builder-fn as-unqualified-kebab-maps})

;; Insert Clauses

(defn insert->insert-ignore [insert-query]
  (let [insert-part (first insert-query)]
    (conj (rest insert-query) (string/replace insert-part #"INSERT" "INSERT OR IGNORE"))))

(defn insert->insert-update [insert-query]
  (let [insert-part (first insert-query)]
    (conj (rest insert-query) (string/replace insert-part #"INSERT" "INSERT OR REPLACE"))))

(defn save-headers [headers]
  (jdbc/execute! (jdbc/get-connection (db))
                 (->>
                  (builder/for-insert-multi
                   :headers
                   [:mime_type :subject :message_id :date :in_reply_to]
                   (mapv (juxt :mime-type :subject :message-id :date :in-reply-to) headers) {})
                  (insert->insert-ignore))
                 {:batch true}))

(defn save-bodies [bodies]
  (->>
   (builder/for-insert-multi
    :bodies
    [:original_content :sanitized_content :mime_type :charset :transfer_encoding :message_id]
    (mapv (juxt :original-content :sanitized-content :mime-type :charset :transfer-encoding :message-id) bodies) {})
   (insert->insert-ignore)
   (jdbc/execute! (jdbc/get-connection (db)))))

(defn save-contacts [contacts]
  (->>
   (builder/for-insert-multi
    :contacts
    [:contact_key :name :address]
    (mapv (juxt :contact-key :name :address) contacts) {})
   (insert->insert-ignore)
   (jdbc/execute! (jdbc/get-connection (db)))))

(defn save-communications [contacts]
  (->> (builder/for-insert-multi
        :communications
        [:message_id :contact_key :type]
        (mapv (juxt :message-id :contact-key :type) contacts) {})
       (insert->insert-ignore)
       (jdbc/execute! (jdbc/get-connection (db)))))

(defn update-metadata-batch [metadata]
  (when (seq metadata)
    (->> (builder/for-insert-multi
          :metadata
          [:message_id :language :language_confidence :category :category_confidence]
          (mapv (juxt :message-id :language :language-confidence :category-id :category-confidence) metadata) {})
         (insert->insert-update)
         (jdbc/execute! (jdbc/get-connection (db))))))

(def batch-size 500)

(defn empty-buffer [] {:headers [] :bodies [] :participants [] :metadata []})

(defn add-to-buffer [e-mail buffer]
  (let [updated-buffer
        (-> (update buffer :headers conj (:header e-mail))
            (update :bodies concat (:body e-mail))
            (update :participants concat (:participants e-mail)))]
    (if (some? (:metadata e-mail))
      (update updated-buffer :metadata conj (:metadata e-mail))
      updated-buffer)))

(defn save-emails-in-buffer [buffer]
  (save-headers (:headers buffer))
  (save-bodies (:bodies buffer))
  (save-contacts (:participants buffer))
  (save-communications (:participants buffer))
  (when (seq (:metadata buffer)) (update-metadata-batch (:metadata buffer))))

(defn save-email-loop [publisher]
  (let [parsed-chan (async/chan)
        enriched-chan (async/chan)
        local-chan (async/merge [parsed-chan enriched-chan] batch-size)]
    (async/sub publisher :parsed-email local-chan)
    (async/sub publisher :enriched-email local-chan)
    (async/go-loop [event (async/<! local-chan)
                    buffer (empty-buffer)]
      (when (some? event)
        (cond
          (= :timed-out event)
          (do (t/log! :debug ["Received timeout. Saving everything in the buffer."])
              (save-emails-in-buffer buffer)
              (recur (async/<! local-chan) (empty-buffer)))

          (> (count (:headers buffer)) batch-size)
          (do (t/log! :debug ["DB buffer full. Emptying"])
              (let [updated-buffer (add-to-buffer (:payload event) buffer)]
                (save-emails-in-buffer updated-buffer))
              (recur (async/<! local-chan) (empty-buffer)))

          :else
          (recur (async-utils/fetch-or-timeout! local-chan 1000) (add-to-buffer (:payload event) buffer)))))))

(defonce year-format "STRFTIME(\"%Y\", DATETIME(date, 'unixepoch'))")

(defonce month-format "STRFTIME(\"%Y-%m\", DATETIME(date, 'unixepoch'))")

(defonce intervals
  {:yearly  year-format
   :monthly month-format})

(defonce honey-intervals
  {:yearly [:strftime "%Y" [:datetime :date "unixepoch"]]
   :monthly [:strftime "%Y-%m" [:datetime :date "unixepoch"]]})

(defn update-metadata [message_id category cat-confidence language lang-confidence]
  (jdbc/execute! (ds) (-> (insert-into :metadata)
                          (values [{:message_id          message_id
                                    :category            category
                                    :category_modified   [:strftime "%s" "now"]
                                    :category_confidence cat-confidence
                                    :language            language
                                    :language_modified   [:strftime "%s" "now"]
                                    :language_confidence lang-confidence}])
                          (upsert (-> (on-conflict :message_id)
                                      (do-update-set :category
                                                     :category_modified
                                                     :category_confidence
                                                     :language
                                                     :language_modified
                                                     :language_confidence)))
                          (honey/format))))

(defn update-metadata-category [message_id category confidence]
  (jdbc/execute! (ds) (-> (insert-into :metadata)
                          (values [{:message_id message_id :category category :category_modified [:strftime "%s" "now"] :category_confidence confidence}])
                          (upsert (-> (on-conflict :message_id)
                                      (do-update-set :category :category_modified :category_confidence)))
                          (honey/format))))

(defn get-categories []
  (jdbc/execute! (ds) (honey/format {:select [:*] :from :categories}) builder-function))

(defn create-category [category]
  (jdbc/execute! (ds) (honey/format {:insert-into :categories :columns [:name] :values [[category]]})))

(defn delete-category-by-id [id]
  (jdbc/execute! (ds) (honey/format {:delete-from :categories :where [:= :id id]})))

(defn category-by-name [category-name]
  (jdbc/execute-one! (ds) (honey/format {:select [:*] :from :categories :where [:= :name category-name]}) builder-function))

(defn get-languages []
  (jdbc/execute! (ds) ["select language from metadata group by language"] builder-function))

(defn get-language-preferences []
  (jdbc/execute! (ds) ["select * from category_training_preferences"] builder-function))

(defn get-activated-language-preferences []
  (jdbc/execute! (ds) ["select * from category_training_preferences where use_in_training = 1"] builder-function))

(defn add-language-preferences [preferences]
  (jdbc/execute! (ds) (insert->insert-ignore (honey/format {:insert-into :category-training-preferences :columns [:language :use-in-training] :values preferences}))))

(defn update-language-preference [preference]
  (jdbc/execute! (ds) ["UPDATE category_training_preferences SET use_in_training = ?  WHERE id = ?" (:use preference) (:id preference)] builder-function))

(defn years-of-data []
  (mapv :year (jdbc/execute! (ds) [(str "SELECT " year-format " as year FROM headers WHERE date IS NOT NULL GROUP BY year ORDER BY year;")] builder-function)))

;;;;;;;;;;;;;; Refactored call stuff

(defn headers-for-strict-options [strict]
  (if strict
    "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id"
    "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers LEFT JOIN metadata ON headers.message_id = metadata.message_id"))

(defn body-parts-for-options [] "SELECT * FROM bodies INNER JOIN metadata ON metadata.message_id = bodies.message_id")

(defn participants [] "SELECT * FROM communications INNER JOIN contacts ON communications.contact_key = contacts.contact_key")

(defn interval-for [key] (get intervals key :yearly))

(defn interval-for-honey [key] (get honey-intervals key :yearly))

(defn convert-to-count [sql-result entity]
  (let [sql (first sql-result)
        to-format (string/replace (string/replace sql #"SELECT .* FROM" "SELECT COUNT(%s) as count FROM") #"ORDER.*$" "")]
    (cond (= entity :enriched-email) (flatten [(format to-format "headers.message_id") (rest sql-result)])
          (= entity :body-part) (flatten [(format to-format "bodies.message_id") (rest sql-result)]))))

(def key-lookup {:message-id :headers.message_id
                 :date :headers.date
                 :category :metadata.category
                 :language :metadata.language
                 :category-modified :metadata.category_modified})

(defn change-important-keys [key]
  (let [lookup (get key-lookup key)]
    (if (nil? lookup)
      key
      lookup)))

(defmulti data->sql :entity)

(defmethod data->sql :body-part [_ sql-clause]
  (let [jdbc-sql (honey/format (postwalk change-important-keys sql-clause))]
    (flatten [(str (body-parts-for-options) " " (first jdbc-sql)) (rest jdbc-sql)])))

(defmethod data->sql :enriched-email
  ([entity-clause sql-clause]
   (let [strict (:strict entity-clause)
         jdbc-sql (honey/format (postwalk change-important-keys sql-clause))]
     (flatten [(str (headers-for-strict-options strict) " " (first jdbc-sql)) (rest jdbc-sql)])))
  ([entity-clause]
   (let [strict (:strict entity-clause)]
     [(headers-for-strict-options strict)])))

(defmethod data->sql :participant [_ sql-clause]
  (let [first-part  {:select [:communications.contact-key :message-id :type :name :address] :from [:communications] :join [:contacts [:= :contacts.contact-key :communications.contact-key]]}]
    (->> (conj first-part sql-clause)
         honey/format)))

(defn fetch-headers [entity-clause sql-clause] (jdbc/execute! (ds) (data->sql entity-clause sql-clause) builder-function-kebab))

(defn fetch-metadata [message-id] (jdbc/execute-one! (ds) ["SELECT message_id, language, language_modified, language_confidence, metadata.category AS category_id, category_modified, category_confidence, categories.name AS category FROM metadata LEFT JOIN categories ON metadata.category = categories.id WHERE metadata.message_id = ?" message-id] builder-function-kebab))

(defn fetch-bodies [message-id] (jdbc/execute! (ds) ["SELECT * FROM bodies WHERE message_id = ?" message-id] builder-function-kebab))

(defn fetch-participants [message-id] (jdbc/execute! (ds) ["SELECT * FROM communications LEFT JOIN contacts ON contacts.contact_key = communications.contact_key WHERE message_id = ? " message-id] builder-function-kebab))

(defn db->metadata [db-metadata] (apply core.email/->Metadata ((juxt :message-id :language :language-modified :language-confidence :category :category-id :category-modified :category-confidence) db-metadata)))

(defn related-data-to-header [header]
  (let [message-id (:message-id header)
        metadata (db->metadata (fetch-metadata message-id))
        bodies (map core.email/construct-body-part (fetch-bodies message-id))
        participants (map core.email/construct-participants (fetch-participants message-id))]
    (core.email/->EnrichedEmail header bodies participants metadata)))

(defmulti fetch-data (fn [options _] (:entity options)))

(defmethod fetch-data :body-part [entity-clause sql-clause]
  (if (nil? (:page entity-clause))
    (let [result (jdbc/execute! (ds) (data->sql entity-clause sql-clause) builder-function-kebab)]
      (map (fn [el] (core.email/->EnrichedBodyPart (core.email/construct-body-part el) (core.email/map->Metadata el))) result))
    (let [limit-offset (page/page-request->limit-offset (:page entity-clause))
          sql-clause-with-limit-offset (conj sql-clause limit-offset)
          result (jdbc/execute! (ds) (data->sql entity-clause sql-clause-with-limit-offset) builder-function-kebab)
          data (map (fn [el] (core.email/->EnrichedBodyPart (core.email/construct-body-part el) (core.email/map->Metadata el))) result)]
      {:data  data
       :size  (count data)
       :page  (inc (quot (:offset limit-offset) (:limit limit-offset)))
       :total (:count (jdbc/execute-one! (ds) (convert-to-count (data->sql entity-clause sql-clause) (:entity entity-clause)) builder-function-kebab))})))

(defmethod fetch-data :enriched-email [entity-clause sql-clause]
  (if (nil? (:page entity-clause))
    (map related-data-to-header (map core.email/construct-header (fetch-headers entity-clause sql-clause)))
    (let [limit-offset (page/page-request->limit-offset (:page entity-clause))
          sql-clause-with-limit-offset (conj sql-clause limit-offset)
          data (map related-data-to-header (map core.email/construct-header (fetch-headers entity-clause sql-clause-with-limit-offset)))]
      {:data  data
       :size  (count data)
       :page  (inc (quot (:offset limit-offset) (:limit limit-offset)))
       :total (:count (jdbc/execute-one! (ds) (convert-to-count (data->sql entity-clause sql-clause) (:entity entity-clause)) builder-function-kebab))})))

(defmethod fetch-data :participant [entity-clause sql-clause]
  (map core.email/map->Participant (jdbc/execute! (ds) (data->sql entity-clause sql-clause) builder-function-kebab)))

(defn yearly-email-stats []
  (jdbc/execute! (ds) [(str "SELECT COUNT(message_id) AS count, " (interval-for :yearly) " AS date FROM headers WHERE date IS NOT NULL GROUP BY " (interval-for :yearly))]))

(defn query-db [honeysql-query]
  (jdbc/execute! (ds) (honey/format honeysql-query) builder-function-kebab))

(defn update-preference [preference value]
  (jdbc/execute! (ds)
                 (-> {:insert-into [:preferences]
                      :columns     [:preference :value]
                      :values      [[(name preference) value]]}
                     (honey/format)
                     (insert->insert-update)) builder-function-kebab))

(defn fetch-preference [preference]
  (let [result (jdbc/execute-one! (ds)
                                  (honey/format {:select [:value]
                                                 :from [:preferences]
                                                 :where [:= :preference (name preference)]}) builder-function-kebab)]
    (when (some? result) (:value result))))
