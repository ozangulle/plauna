(ns plauna.database-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async]
            [plauna.database :as db]
            [plauna.files :as files]
            [clojure.java.io :as io]))

(defn setup-clean-db [f]
  (files/set-custom-config-location! (.getPath (io/resource "test/test.edn")))
  (files/check-and-create-database-file)
  (db/create-db)
  (alter-var-root #'db/batch-size (fn [_] 2))
  (f)
  (files/delete-database-file))

(use-fixtures :once setup-clean-db)

(deftest save-email-batch
  (let [example {:type :parsed-email :payload {:header {:message-id "test" :date 0 :subject "Test" :in-reply-to nil :mime-type "text/plain"} :body [{:message-id "test" :mime-type "text/plain" :charset "fake" :transfer-encoding "fake" :content "Test" :sanitized-content "Test"}] :participants [{:type :sender :message-id "test" :name "fake" :address "fake" :contact-key "fake"} {:type :receiver :message-id "test" :name "fake" :address "fake" :contact-key "fake"}]}}
        to-insert (repeatedly 6 (fn [] example))
        test-channel (async/chan)
        test-publisher (async/pub test-channel :type)]
    (db/database-event-loop test-publisher)
    (doseq [test-event to-insert] (async/>!! test-channel test-event))
    (Thread/sleep 1000)
    (async/close! test-channel)
    (println "Done")))

(deftest enriched-email-simple
  (let [sql (db/data->sql {:entity :enriched-email :strict false})]
    (is (=  "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers LEFT JOIN metadata ON headers.message_id = metadata.message_id"
            (first sql)))))

(deftest enriched-email-simple-2
  (let [sql (db/data->sql {:entity :enriched-email :strict true})]
    (is (=  "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id"
            (first sql)))))

(deftest enriched-email-simple-3
  (let [sql (db/data->sql {:entity :enriched-email :strict true} {:where [:= :message-id "123"]})]
    (is (=  "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id WHERE headers.message_id = ?"
            (first sql)))))

(deftest enriched-email-simple-4
  (let [sql (db/data->sql {:entity :enriched-email :strict true} {:where [:and [:= :message-id "123"] [:<> :language nil] [:<> :category nil]]})]
    (is (= "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id WHERE (headers.message_id = ?) AND (metadata.language IS NOT NULL) AND (metadata.category IS NOT NULL)"
           (first sql)))))

