(ns plauna.database-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async]
            [plauna.database :as db]
            [taoensso.telemere :as t]
            [plauna.files :as files]
            [clojure.test.check.generators :as gen])
  (:import [plauna.database SqliteDB]))

(t/set-min-level! :error)

(defn setup-clean-db [f]
  (swap! files/plauna-config (fn [_] {:data-folder "tmp/"}))
  (files/check-and-create-database-file)
  (db/create-db)
  (alter-var-root #'db/batch-size (fn [_] 2))
  (f)
  (files/delete-database-file))

(use-fixtures :each setup-clean-db)

(def db-instance (SqliteDB.))

(deftest save-email-batch
  (let [example {:type :parsed-email :payload {:header {:message-id "test" :date 0 :subject "Test" :in-reply-to nil :mime-type "text/plain"} :body [{:message-id "test" :mime-type "text/plain" :charset "fake" :transfer-encoding "fake" :content "Test" :sanitized-content "Test"}] :participants [{:type :sender :message-id "test" :name "fake" :address "fake" :contact-key "fake"} {:type :receiver :message-id "test" :name "fake" :address "fake" :contact-key "fake"}]}}
        to-insert (repeatedly 6 (fn [] example))
        test-channel (async/chan)
        test-publisher (async/pub test-channel :type)]
    (db/database-event-loop test-publisher)
    (doseq [test-event to-insert] (async/>!! test-channel test-event))
    (Thread/sleep 1000)
    (async/close! test-channel)))

(deftest enriched-email-simple
  (let [sql (db/data->sql {:entity :enriched-email :strict false})]
    (is (= "SELECT DISTINCT headers.message_id, in_reply_to, subject, headers.mime_type, date FROM headers LEFT JOIN metadata ON headers.message_id = metadata.message_id LEFT JOIN communications ON communications.message_id = headers.message_id LEFT JOIN contacts ON contacts.contact_key = communications.contact_key LEFT JOIN bodies ON bodies.message_id = headers.message_id"
           (first sql)))))

(deftest enriched-email-simple-2
  (let [sql (db/data->sql {:entity :enriched-email :strict true})]
    (is (= "SELECT DISTINCT headers.message_id, in_reply_to, subject, headers.mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id INNER JOIN communications ON communications.message_id = headers.message_id INNER JOIN contacts ON contacts.contact_key = communications.contact_key INNER JOIN bodies ON bodies.message_id = headers.message_id"
           (first sql)))))

(deftest enriched-email-simple-3
  (let [sql (db/data->sql {:entity :enriched-email :strict true} {:where [:= :message-id "123"]})]
    (is (= "SELECT DISTINCT headers.message_id, in_reply_to, subject, headers.mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id INNER JOIN communications ON communications.message_id = headers.message_id INNER JOIN contacts ON contacts.contact_key = communications.contact_key INNER JOIN bodies ON bodies.message_id = headers.message_id WHERE headers.message_id = ?"
           (first sql)))))

(deftest enriched-email-simple-4
  (let [sql (db/data->sql {:entity :enriched-email :strict true} {:where [:and [:= :message-id "123"] [:<> :language nil] [:<> :category nil]]})]
    (is (= "SELECT DISTINCT headers.message_id, in_reply_to, subject, headers.mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id INNER JOIN communications ON communications.message_id = headers.message_id INNER JOIN contacts ON contacts.contact_key = communications.contact_key INNER JOIN bodies ON bodies.message_id = headers.message_id WHERE (headers.message_id = ?) AND (metadata.language IS NOT NULL) AND (metadata.category IS NOT NULL)"
           (first sql)))))

(deftest fcp-data-integrity-tests
  (let [connection-id (str (first (gen/sample gen/uuid)))]
    (db/add-connection {:id connection-id :host "bla" :user "bla" :secret "bla" :folder "" :security "ssl"})
    (.save_category db-instance "newsletter")
    
    (testing "Wrong connection id throws an exception on save"
      (let [fcp {:connection-id "wrong-id" :folder "newsletters" :category-id 1}]
        (is (thrown? org.sqlite.SQLiteException (.save-folder-category-map db-instance fcp)))))

    (testing "Wrong category id throws an exception on save"
      (let [fcp {:connection-id connection-id :folder "newsletters" :category-id 2}]
        (is (thrown? org.sqlite.SQLiteException (.save-folder-category-map db-instance fcp)))))

    (testing "fcp can be saved with correct foreign keys"
      (let [fcp {:connection-id connection-id :folder "newsletters" :category-id 1}
            result (.save-folder-category-map db-instance fcp)]
        (is (= [#:next.jdbc{:update-count 1}] result))
        (is (= (dissoc (first (.fetch-folder-category-maps db-instance connection-id)) :id) fcp))))))
