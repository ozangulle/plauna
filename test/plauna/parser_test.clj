(ns plauna.parser-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [plauna.files :as files]
            [plauna.util.async :as async-utils]
            [plauna.parser :as parser]
            [clojure.core.async :refer [pub sub chan >!!] :as async]
            [plauna.analysis :as analysis]))

(defn resource->is [resource-path]
  (io/input-stream (io/resource resource-path)))

;; Testing email parsing

(deftest basic-parse-test
  (let [test-chan (chan)
        test-pub (pub test-chan :type)
        email-bytes (.getBytes ^String (slurp (io/resource "test/email_corpus/simple-lorem-ipsum.eml")))]
    (parser/listen-to-events test-pub test-chan)
    (>!! test-chan {:type :received-email :options {} :payload email-bytes})
    (let [results-chan (chan)
          _ (sub test-pub :parsed-email results-chan)
          parsed-mail (:payload (async-utils/fetch-or-timeout!! results-chan 1000))]
      (is (= "<unique_message_id@example.com>" (:message-id (:header parsed-mail))))
      (is (= "Lorem Ipsum Sample" (:subject (:header parsed-mail))))
      (is (= "text/plain" (:mime-type (first (:body parsed-mail)))))
      (is (= "Dear Test, Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed ac justo vel odio efficitur consectetur. Integer nec urna vitae elit imperdiet ultrices. Fusce vel neque vel justo dapibus luctus a eget quam. Sincerely, Tester" (:sanitized-content (first (:body parsed-mail))))))))


(deftest parse-test-2
  (let [test-chan (chan)
        test-pub (pub test-chan :type)
        email-bytes (.getBytes ^String (slurp (io/resource "test/email_corpus/greek-text.mbox")))]
    (parser/listen-to-events test-pub test-chan)
    (>!! test-chan {:type :received-email :options {} :payload email-bytes})
    (let [results-chan (chan)
          _ (sub test-pub :parsed-email results-chan)
          parsed-mail (:payload (async-utils/fetch-or-timeout!! results-chan 1000))]
      (is (= "Παράδοση" (:subject (:header parsed-mail)))))))

  ;; Normalization tests

(deftest normalization-1
  (let [res (parser/normalize (slurp (io/resource "test/normalization/original-text-1.txt")))]
    (is (= (s/trim (slurp (io/resource "test/normalization/normalized-text-1.txt"))) res))))

;; Wrong data tests

(deftest wrong-data-1
    ;; The mbox contains more than 3 e-mails. The expectation is that only the ones with proper message-id will get through.
  (let [inner-chan (chan 20)
        test-chan (pub inner-chan :type)]
    (parser/listen-to-events test-chan inner-chan)
    (files/read-emails-from-mbox (resource->is "test/email_corpus/weird-mbox.mbox") inner-chan)
    (let [results-chan (chan)]
      (sub test-chan :parsed-enrichable-email results-chan)
      (loop [event (async-utils/fetch-or-timeout!! results-chan 200) results []]
        (if (or (nil? event) (= :timed-out event))
          (is (= 3 (count results)))
          (recur (async-utils/fetch-or-timeout!! results-chan 200) (conj results event)))))))

