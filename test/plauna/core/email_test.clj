(ns plauna.core.email-test
  (:require [clojure.test :refer :all]
            [plauna.core.email :as core.email]))

  (defn create-mock-header []
        (core.email/->Header "test123" nil "test subject" "multipart" 1123))

  (defn create-body-part [mime-type content]
        (core.email/->Body-Part "test123" "utf-8" mime-type "test" content content nil))

  (defn create-email-with-fake-body [mimetype-content-tuples]
    (core.email/->Email (create-mock-header) (map (fn [tuple] (create-body-part (first tuple) (second tuple))) mimetype-content-tuples) []))

  (deftest training-content-single-body-part
           (let [email (core.email/->Email (create-mock-header)
                                           [(create-body-part "text/html" "Simple content")]
                                           [])
                 training-data (core.email/training-content "text/html" email)]
             (testing
               (is (= (:message-id training-data) "test123"))
               (is (= (:training-content training-data) "Simple content")))))

(deftest training-content-single-body-part-wrong-mime-type
  (let [email (core.email/->Email (create-mock-header)
                                  [(create-body-part "text/html" "Simple content")]
                                  [])
        training-data (core.email/training-content "text/text" email)]
    (testing
      (is (= (:message-id training-data) "test123"))
      (is (= (:training-content training-data) "Simple content")))))

(deftest training-content-multiple-body-parts
  (let [email (core.email/->Email (create-mock-header)
                                  [(create-body-part "text/text" "Simple content")
                                   (create-body-part "text/html" "Better content")]
                                  [])
        training-data (core.email/training-content "text/html" email)]
    (testing
      (is (= (:message-id training-data) "test123"))
      (is (= (:training-content training-data) "Better content")))))

(deftest training-content-empty-text-contents
  (let [mock-data (core.email/->Email (create-mock-header) [] [])
        result (core.email/training-content "text/plain" mock-data)]
    (is (= nil result))))

(deftest training-content-single-text-contents
  (let [mock-data (create-email-with-fake-body [["text/plain" "test"]])
        expected-result {:message-id "test123" :training-content "test" :language nil :category nil :subject "test subject"}
        result (core.email/training-content "text/plain" mock-data)]
    (is (= expected-result result))))

(deftest trained-emails-single-text-contents-different-mimetype
  (let [mock-data (create-email-with-fake-body [["text/html" "test"]])
        expected-result {:message-id "test123" :training-content "test" :language nil :category nil :subject "test subject"}
        result (core.email/training-content "text/plain" mock-data)]
    (is (= expected-result result))))

(deftest trained-emails-multiple-text-contents
  (let [mock-data (create-email-with-fake-body [["text/html" "test"] ["text/plain" "test2"]])
        expected-result {:message-id "test123" :training-content "test2" :language nil :category nil :subject "test subject"}
        result (core.email/training-content "text/plain" mock-data)]
    (is (= expected-result result))))
