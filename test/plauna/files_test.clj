(ns plauna.files-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [plauna.files :as files]
            [plauna.messaging :refer [main-chan restart-main-chan]]
            [clojure.core.async :refer [go <!! chan close!]]))

(defn resource->is [resource-path]
  (io/input-stream (io/resource resource-path)))

(deftest read-single-item-mbox
  (let [test-chan (chan 20)]
    (files/read-emails-from-mbox (resource->is "test/email_corpus/test-email-1.mbox") test-chan)
    (close! test-chan)
    (loop [result (<!! test-chan)
           results []]
      (if (nil? result)
        (is (= 1 (count results)))
        (recur (<!! test-chan) (conj results result))))))

(deftest read-larger-mbox
  (let [test-chan (chan 20)]
    (files/read-emails-from-mbox (resource->is  "test/email_corpus/weird-mbox.mbox") test-chan)
    (close! test-chan)
    (loop [result (<!! test-chan)
           results []]
      (if (nil? result)
        (is (= 17 (count results)))
        (recur (<!! test-chan) (conj results result))))))
