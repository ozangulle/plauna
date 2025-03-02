(ns plauna.analysis-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [plauna.analysis :as analysis]))

;; Normalization tests

(deftest normalization-1
  (let [res (analysis/normalize (slurp (io/resource "test/normalization/original-text-1.txt")))]
    (is (= (s/trim (slurp (io/resource "test/normalization/normalized-text-1.txt"))) res))))
