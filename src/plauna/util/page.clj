(ns plauna.util.page
  (:require [clojure.math :refer [ceil]]))

(defrecord PageRequest [page size])

(defn page-request [page size]
  (->PageRequest (or page 1) (or size 10)))

(defn calculate-pages-total [total page-size] (inc (int (ceil (quot total page-size)))))

(defn page-request->limit-offset [page-request]
  {:limit (:size page-request) :offset (* (:size page-request) (dec (:page page-request)))})
