(ns plauna.util.page)

(defrecord PageRequest [page size])

(defn page-request [page size]
  (->PageRequest (or page 1) (or size 10)))

(defn page-request->limit-offset [page-request]
  {:limit (:size page-request) :offset (* (:size page-request) (dec (:page page-request)))})
