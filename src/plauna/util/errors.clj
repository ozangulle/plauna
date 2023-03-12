(ns plauna.util.errors)

(defn return-error [error]
  {:error error})

(defn return-success [success]
  {:success success})

(defn nil-or-success [result]
  (if (nil? result)
    (return-error "The result was nil.")
    (return-success result)))

(defn error-check [result success-fn error-fn]
  (if (some? (:error result))
    (error-fn (:error result))
    (success-fn (:success result))))
