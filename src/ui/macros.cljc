(ns ui.macros)

(defmacro backend-call
  "Has the form
  {:backend (backend-call with arguments)
   :on-success (fn [body] do something)
   :on-error (error handling, fallback to showing a toast message)"
  [data]
  `(let [data# ~data
         success-fn# (:on-success data#)
         error-fn#   (or (:on-error data#)
                         (fn [body#]
                           (ui.components/show-snackbar (:message body#) :warning nil)))
         callback#   (fn [response#]
                       (if (= 200 (:status response#))
                         (success-fn# (:body response#))
                         (error-fn# (:body response#))))]
     (cljs.core.async/take! (:backend data#) callback#)))
