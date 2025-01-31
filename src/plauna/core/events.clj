(ns plauna.core.events)

(defn create-event
  ([type payload options]
   {:type type
      :options options
      :payload payload})
  ([type payload options triggering-event]
   {:type type
    :options (conj (:options triggering-event) options)
    :payload payload}))
