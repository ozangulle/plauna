(ns plauna.core.server-comm)

(defrecord ServerResponse [type message data])

(defn make-server-response [type message data] (->ServerResponse type message data))
