(ns plauna.messaging
  (:require [clojure.core.async :refer [chan pub sub <! go-loop] :as async]))

(set! *warn-on-reflection* true)

(def main-chan (ref (chan 1000)))

(def main-publisher (ref (pub @main-chan :type)))

(defn restart-main-chan [] (dosync (alter main-chan (fn [_] (chan 1000)))
                                   (alter main-publisher (fn [_] (pub @main-chan :type)))))

(def limiter-limit (ref 300))

(defn channel-limiter [target-type]
  (let [bucket-channel (chan @limiter-limit)
        target-channel (chan)]
    (sub @main-publisher target-type target-channel)
    (go-loop []
      (when-some [_ (<! target-channel)]
        (<! bucket-channel))
      (recur))
    bucket-channel))

(comment (channel-limiter :test)
         (async/<!! @main-chan))
