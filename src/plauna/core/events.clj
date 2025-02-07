(ns plauna.core.events
  (:require
   [clojure.core.async :refer [chan go <! go-loop] :as async]
   [taoensso.telemere :as t]))

(defn create-event
  ([type payload options]
   {:type type
    :options options
    :payload payload})
  ([type payload options triggering-event]
   {:type type
    :options (conj (:options triggering-event) options)
    :payload payload}))

(defn return-key-on-complete [key fn]
  (t/log! :info ["Starting restart loop for" key])
  (go  (let [return-val (<! (fn))]
         (t/log! :debug ["Event loop for" key "returned" return-val])
         key)))

(defn keep-track [active-register event-register]
  (let [event-chan (chan 10)
        event-mix (async/mix event-chan)]
    (doseq [val (vals active-register)] (async/admix event-mix val))
    (go-loop [mix event-mix
              register active-register]
      (when-let [event-key (<! event-chan)]
        (async/unmix mix (event-key register))
        (let [new-chan (return-key-on-complete event-key (get event-register event-key))]
          (async/admix mix new-chan)
          (recur mix (conj register {event-key new-chan})))))))

(defn start-event-loops
  "Start event loops which restart by themselves if they somehow complete.

  Takes an event register in the form {:event-key event-fn} where event-fn should always return a channel."
  [event-register]
  (let [active-register (reduce (fn [register entry] (conj register {(first entry) (return-key-on-complete (first entry) (second entry))})) {} event-register)]
    (keep-track active-register event-register)))
