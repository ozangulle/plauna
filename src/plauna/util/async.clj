(ns plauna.util.async
  (:require [clojure.core.async :refer [timeout alts! alts!!]]))

(defmacro fetch-or-timeout!
  "Non-blocking. Either fetch a value from the channel or timeout after x milliseconds."
  [chan milliseconds]
  `(let [timeout# (timeout ~milliseconds)
         [value# port#] (alts! [~chan timeout#])]
        (if (= ~chan port#)
          value#
          :timed-out)))

(defmacro fetch-or-timeout!!
  "Blocking. Either fetch a value from the channel or timeout after x milliseconds."
  [chan milliseconds]
  `(let [timeout# (timeout ~milliseconds)
         [value# port#] (alts!! [~chan timeout#])]
        (if (= ~chan port#)
          value#
          :timed-out)))
