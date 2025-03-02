(ns plauna.preferences-test
  (:require [plauna.preferences :as sut]
            [clojure.core.cache :as c]
            [clojure.test :as t]))

(defn return-fn-for-preferences [returns] (swap! sut/fetch-fn (fn [_] (fn [_] returns))))

(t/deftest fetch-returns-default
  (swap! sut/cache (fn [_] (c/ttl-cache-factory {} :ttl 1)))
  (return-fn-for-preferences nil)
  (t/is (= (sut/log-level) :info)))

(t/deftest fetch-returns-string
  (swap! sut/cache (fn [_] (c/ttl-cache-factory {} :ttl 1)))
  (return-fn-for-preferences ":debug")
  (t/is (= (sut/log-level) :debug)))

(t/deftest fetch-returns-double
  (swap! sut/cache (fn [_] (c/ttl-cache-factory {} :ttl 1)))
  (return-fn-for-preferences "0.01")
  (t/is (= (sut/categorization-threshold) 0.01)))
