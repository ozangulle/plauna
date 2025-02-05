(ns plauna.core.events-test
  (:require [plauna.core.events :as events]
            [clojure.core.async :as async]
            [clojure.test :as test]))

(test/deftest return-key-on-complete-works
  (let [test-chan (async/chan)
        test-fn (fn [] (async/go (async/<! test-chan)))
        test-case (events/return-key-on-complete :test-key test-fn)]
    (async/close! test-chan)
    (test/is (= :test-key (async/<!! test-case)))))

(test/deftest keep-track-works
  (let [test-atom (atom 0)
        test-chan (atom (async/chan))
        test-fn (fn [] (async/go (swap! test-atom inc) (async/<! @test-chan)))
        test-register {:test-case test-fn}]
    (events/keep-track {:test-case (events/return-key-on-complete :test-case test-fn)} test-register)
    (swap! test-chan (fn [old] (async/close! old) (async/chan)))
    (Thread/sleep 100)
    (swap! test-chan (fn [old] (async/close! old) (async/chan)))
    (Thread/sleep 100)
    (test/is (= 2 @test-atom))))

(test/deftest event-register-works
  (let [test-atom (atom 0)
        test-chan (atom (async/chan))
        test-fn (fn [] (async/go (swap! test-atom inc) (async/<! @test-chan)))
        test-register {:test-case test-fn}]
    (events/start-event-loops test-register)
    (swap! test-chan (fn [old] (async/close! old) (async/chan)))
    (Thread/sleep 100)
    (swap! test-chan (fn [old] (async/close! old) (async/chan)))
    (Thread/sleep 100)
    (swap! test-chan (fn [old] (async/close! old) (async/chan)))
    (Thread/sleep 100)
    (test/is (= 3 @test-atom))))
