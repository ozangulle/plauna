(ns plauna.client-test
  (:require [plauna.client :as sut]
            [plauna.interfaces :as int]
            [plauna.client.connection :as conn]
            [clojure.test :as t])
  (:import [plauna.interfaces DB IMAPConnection]))

(t/deftest start-imap-connections
  (let [connect-called (atom false)
        monitor-called (atom false)
        mock-db (proxy [DB] []
                  (fetch_connections [] [{:id "some-id"}])
                  (fetch_connection [_] {:id "some-id"}))
        mock-conn (proxy [IMAPConnection] []
                    (connect [] (reset! connect-called true))
                    (connected_QMARK_ [] true)
                    (monitor_folders [] (reset! monitor-called true))
                    (list_folders [] ["INBOX"]))
        context {:db mock-db}]
    (t/testing "Client creates and starts connections"
      (with-redefs [conn/create-connection (fn [_ _] mock-conn)
                    sut/connection-config (fn [_ _] {:imap {:id "some-id"}})]
        (sut/start-imap-connections context)
        (t/is (= mock-conn (get (deref sut/connections) "some-id")))
        (t/is (true? @connect-called))
        (t/is (true? @monitor-called))))

    (t/testing "conncetion-information returns real connected? value when connections are populated"
      (reset! sut/connections {"some-id" mock-conn})
      (let [result (sut/connection-information "some-id" context)]
        (t/is (true? (:connected result)))))

    (t/testing "conncetion-information returns false if connection is not in variable"
      (reset! sut/connections {})
      (let [result (sut/connection-information "some-id" context)]
        (t/is (false? (:connected result)))))

    (t/testing "conncetion-folders returns the folders when connections are populated"
      (reset! sut/connections {"some-id" mock-conn})
      (let [result (sut/connection-folders {:id "some-id"})]
        (t/is (= ["INBOX"] result))))

    (t/testing "conncetion-folders returns the empty vector when connections are not populated"
      (reset! sut/connections {})
      (let [result (sut/connection-folders {:id "some-id"})]
        (t/is (= [] result))))))

