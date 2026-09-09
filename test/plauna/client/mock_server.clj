(ns plauna.client.mock-server
  (:require  [clojure.test :as t])
  (:import [com.icegreen.greenmail.util GreenMail ServerSetup ServerSetupTest]))

(def imap-server (atom nil))

(defn start-server []
  (let [setup ServerSetupTest/IMAP]
    (.setServerStartupTimeout setup 1000)
    (reset! imap-server (GreenMail. (into-array [ServerSetupTest/IMAP])))
    (.setUser @imap-server "test-user@localhost" "test-user" "secret")
    (.start @imap-server)))

(defn stop-server []
  (.stop @imap-server))
