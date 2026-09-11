(ns plauna.client.mock-server
  (:require  [clojure.test :as t])
  (:import [com.icegreen.greenmail.util GreenMail GreenMailUtil ServerSetup ServerSetupTest]
           [jakarta.mail Folder Session Message]
           [java.util Properties]))

(def imap-server (atom nil))

(defn start-server []
  (let [setup ServerSetupTest/IMAP]
    (.setServerStartupTimeout setup 1000)
    (reset! imap-server (GreenMail. (into-array [ServerSetupTest/IMAP])))
    (.setUser @imap-server "test-user@localhost" "test-user" "secret")
    (.start @imap-server)))

(defn stop-server []
  (.stop @imap-server))

(defn createMimeMessage [subject body store]
  (GreenMailUtil/createTextEmail "test-user@localhost" "from@localhost" subject body (.getServerSetup (.getImap store))))

(defn store []
  (let [session (Session/getInstance (Properties.))
        store (.getStore session "imap")]
    (.connect store "localhost" 3143 "test-user" "secret")
    store))

(defn create-folder [folder-name]
  (let [target-folder (.getFolder (store) folder-name)]
    (when-not (.exists target-folder)
      (.create target-folder Folder/HOLDS_MESSAGES))))

(defn send-email-to-inbox []
  (let [subject (GreenMailUtil/random)
        body (GreenMailUtil/random)
        message (createMimeMessage subject body @imap-server)
        user (.setUser @imap-server "test-user@localhost" "test-user", "secret")]
    (.deliver user message)))

(defn move-email-to-folder [folder-name]
  (let [connected-store (store)
        inbox (.getFolder connected-store "INBOX")
        target-folder (.getFolder connected-store folder-name)
        _ (.open inbox Folder/READ_WRITE)
        message (.getMessage inbox 1)]
    (.open target-folder Folder/READ_WRITE)
    (.moveMessages inbox (into-array Message [message]) target-folder)))
