(ns plauna.client-test
  (:require [clojure.test :refer :all]
            [plauna.client :as client])
  (:import (java.util Properties)
           (jakarta.mail Session)))

(deftest ssl-properties-set-correctly
  (let [session ^Session (client/config->session {:security :ssl :port 993})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.ssl.enable", "true")
                              (.setProperty "mail.imap.port", "993")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (is (= expected-properties (.getProperties session)))))

(deftest starttls-properties-set-correctly
  (let [session ^Session (client/config->session {:security :starttls :port 143})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.starttls.enable", "true")
                              (.setProperty "mail.imap.port", "143")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (is (= expected-properties (.getProperties session)))))

(deftest plain-text-properties-set-correctly
  (let [session ^Session (client/config->session {:security :plain :port 143})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.port", "143")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (is (= expected-properties (.getProperties session)))))

(deftest empty-values-return-ssl
  (let [session ^Session (client/config->session {})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.ssl.enable", "true")
                              (.setProperty "mail.imap.port", "993")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (is (= expected-properties (.getProperties session)))))

(deftest non-compliant-security-values-return-ssl
  (let [session ^Session (client/config->session {:security :does-not-exist})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.ssl.enable", "true")
                              (.setProperty "mail.imap.port" "993")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (is (= expected-properties (.getProperties session)))))

(deftest debug-false-on-default
  (let [session ^Session (client/config->session {})]
    (is (= false (.getDebug session)))))

(deftest debug-can-be-set-true
  (let [session ^Session (client/config->session {:debug true})]
    (is (= true (.getDebug session)))))

(deftest set-cert-checks-to-false
  (let [session ^Session (client/config->session {:security :ssl :check-ssl-certs false})]
    (is (= "*" (.getProperty session "mail.imap.ssl.trust")))))
