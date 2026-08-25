(ns plauna.client.session-test
  (:require [plauna.client.session :as sut]
            [clojure.test :as t])
  (:import
   (java.util Properties)
   (jakarta.mail Session)))

(t/deftest ssl-properties-set-correctly
  (let [session ^Session (sut/config->session {:security "ssl" :port 993})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.ssl.enable", "true")
                              (.setProperty "mail.imap.port", "993")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (t/is (= expected-properties (.getProperties session)))))

(t/deftest starttls-properties-set-correctly
  (let [session ^Session (sut/config->session {:security "starttls" :port 143})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.starttls.enable", "true")
                              (.setProperty "mail.imap.port", "143")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (t/is (= expected-properties (.getProperties session)))))

(t/deftest plain-text-properties-set-correctly
  (let [session ^Session (sut/config->session {:security "plain" :port 143})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.port", "143")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (t/is (= expected-properties (.getProperties session)))))

(t/deftest empty-values-return-ssl
  (let [session ^Session (sut/config->session {})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.ssl.enable", "true")
                              (.setProperty "mail.imap.port", "993")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (t/is (= expected-properties (.getProperties session)))))

(t/deftest non-compliant-security-values-return-ssl
  (let [session ^Session (sut/config->session {:security "does-not-exist"})
        expected-properties (doto (new Properties)
                              (.setProperty "mail.imap.ssl.enable", "true")
                              (.setProperty "mail.imap.port" "993")
                              (.setProperty "mail.imap.usesocketchannels" "true")
                              (.setProperty "mail.imap.timeout" "5000")
                              (.setProperty "mail.imap.partialfetch" "false")
                              (.setProperty "mail.imap.fetchsize" "1048576"))]
    (t/is (= expected-properties (.getProperties session)))))

(t/deftest debug-false-on-default
  (let [session ^Session (sut/config->session {})]
    (t/is (= false (.getDebug session)))))

(t/deftest debug-can-be-set-true
  (let [session ^Session (sut/config->session {:debug true})]
    (t/is (= true (.getDebug session)))))

(t/deftest set-cert-checks-to-false
  (let [session ^Session (sut/config->session {:security "ssl" :check-ssl-certs false})]
    (t/is (= "*" (.getProperty session "mail.imap.ssl.trust")))))
