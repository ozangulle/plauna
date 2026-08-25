(ns plauna.client.session
  (:import
   (java.util Properties)
   (jakarta.mail Session)))

(defn default-port-for-security [security]
  (if (= security "ssl") 993 143))

(defn oauth2? [connection-config] (= "oauth2" (:auth-type connection-config)))

(defn security [connection-config]
  (let [security (get connection-config :security "ssl")]
    (if (some #(= security %) ["ssl" "starttls" "plain"])
      security
      "ssl")))

(defn port [connection-config]
  (str (get connection-config :port (default-port-for-security (security connection-config)))))

(defn check-ssl-certs? [connection-config] (get connection-config :check-ssl-certs true))

(defn default-imap-properties ^Properties [connection-config]
  (doto (new Properties)
    (.setProperty "mail.imap.port" (port connection-config))
    (.setProperty "mail.imap.usesocketchannels" "true")
    (.setProperty "mail.imap.timeout" "5000")
    (.setProperty "mail.imap.partialfetch" "false")
    (.setProperty "mail.imap.fetchsize" "1048576")))

(defn oauth-properties [connection-config]
  (fn [^Properties properties]
    (if (oauth2? connection-config)
      (doto properties (.setProperty "mail.imap.auth.mechanisms" "XOAUTH2"))
      properties)))

(defn security-properties [connection-config]
  (let [security-key (security connection-config)]
    (fn [^Properties properties]
      (cond (= security-key "ssl") (doto properties (.setProperty "mail.imap.ssl.enable" "true"))
            (= security-key "starttls") (doto properties (.setProperty "mail.imap.starttls.enable" "true"))
            (= security-key "plain") properties
            :else (doto properties (.setProperty "mail.imap.ssl.enable" "true"))))))

(defn certification-check-properties [connection-config]
  (if (not (check-ssl-certs? connection-config))
    (fn [^Properties properties] (doto properties (.setProperty "mail.imap.ssl.trust" "*")))
    (fn [^Properties properties] properties)))

(defn set-debug-mode [connection-config]
  (let [debug? (get connection-config :debug false)]
    (fn [^Session session]
      (if debug? (doto session (.setDebug true)) session))))

(defn config->session [connection-config]
  (-> (default-imap-properties connection-config)
      ((security-properties connection-config))
      ((oauth-properties connection-config))
      ((certification-check-properties connection-config))
      Session/getInstance
      ((set-debug-mode connection-config))))
