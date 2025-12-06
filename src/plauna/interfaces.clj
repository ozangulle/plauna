(ns plauna.interfaces)

(defprotocol DB
  "Database protocol"
  (fetch-connection [this id] "Get connection for id.")
  (fetch-oauth-token-data [this id] "Get oauth token data for a connection")
  (fetch-auth-provider [this id]))

(defprotocol EmailClient
  "Email client"
  (start-monitor [this config] "Connect to the client"))
