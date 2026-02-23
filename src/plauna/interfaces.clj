(ns plauna.interfaces)

(defprotocol DB
  "Database protocol"
  (fetch-connection [this id] "Get connection for id.")
  (fetch-oauth-token-data [this id] "Get oauth token data for a connection")
  (fetch-auth-provider [this id])
  (fetch-categories [this] "Get a list of all categories")
  (fetch-emails [this entity customization] "Get a list of emails")
  (save-category [this category-name]))

(defprotocol EmailClient
  "Email client"
  (start-monitor [this config] "Connect to the client")
  (connections [this] "Get a list of connections")
  (create-category-directories! [this connection-data category-names])
  (connection-id-for-email [this connections email])
  (move-email-between-categories [this connection-id message-id old-category new-category]))
