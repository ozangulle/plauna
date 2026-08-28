(ns plauna.interfaces)

(defprotocol DB
  "Database protocol"
  (fetch-auth-provider [this id])
  (fetch-auth-providers [this])
  (fetch-categories [this] "Get a list of all categories")
  (fetch-connection [this id] "Get connection for id.")
  (fetch-emails [this entity customization] "Get a list of emails")
  (fetch-folder-category-maps [this id] "Get the folder-category pairs for a connection by id")
  (fetch-oauth-token-data [this id] "Get oauth token data for a connection")
  (save-category [this category-name])
  (save-email [this email])
  (save-folder-category-map [this fcmap]))

(defprotocol EmailClient
  "Email client"
  (start-monitor [this config context] "Connect to the client")
  (connections [this] "Get a list of connections")
  (create-category-directories! [this connection-data category-names])
  (connection-id-for-email [this connections email])
  (move-email-between-categories [this connection-id message-id old-category new-category context])
  (move-email-to-category [this connection-id original-message original-folder category])
  (number-of-messages-in-folder [this connection-data folder-name])
  (nth-email-from-folder [this n folder]))

(defprotocol Analyzer
  "Language detection and categorization"
  (enrich-email [this email])
  (detect-language [this email])
  (normalize [this body-part]))

(defprotocol IMAPConnection
  (connect [this])
  (connected? [this])
  (list-folders [this])
  (no-of-messages-in-folder [this folder-name])
  (nth-message-in-folder [this folder-name n])
  (move-message [this message source-name target-name])
  (move-email-by-id [this message-id source-name target-name])
  (monitor-folders [this])
  (disconnect-and-stop-monitoring [this]))
