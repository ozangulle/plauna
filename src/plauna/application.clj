(ns plauna.application
  (:require [plauna.interfaces :as int]
            [taoensso.telemere :as t]))

(defn connect-to-client
  "Returns {:result :ok} or {:result :redirect :provider provider} in case of oauth2"
  [context id]
  (let [db (:db context)
        client (:client context)
        connection (int/fetch-connection db id)]
    (if (= "oauth2" (:auth-type connection))
      (let [auth-provider (int/fetch-auth-provider db (:auth-provider connection))
            oauth-data (int/fetch-oauth-token-data db id)]
        (cond
          (nil? auth-provider) (throw (ex-info "Auth type is 'oauth2' but there is no auth provider." {:connection connection}))
          (or (nil? oauth-data) (nil? (:access-token oauth-data)) (nil? (:refresh-token oauth-data)))
          (do
            (t/log! :warn ["Connection" (:user connection) (:host connection) "is set to use oauth2 but has no tokens in the db. You need to login manually from the 'Connections' page first."])
            {:result :redirect
             :provider (int/fetch-auth-provider db (:auth-provider connection))})
          :else (do (int/start-monitor client connection) {:result :ok})))
      (do (int/start-monitor client connection) {:result :ok}))))
