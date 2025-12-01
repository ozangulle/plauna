(ns plauna.application
  (:require [plauna.database :as d]
            [plauna.client :as cl]))

(defmulti connect-with-type (fn [context connection] (:auth-type connection)))

(defmethod connect-with-type "oauth2" [context connection])

(defmethod connect-with-type :default [context connection])

(defn connect-to-client
  "Returns {:result :ok} or {:result :redirect :provider provider} in case of oauth2"
  [context id]
  (let [db (:db context)
        client (:client context)
        connection (d/fetch-connection db id)]
                (if (= "oauth2" (:auth-type connection))
                  (let [auth-provider (d/fetch-auth-provider db (:auth-provider connection))]
                      (cond
                        (nil? auth-provider) (throw (ex-info "Auth type is 'oauth2' but there is no auth provider." {:connection connection}))
                        (nil? (d/fetch-oauth-token-data db id))
                        {:result :redirect
                         :provider (d/fetch-auth-provider db (:auth-provider connection))}
                        :else (do (cl/start-monitor client connection) {:result :ok})))
                  (do (cl/start-monitor client connection) {:result :ok}))))
