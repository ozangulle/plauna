(ns plauna.client.oauth
  (:require [clj-http.client :as http]
            [ring.util.codec :refer [url-encode]]))

(defn authorize-uri [provider csrf-token]
  (str
   (:auth-url provider)
   "?response_type=code"
   "&client_id="
   (url-encode (:client-id provider))
   "&redirect_uri="
   (url-encode (:redirect-url provider))
   "&scope="
   (url-encode (:scope provider))
   "&state="
   (url-encode csrf-token)
   "&access_type=offline"))

(defn exchange-code-for-access-token [provider code]
  (-> (http/post (:token-url provider)
                 {:form-params {:code         code
                                :grant_type   "authorization_code"
                                :client_id    (:client-id provider)
                                :redirect_uri (:redirect-url provider)}
                  :basic-auth [(:client-id provider) (:client-secret provider)]
                  :as          :json})
      :body))

(defn exchange-refresh-token-for-access-token [provider refresh-token]
  (-> (http/post (:token-url provider)
                 {:form-params {:refresh_token refresh-token
                                :grant_type   "refresh_token"
                                :client_id (:client-id provider)
                                :client_secret (:client-secret provider)}
                  :as          :json})
      :body))
