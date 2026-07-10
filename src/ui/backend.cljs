(ns ui.backend
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<! take!]]))

(defn serialize [val] (js/JSON.stringify (clj->js val)))

(defn fetch-emails [parameters callback]
  (take! (http/get "/api/emails" {:query-params parameters}) callback))

(defn fetch-email-data [id callback] (take! (http/get (str "/api/emails/" id)) callback))

(defn delete-email [id callback] (take! (http/delete (str "/api/emails/" id)) callback))

(defn save-metadata-for-email [email move?] (http/post "/api/metadata" {:content-type "application/json" :body (serialize {:message-id (get-in email [:header :message-id]) :metadata (:metadata email) :move? move?})}))

(defn fetch-preferences [callback] (take! (http/get "/api/admin/preferences") callback))

(defn save-preferences [preferences] (http/post "/api/admin/preferences" {:content-type "application/json"
                                                                          :body (serialize preferences)}))

(defn fetch-categories [callback] (take! (http/get "/api/admin/categories") callback))

(defn add-category [name callback] (take! (http/post "/api/admin/categories" {:content-type "application/json ":body (serialize {:name name})}) callback))

(defn delete-category [id callback] (take! (http/delete (str "/api/admin/categories/" id)) callback))

(defn fetch-languages [callback] (take! (http/get "/api/admin/languages") callback))

(defn update-languages [languages callback] (take! (http/post "/api/admin/languages" {:content-type "application/json" :body (serialize languages)}) callback))

(defn fetch-connections [callback] (take! (http/get "/api/admin/connections") callback))

(defn post-connection-control [id control parse-settings callback] (take! (http/post (str "/api/admin/connections/" id "/controls") {:content-type "application/json" :body (serialize {:operation control :parse-settings parse-settings})}) callback))

(defn fetch-connection [id callback] (take! (http/get (str "/api/admin/connections/" id)) callback))

(defn delete-connection [id callback] (take! (http/delete (str "/api/admin/connections/" id)) callback))

(defn update-connection [id config callback] (take! (http/put (str "/api/admin/connections/" id) {:content-type "application/json" :body (serialize {:config config})}) callback))

(defn add-connection [config callback] (take! (http/post "/api/admin/connections" {:content-type "application/json" :body (serialize config)}) callback))

(defn add-auth-provider [provider callback] (take! (http/post "/api/admin/auth-providers" {:content-type "application/json" :body (serialize provider)}) callback))

(defn delete-auth-provider [id callback] (take! (http/delete (str "/api/admin/auth-providers/" id)) callback))

(defn update-auth-provider [id provider callback] (take! (http/put (str "/api/admin/auth-providers/" id) {:content-type "application/json" :body (serialize provider)}) callback))

(defn fetch-auth-providers [callback] (take! (http/get "/api/admin/auth-providers" {:content-type "application/json"}) callback))
