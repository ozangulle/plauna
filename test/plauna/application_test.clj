(ns plauna.application-test
  (:require [clojure.test :refer :all]
            [plauna.database :as db]
            [plauna.client :as cl]
            [plauna.application :as app]))

(deftest basic-auth
  (let [database (reify db/DB (fetch-connection [_ id] {:id id :auth-type "basic"}))
        client (reify cl/EmailClient (start-monitor [_ config]))
        context {:db database :client client}]
    (is (= {:result :ok} (app/connect-to-client context "abc"))  "Basic authentication calls email-client's login method and returns ok")))

(deftest basic-auth-2
  (let [database (reify db/DB (fetch-connection [_ id] {:id id}))
        client (reify cl/EmailClient (start-monitor [_ config]))
        context {:db database :client client}]
    (is (= {:result :ok} (app/connect-to-client context "abc"))  "If no auth-type is defined, fall back on basic auth and return ok")))

(deftest oauth2-auth
  (let [database (reify db/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] {:id id}))
        client (reify cl/EmailClient (start-monitor [_ config]))
        context {:db database :client client}]
    (is (= {:result :redirect, :provider {:id 2}}
           (app/connect-to-client context "abc"))
        "auth-type 'oauth2' with auth provider but no token data returns a :redirect with the provider")))

(deftest oauth2-auth-2
  (let [database (reify db/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] {:not :empty})
                   (fetch-auth-provider [_ id] {:id id}))
        client (reify cl/EmailClient (start-monitor [_ config]))
        context {:db database :client client}]
    (is (= {:result :ok}
           (app/connect-to-client context "abc"))
        "auth-type 'oauth2' with auth provider and token data calls client login and returns ok")))

(deftest oauth2-auth-3
  (let [database (reify db/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] nil))
        client (reify cl/EmailClient (start-monitor [_ config]))
        context {:db database :client client}]
    (is (thrown? RuntimeException (app/connect-to-client context "abc"))
        "auth-type 'oauth2' with auth provider and token data calls client login and returns ok")))
