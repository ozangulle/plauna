(ns plauna.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.codec :refer [base64-decode]]
            [plauna.application :as app]
            [plauna.markup :as markup]
            [plauna.files :as files]
            [plauna.util.page :as page]
            [plauna.client :as client]
            [plauna.client.oauth :as oauth]
            [plauna.preferences :as p]
            [plauna.core.email :as core-email]
            [clojure.math :refer [ceil]]
            [compojure.core :as comp]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.session.cookie :refer [cookie-store]]
            [ring.util.response :refer [response redirect]]
            [cheshire.core :refer [parse-string]]
            [taoensso.telemere :as t]
            [clojure.java.io :as io]
            [plauna.analysis :as analysis]
            [plauna.database :as db]
            [plauna.messaging :as messaging]
            [clojure.core.async :as async]
            [clojure.data :as cd]
            [nrepl.server :as nrepl])
  (:import [java.net ServerSocket]
           [java.util UUID]
           [org.eclipse.jetty.server Server]))

(set! *warn-on-reflection* true)

(defonce server (atom nil))

(defonce repl-server (atom nil))

(def html-headers {"Content-Type" "text/html; charset=UTF-8"})

(defonce global-messages (atom []))

(defn interleave-all [& seqs]
  (reduce (fn [acc index] (into acc (map #(get % index) seqs)))
          []
          (range (apply max (map count seqs)))))

(defn vectorize [items]
  (if (vector? items) items [items]))

(defn flatten-map [param-map]
  (let [message-ids (vectorize (get param-map :message-id []))
        languages (vectorize (get param-map :language []))
        categories (vectorize (get param-map :category []))
        language-confidence (vectorize (get param-map :language-confidence []))
        category-confidence (vectorize (get param-map :category-confidence))]
    (map (fn [vect] {:message-id (nth vect 0) :language (nth vect 1) :category (nth vect 2) :language-confidence (nth vect 3) :category-confidence (nth vect 4)}) (partition 5 (interleave-all message-ids languages categories language-confidence category-confidence)))))

(defn params->update-request [params]
  (let [language (:language params)
        category-id (:category params)
        language-exists (and (some? language) (seq language))
        category-exists (and (some? category-id) (seq category-id))]
    {:language   (when language-exists (:language params))
     :category-id (when category-exists (Integer/parseInt (:category params)))
     :category-confidence  (when category-exists (Float/parseFloat (:category-confidence params)))
     :language-confidence (when language-exists (Float/parseFloat (:language-confidence params)))}))

(defn save-metadata-form [params]
  (let [transformed (flatten-map params)]
    (dorun (map (fn [x] (let [request (params->update-request x)]
                          (db/update-metadata (:message-id x) (:category-id request) (:category-confidence request) (:language request) (:language-confidence request)))) transformed))))

(defn success-html-with-body [body]
  {:status  200
   :headers {"Content-Type" "text/html; charset=UTF-8"}
   :body    body})

(defn redirect-to-referer [request]
  {:status 303
   :headers {"Location" (get (:headers request) "referer")}})

(defn redirect-request
  ([request]
   (let [redirect-url (get-in request [:params :redirect-url])]
     (if (some? redirect-url)
       {:status 303 :headers {"Location" redirect-url}}
       {:status 303 :headers {"Location" (-> request :uri)}})))
  ([request messages]
   (swap! global-messages (fn [m] (conj m messages)))
   (let [redirect-url (get-in request [:params :redirect-url])]
     (if (some? redirect-url)
       {:status 303 :headers {"Location" redirect-url}}
       {:status 303 :headers {"Location" (-> request :uri)}}))))

(defn language-preferences []
  (let [preferences (db/get-language-preferences)
        languages (filterv #(not (= "n/a" %)) (mapv :language (db/get-languages)))]
    (if (empty? languages)
      []
      (if (< (count preferences) (count languages))
        (let [existing-languages-in-pref (mapv :language preferences)
              diff (cd/diff (set existing-languages-in-pref) (set languages))]
          (db/add-language-preferences
           (mapv vector (second diff) (repeat (count (second diff)) false)))
          (db/get-language-preferences))
        preferences))))

(defn languages-to-use-in-training []
  (sequence (comp (filter #(= 1 (:use_in_training %))) (map :language)) (db/get-language-preferences)))

(defn write-all-categorized-emails-to-training-files []
  (files/delete-files-with-type :train)
  (doseq [language (languages-to-use-in-training)
          :let [entity-query {:entity :enriched-email :page {:size 100 :page 1}}
                sql-query {:where [:and [:<> :category nil] [:= :language language]]}
                write-func (fn [data] (files/write-to-training-file language (analysis/format-training-data data)))]]
    (core-email/iterate-over-all-pages db/fetch-data write-func entity-query sql-query false)))

(defn write-emails-to-training-files-and-train []
  (if (seq (languages-to-use-in-training))
    (do (write-all-categorized-emails-to-training-files)
        (doseq [training-model (analysis/train-data (files/training-files))]
          (let [os (io/output-stream (files/model-file (:language training-model)))]
            (analysis/serialize-and-write-model! (:model training-model) os))))
    {:type :alert :content "There are no selected languages to train in. Cannot proceed."}))

(defn categorize-content [content language] ;; FIXME This kills the process if content is nil
  (let [category (analysis/categorize content (files/model-file language))]
    {:id         (:id (db/category-by-name (:name category)))
     :name       (:name category)
     :confidence (:confidence category)}))

(defn categorize-uncategorized-n-emails [n]
  (let [languages-to-use (map :language (db/get-activated-language-preferences))
        uncategorized-bodies (:data (db/fetch-data {:entity :body-part :page {:page 0 :size n}} {:where [:and [:in :language languages-to-use] [:<> :language nil] [:= :category nil] [:= :mime-type "text/html"]]}))
        trained-emails (map (fn [email] (conj {:message-id (-> email :body-part :message-id)} (categorize-content (-> email :body-part :sanitized-content) (-> email :metadata :language)))) uncategorized-bodies)]
    (doseq [trained-email trained-emails] (db/update-metadata-category (:message-id trained-email) (:id trained-email) (:confidence trained-email)))))

(defn mime-type-statistics [period]
  (db/query-db {:select [[[:count :headers.message-id] :count] :bodies.mime-type [(db/interval-for-honey period) :interval]] :from [:bodies]
                :join [:headers [:= :bodies.message-id :headers.message_id]]
                :where [:<> :interval nil]
                :group-by [:interval :bodies.mime-type]
                :order-by [[:count :desc]]}))

(defn language-statistics-by-period [period]
  (db/query-db {:select [[[:count :metadata.language] :count] :metadata.language [(db/interval-for-honey period) :interval]] :from [:metadata]
                :join [:headers [:= :metadata.message-id :headers.message_id]]
                :group-by [:language :interval]}))

(defn category-statistics-by-period [period]
  (let [year (:year period)
        categories (reduce (fn [acc el] (merge acc {(:id el) (:name el)})) {} (db/get-categories))
        statistics (if (some? year)
                     (db/query-db {:select [[[:count :metadata.category] :count] :metadata.category [(db/interval-for-honey (:interval period)) :interval]] :from [:metadata]
                                   :join [:headers [:= :metadata.message-id :headers.message_id]]
                                   :where [:and [:<> :category nil] [:like :interval (str year "%")]]
                                   :group-by [:category :interval]})
                     (db/query-db {:select [[[:count :metadata.category] :count] :metadata.category [(db/interval-for-honey (:interval period)) :interval]] :from [:metadata]
                                   :join [:headers [:= :metadata.message-id :headers.message_id]]
                                   :where [:<> :category nil]
                                   :group-by [:category :interval]}))]
    (map (comp
          (fn [map] (if (= 0 (get map :category)) map (update map :category (fn [cat-key] (get categories cat-key)))))
          (fn [map] (update map :category #(if (int? %) % (Integer/parseInt %))))) statistics)))

(defn enriched-email-by-message-id [id] (first (db/fetch-data {:entity :enriched-email :strict false} {:where [:= :message-id id]})))

;; TODO change name template
(def emails-template {:size {:default 20 :type-fn Integer/parseInt}
                      :page {:default 1 :type-fn Integer/parseInt}
                      :filter {:default "all" :type-fn identity}
                      :search-field {:default "subject" :type-fn identity}
                      :search-text {:default nil :type-fn identity}})

(defn template->request-parameters [template]
  (fn [rp] (reduce (fn [acc [k v]] (if (contains? rp k)
                                     (conj acc {k ((:type-fn v) (get rp k))})
                                     (conj acc {k (:default v)})))
                   {} template)))

(defn add-sanitized-text-to-enriched-email [email]
  {:header (:header email)
   :metadata (:metadata email)
   :participants (:participants email)
   :body (map (fn [body-part] (if (core-email/body-text-content? body-part)
                                (conj body-part {:sanitized-content (analysis/normalize-body-part body-part)})
                                body-part)) (:body email))})

(defn get-status-repl-server [] {:status (some? @repl-server) :port 7888})

(defn connection-information [id] (let [conn (db/get-connection id)] (merge conn (client/monitor->map (get @client/connections (:id conn))))))
(defn connection-folders [conn]
  (if (= true (:connected conn))
    (client/folders-in-store (:store (client/connection-data-from-id (:id conn))))
    []))

(defn empty-global-messages [] (swap! global-messages (fn [_] [])))

(defn make-routes [context]
  (comp/defroutes routes

    (route/resources "/")

    (comp/GET "/" {} (let [data (db/yearly-email-stats)]
                       (if (> (count data) 0)
                         {:status  302
                          :headers {"Location" "/emails"}}
                         {:status  302
                          :headers {"Location" "/admin"}})))

    (comp/GET "/admin" {}
      (if (seq @global-messages)
        (let [messages @global-messages]
          (swap! global-messages (fn [_] []))
          (success-html-with-body (markup/administration messages)))
        (success-html-with-body (markup/administration {:repl (get-status-repl-server)}))))

    (comp/POST "/emails/parse" request
      (let [temp-file (get-in request [:params :filename :tempfile])]
        (files/read-emails-from-mbox (io/input-stream temp-file) @messaging/main-chan)
        (redirect-request request {:type :success :content (str "Starting to parse file: " temp-file)})))

    (comp/GET "/admin/categories" {}
      (let [categories (db/get-categories)]
        (success-html-with-body (markup/categories-page categories))))

    (comp/GET "/admin/languages" {}
      (success-html-with-body
       (markup/languages-admin-page (language-preferences))))

    (comp/GET "/admin/preferences" {}
      (let [language-datection-threshold (p/language-detection-threshold)
            categorization-threshold (p/categorization-threshold)
            client-health-check-interval (p/client-health-check-interval)
            log-level (p/log-level)]
        (success-html-with-body (markup/preferences-page
                                 {:language-detection-threshold language-datection-threshold
                                  :categorization-threshold categorization-threshold
                                  :log-level log-level
                                  :client-health-check-interval client-health-check-interval}))))

    (comp/POST "/admin/preferences" request
      (doseq [param (dissoc (:params request) :redirect-url)]
        (p/update-preference (first param) (second param)))
      (t/set-min-level! (p/log-level))
      (redirect-request request))

    (comp/POST "/admin/languages" {params :params}
      (let [langs-to-use (if (vector? (:use params)) (:use params) [(:use params)])]
        (doseq [preference (mapv (fn [id language]
                                   {:id id :language language :use (some? (some #(= language %) langs-to-use))})
                                 (vectorize (:id params))
                                 (vectorize (:language params)))]
          (db/update-language-preference preference)))
      (let [language-preferences (language-preferences)]
        (success-html-with-body (markup/languages-admin-page language-preferences))))

    (comp/POST "/admin/categories" {params :params}
      (app/create-new-category! context (:name params))
      {:status  301
       :headers {"Location" "/admin/categories"}
       :body    (markup/administration {:repl (get-status-repl-server)})})

    (comp/DELETE "/admin/categories/:id" {route-params :route-params}
      (db/delete-category-by-id (:id route-params))
      {:status  301
       :headers {"Location" "/admin/categories"}
       :body    (markup/administration {:repl (get-status-repl-server)})})

    (comp/POST "/admin/database" {}
      (files/check-and-create-database-file)
      (db/create-db)
      {:status  301
       :headers {"Location" "/admin"}
       :body    (markup/administration {:repl (get-status-repl-server)})})

    (comp/GET "/statistics" {}
      (success-html-with-body (markup/statistics-overall (db/yearly-email-stats) (mime-type-statistics :yearly) (language-statistics-by-period :yearly) (category-statistics-by-period {:interval :yearly}))))

    (comp/POST "/metadata" request
      (if (some? (:move (:params request)))
        (let [message-id (:message-id (:params request))
              email-before-update (enriched-email-by-message-id message-id)
              new-category-id (Integer/parseInt (:category (:params request)))
              new-category-name (get (first (filter #(= (:id %) new-category-id) (db/get-categories))) :name "")]
          (app/move-email-to-category email-before-update new-category-name context)
          (save-metadata-form (:params request)))
        (save-metadata-form (:params request)))
      (redirect-to-referer request))

    (comp/POST "/training" request
      (let [result (write-emails-to-training-files-and-train)]
        (when (some? result) (swap! global-messages (fn [mess] (conj mess result))))
        (redirect-to-referer request)))

    (comp/POST "/training/new" request
      (let [n (get (:route-params request) :new 20)]
        (categorize-uncategorized-n-emails n)
        (redirect-request request)))

    (comp/GET "/emails" {params :params}
      (let [parse-fn (template->request-parameters emails-template)
            result (app/fetch-emails context (parse-fn params))]
        (if (seq @global-messages)
          (let [messages @global-messages]
            (swap! global-messages (fn [_] []))
            (success-html-with-body (markup/list-emails (:data result) (:parameters result) (:categories (:optional result)) messages)))
          (success-html-with-body (markup/list-emails (:data result) (:parameters result) (:categories (:optional result)))))))

    (comp/GET "/emails/:id" [id]
      (let [decoded-id (new String ^"[B" (base64-decode id))
            email-data (add-sanitized-text-to-enriched-email (enriched-email-by-message-id decoded-id))
            categories (conj (db/get-categories) {:id nil :name "n/a"})]
        {:status 200
         :header html-headers
         :body   (markup/list-email-contents email-data categories)}))

    (comp/DELETE "/emails/:id" [id]
      (db/delete-email-by-message-id (new String ^"[B" (base64-decode id)))
      {:status  200})

    (comp/GET "/admin/connections" _
      (let [messages @global-messages]
        (empty-global-messages)
        (if (seq messages)
          (response (markup/connections-list (mapv (fn [conn] (merge conn (client/monitor->map (get @client/connections (:id conn))))) (db/get-connections)) messages))
          (response (markup/connections-list (mapv (fn [conn] (merge conn (client/monitor->map (get @client/connections (:id conn))))) (db/get-connections)))))))

    (comp/POST "/admin/connections" request
      (let [params (:params request)
            config {:host (get params :host) :user (get params :user) :secret (get params :secret) :folder (get params :folder) :debug (= "true" (get params :debug)) :security (get params :security) :port (get params :port) :check-ssl-certs (= "true" (get params :check-ssl-certs))}
            id (client/id-from-config config)]
        (db/add-connection (merge config {:id id}))
        (redirect-request request)))

    (comp/DELETE "/admin/connections/:id" request
      (let [params (:params request)]
        (db/delete-connection (get params :id))
        {:status 200}))

    (comp/GET "/admin/new-connection" []
      (let [providers (db/get-auth-providers)]
        {:status 200
         :header html-headers
         :body   (markup/new-connection providers)}))

    (comp/DELETE "/admin/auth-providers/:id" request
      (let [params (:params request)
            body (parse-string (slurp (:body request)) true)]
        (db/delete-auth-provider (get params :id))
        (if (empty? (:conn-id body))
          (redirect "/admin/new-connection" 303)
          (redirect (str "/admin/connections/" (:conn-id body) 303)))))

    (comp/POST "/admin/auth-providers" request
      (let [params (:params request)]
        (db/add-auth-provider (dissoc params :redirect-url))
        (if (= "/admin/connections/" (:redirect-url params))
          (redirect-request (assoc-in request [:params :redirect-url] "/admin/new-connection"))
          (redirect-request request))))

    (comp/PUT "/admin/auth-providers/:id" request
      (let [params (:params request)]
        (db/update-auth-provider params)))

    (comp/GET "/admin/connections/:id" [id]
      (let [conn-info (connection-information id)
            providers (db/get-auth-providers)]
        (if (seq @global-messages)
          (let [messages @global-messages]
            (swap! global-messages (fn [_] []))
            (success-html-with-body (markup/connection (assoc conn-info :auth-providers providers) (connection-folders conn-info) messages)))
          (success-html-with-body (markup/connection (assoc conn-info :auth-providers providers) (connection-folders conn-info))))))

    (comp/PUT "/admin/connections/:id" request
      (let [params (:params request)]
        (db/update-connection {:id (get params :id) :host (get params :host) :user (get params :user) :secret (get params :secret) :folder (get params :folder) :debug (= "true" (get params :debug)) :security (get params :security) :port (get params :port) :check-ssl-certs (= "true" (get params :check-ssl-certs)) :auth-type (get params :auth-type) :auth-provider (get params :auth-provider)})
        {:status 200}))

    (comp/POST "/admin/connections/:id/controls" request
      (let [id (:id (:route-params request))
            operation (:operation (:params request))]
        (cond (= "reconnect" operation) (do (client/reconnect (client/connection-data-from-id id)) (redirect-request request))
              (= "disconnect" operation) (do (client/disconnect (client/connection-data-from-id id)) (redirect-request request))
              (= "connect" operation)
              (let [action (app/connect-to-client context id)]
                (cond
                  (= :redirect (:result action))
                  (let [csrf (.toString (UUID/randomUUID))]
                    (-> (redirect (oauth/authorize-uri (:provider action) csrf)) (assoc :session {:oauth-csrf csrf :connection-id id :provider (:provider action)})))
                  (= :ok (:result action))
                  (redirect-request request)
                  (= :error (:result action))
                  (redirect-request request {:type :alert :content "Connection failed. Please see the logs for the details."})))
              (= "parse" operation) (let [params (:params request)
                                          folder (:folder params)
                                          move (some? (:move params))
                                          conn-data (client/connection-data-from-id id)]
                                      (client/parse-all-in-folder conn-data folder move)
                                      (swap! global-messages (fn [mess] (conj mess {:type :success :content (str "Started parsing " folder " asynchronously. Move folders after parsing: " move)})))
                                      (redirect-request request)))))

    (comp/POST "/metadata/languages" request
      (let [limiter (messaging/channel-limiter :enriched-email)
            process-fn (fn [enriched-emails]
                         (doseq [enriched-email enriched-emails]
                           (async/>!! limiter :token)
                           (async/>!! @messaging/main-chan {:type :language-detection-request :options {} :payload enriched-email})))]
        (core-email/iterate-over-all-pages db/fetch-data process-fn {:entity :enriched-email :strict false :page {:page 1 :size 500}} {:where [:= :language nil]} true))
      (redirect-request request))

    (comp/POST "/repl" request
      (let [operation (get-in request [:params :operation])]
        (cond (= operation "start") (swap! repl-server (fn [_] (t/log! :info "Starting repl server") (nrepl/start-server :bind "0.0.0.0" :port 7888)))
              (= operation "stop") (swap! repl-server (fn [_] (t/log! :info "Stopping repl server") (nrepl/stop-server @repl-server) nil))
              :else (t/log! :error ["Unsupported operation" operation "at /repl"]))
        (redirect-request request)))

    (comp/GET "/oauth2/callback" request
      (let [params (:params request)
            session (:session request)]
        (if (= (:state params) (:oauth-csrf session))
          (try
            (let [response (oauth/exchange-code-for-access-token (:provider session) (:code params))]
              (db/save-oauth-token (assoc response :connection-id (:connection-id session)))
              (app/connect-to-client context (:connection-id session)))
            (redirect "/admin/connections")
            (catch Exception e (t/log! :error e) (redirect "/admin/connections")))
          "Bad response - csrf token mismach")))

    (route/resources "/")))

(defn upload-progress [_ bytes-read content-length item-count]
  (t/log! {:level :info
           :limit  [[1 5000]]
           :limit-by content-length
           :let [read-percent  (* 100 (float (/ bytes-read content-length)))]}
          ["Writing" item-count "files. Read" read-percent "% until now. Total length: " content-length]))

(defn app [context] (-> (fn [req] ((make-routes context) req))
                        wrap-keyword-params
                        (wrap-multipart-params {:progress-fn upload-progress})
                        wrap-params
                        (wrap-session {:store (cookie-store)})))

(defn get-random-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn start-server [context]
  (let [config (:config context)
        port (if (some? (-> (:server config) :port)) (-> (:server config) :port) (get-random-port))
        new-app (app context)]
    (t/log! :info [(str "Starting server: http://0.0.0.0:" port)])
    (reset! server
            (jetty/run-jetty (fn [req] (new-app req))
                             {:port        port
                              :join?       false
                              :min-threads 2}))))

(defn stop-server []
  (if-some [s ^Server @server]
    (do
      (let [port (.getPort (.getURI s))]
        (t/log! {:level :info} ["Stopping server on port" port]))
      (.stop s)
      (reset! server nil)
      nil)
    (do (t/log! :info "No server running.")
        nil)))
