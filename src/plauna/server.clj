(ns plauna.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.codec :refer [url-decode]]
            [plauna.markup :as markup]
            [plauna.files :as files]
            [plauna.util.page :as page]
            [plauna.client :as client]
            [plauna.core.email :as core-email]
            [clojure.math :refer [ceil]]
            [compojure.core :as comp]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [taoensso.telemere :as t]
            [clojure.java.io :as io]
            [plauna.analysis :as analysis]
            [plauna.database :as db]
            [plauna.messaging :as messaging]
            [clojure.core.async :as async])
  (:import [java.net ServerSocket]
           [org.eclipse.jetty.server Server]))

(set! *warn-on-reflection* true)

(defonce server (atom nil))

(def html-headers {"Content-Type" "text/html; charset=UTF-8"})

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

(defn find-running-clients []
  (map (fn [watcher] {(first watcher) (client/monitor->map (second watcher))}) @client/watchers))

(defn redirect-request [request]
  (let [redirect-url (-> request :params :redirect-url)]
    (if (some? redirect-url)
      {:status 301 :headers {"Location" redirect-url}}
      {:status 301 :headers {"Location" (-> request :uri)}})))

(defn language-preferences []
  (let [preferences (db/get-language-preferences)]
    (if (empty? (db/get-languages))
      []
      (if (empty? preferences)
        (do (db/add-language-preferences (map (comp #(conj % false) vector :language) (db/get-languages)))
            (db/get-language-preferences))
        preferences))))

(defn params->interval-request [params]
  (if (and (some? (:interval params)) (some? (:year params)))
    {:interval (keyword (:interval params)) :year (Integer/parseInt (get params :year))}
    {:interval :yearly}))

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
  (write-all-categorized-emails-to-training-files)
  (doseq [training-model (analysis/train-data (files/training-files))]
    (let [os (io/output-stream (files/model-file (:language training-model)))]
      (analysis/serialize-model! (:model training-model) os))))

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

(defn email-type-statistics-overview []
  (db/query-db {:select [[[:count :headers.message-id] :count] :bodies.mime-type] :from [:bodies]
                :join [:headers [:= :bodies.message-id :headers.message_id]]
                :group-by [:bodies.mime-type]
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

(defn email-from-count
  ([] (db/query-db {:select [[[:count :address] :count] :address] :from [:communications]
                    :join [:contacts [:= :contacts.contact-key :communications.contact-key]
                           :headers [:= :communications.message-id :headers.message_id]]
                    :group-by [:address]
                    :where [:and [:= :type ":sender"] [:not-in :address @db/my-addresses]]
                    :limit 10
                    :order-by [[:count :desc]]}))
  ([year] (db/query-db {:select [[[:count :address] :count] :address [(db/interval-for-honey :yearly) :interval]] :from [:communications]
                        :join [:contacts [:= :contacts.contact-key :communications.contact-key]
                               :headers [:= :communications.message-id :headers.message_id]]
                        :group-by [:address]
                        :where [:and [:= :type ":sender"] [:like :interval year] [:not-in :address @db/my-addresses]]
                        :limit 10
                        :order-by [[:count :desc]]})))

(defn email-from-statistics [interval-request]
  (let [where-clause (if (and (some? (:year interval-request)) (not (= (:interval interval-request) :yearly)))
                       [:and [:= :type ":sender"] [:like :interval (str (:year interval-request) "%")] [:not-in :address @db/my-addresses] [:<> :interval nil]]
                       [:and [:= :type ":sender"] [:not-in :address @db/my-addresses]])]
    (db/query-db {:select [[[:count :address] :count] [(db/interval-for-honey (:interval interval-request)) :interval]] :from [:communications]
                  :join [:contacts [:= :contacts.contact-key :communications.contact-key]
                         :headers [:= :communications.message-id :headers.message_id]]
                  :group-by [:interval]
                  :where where-clause})))

(defn enriched-email-by-message-id [id] (first (db/fetch-data {:entity :enriched-email :strict false} {:where [:= :message-id id]})))

;; TODO change name template
(def emails-template {:page-size {:default 20 :type-fn Integer/parseInt}
                      :page {:default 1 :type-fn Integer/parseInt}
                      :filter {:default "all" :type-fn (fn [x] x)}})

(defn template->request-parameters [template]
  (fn [rp] (reduce (fn [acc [k v]] (if (contains? rp k)
                                     (conj acc {k ((:type-fn v) (get rp k))})
                                     (conj acc {k (:default v)})))
                   {} template)))

(defn filter->sql-clause [filter]
  (cond
    (= filter "enriched-only") {:where [:and [:<> :metadata.category nil] [:<> :metadata.language nil]] :order-by [[:date :desc]]}
    (= filter "without-category") {:where [:= :metadata.category nil] :order-by [[:date :desc]]}
    :else {:order-by [[:date :desc]]}))

(comp/defroutes routes

  (route/resources "/")

  (comp/GET "/" {} (success-html-with-body (markup/administration)))

  (comp/GET "/admin" {}
    (success-html-with-body (markup/administration)))

  (comp/POST "/emails/parse" request
    (let [temp-file (:tempfile (:filename (:params request)))
          result (files/read-emails-from-mbox (io/input-stream temp-file) @messaging/main-chan)]
      (redirect-request request)))

  (comp/GET "/admin/categories" {}
    (let [categories (db/get-categories)]
      (success-html-with-body (markup/categories-page categories))))

  (comp/GET "/admin/languages" {}
    (success-html-with-body
     (markup/languages-admin-page (language-preferences))))

  (comp/GET "/admin/preferences" {}
    (let [language-datection-threshold (analysis/language-detection-threshold)
          categorization-threshold (analysis/categorization-threshold)]
      (success-html-with-body (markup/preferences-page {:language-detection-threshold language-datection-threshold :categorization-threshold categorization-threshold}))))

  (comp/POST "/admin/preferences" request
    (doseq [param (dissoc (:params request) :redirect-url)]
      (db/update-preference (first param) (second param)))
    (redirect-request request))

  (comp/POST "/admin/languages" {params :params}
    (let [langs-to-use (if (vector? (:use params)) (:use params) [(:use params)])]
      (doseq
       [preference (mapv (fn [id language]
                           {:id id :language language :use (some? (some #(= language %) langs-to-use))})
                         (vectorize (:id params))
                         (vectorize (:language params)))]
        (db/update-language-preference preference)))
    (let [language-preferences (language-preferences)]
      (success-html-with-body (markup/languages-admin-page language-preferences))))

  (comp/POST "/admin/categories" {params :params}
    (db/create-category (:name params))
    {:status  301
     :headers {"Location" "/admin/categories"}
     :body    (markup/administration)})

  (comp/DELETE "/admin/categories/:id" {route-params :route-params}
    (db/delete-category-by-id (:id route-params))
    {:status  301
     :headers {"Location" "/admin/categories"}
     :body    (markup/administration)})

  (comp/DELETE "/admin/database" {}
    (files/delete-database-file)
    {:status  301
     :headers {"Location" "/admin"}
     :body    (markup/administration)})

  (comp/POST "/admin/database" {}
    (files/check-and-create-database-file)
    (db/create-db)
    {:status  301
     :headers {"Location" "/admin"}
     :body    (markup/administration)})

  (comp/DELETE "/admin/training" {}
    (files/delete-files-with-type :train)
    {:status  301
     :headers {"Location" "/admin"}
     :body    (markup/administration)})

  (comp/DELETE "/admin/models" {}
    (files/delete-files-with-type :model)
    {:status  301
     :headers {"Location" "/admin"}
     :body    (markup/administration)})

  (comp/GET "/statistics" {}
    (success-html-with-body (markup/statistics-overall (db/yearly-email-stats))))

  (comp/GET "/statistics/types" {params :params}
    (let [period (keyword (get params :period :yearly))
          yearly-mime-types (mime-type-statistics period)]
      (success-html-with-body (markup/statistics-types (email-type-statistics-overview) yearly-mime-types))))

  (comp/GET "/statistics/contacts" {params :params}
    (let [interval-request (params->interval-request params)
          years (db/years-of-data)
          contact-count (if (some? (:year params)) (email-from-count (:year params)) (email-from-count))
          contacts-over-interval (email-from-statistics interval-request)]
      (success-html-with-body
       (markup/statistics-contacts {:years             years
                                    :selected-interval (get params :interval :yearly)
                                    :selected-year     (get params :year)}
                                   contact-count contacts-over-interval))))

  (comp/GET "/statistics/languages" {}
    (let [yearly-languages (language-statistics-by-period :yearly)]
      (success-html-with-body
       (markup/statistics-languages yearly-languages))))

  (comp/GET "/statistics/categories" {params :params}
    (let [selected-interval (params->interval-request params)
          categories-stats (category-statistics-by-period selected-interval)
          years (db/years-of-data)]
      (success-html-with-body
       (markup/statistics-categories categories-stats
                                     {:years             years
                                      :selected-interval selected-interval
                                      :selected-year     (get params :year)}))))

  (comp/POST "/metadata" request
    (save-metadata-form (:params request))
    (redirect-request request))

  (comp/POST "/training" request
    (write-emails-to-training-files-and-train)
    (redirect-request request))

  (comp/POST "/training/new" request
    (let [n (get (:route-params request) :new 20)]
      (categorize-uncategorized-n-emails n)
      (redirect-request request)))

  (comp/GET "/emails" {params :params}
    (let [parse-fn (template->request-parameters emails-template)
          {page-size :page-size page :page filter :filter} (parse-fn params)
          categories (conj (db/get-categories) {:id nil :name "n/a"})
          sql-clause (filter->sql-clause filter)
          result (db/fetch-data {:entity :enriched-email :strict false :page (page/page-request page page-size)} sql-clause)]
      {:status 200
       :header html-headers
       :body   (markup/list-emails (:data result) {:filter filter :total-pages (inc (int (ceil (quot (:total result) page-size)))) :size page-size :page (:page result) :total (:total result)} categories)}))

  (comp/GET "/emails/:id" [id]
    (let [decoded-id (url-decode id)
          email-data (enriched-email-by-message-id decoded-id)
          categories (conj (db/get-categories) {:id nil :name "n/a"})]
      {:status 200
       :header html-headers
       :body   (markup/list-email-contents email-data categories)}))

  (comp/GET "/watchers" []
    {:status 200
     :header html-headers
     :body   (markup/watcher-list (find-running-clients))})

  (comp/GET "/watchers/:id" [id]
    {:status 200
     :header html-headers
     :body   (markup/watcher (first (client/find-by-id-in-watchers id))
                             (client/folders-in-store (:store (second (first (client/find-by-id-in-watchers id))))))})

  (comp/POST "/watchers/:id" request
    (let [params (:params request)
          id (:id params)
          folder (:folder params)
          refolder (some? (:move params))]
      (client/read-all-emails id folder {:refolder refolder}))
    (redirect-request request))

  (comp/GET "/watchers/:id/restart" request
    (let [id (:id (:params request))]
      (client/connect-using-id id))
    {:status 301 :headers {"Location" "/watchers"}})

  (comp/POST "/metadata/languages" request
    (let [limiter (messaging/channel-limiter :enriched-email)
          process-fn (fn [enriched-emails]
                       (doseq [enriched-email enriched-emails]
                         (async/>!! limiter :token)
                         (async/>!! @messaging/main-chan {:type :language-detection-request :options {} :payload enriched-email})))]
      (core-email/iterate-over-all-pages db/fetch-data process-fn {:entity :enriched-email :strict false :page {:page 1 :size 500}} {:where [:= :language nil]} true))
    (redirect-request request))

  (route/resources "/"))

(defn upload-progress [_ bytes-read content-length item-count]
  (t/log! {:level       :info
           :sample-rate 0.5
           :rate-limit  {"1 per 10 sec" [1 10000]}}
          ["Writing" item-count "files. Read" (* 100 (float (/ bytes-read content-length))) "% until now. Total length: " content-length]))

(def app (-> (fn [req] (routes req))
             wrap-keyword-params
             (wrap-multipart-params {:progress-fn upload-progress})
             wrap-params))

(defn get-random-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn start-server [config]
  (let [port (if (some? (-> (:server config) :port)) (-> (:server config) :port) (get-random-port))]
    (t/log! :info [(str "Starting server: http://localhost:" port)])
    (reset! server
            (jetty/run-jetty (fn [req] (app req))
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
