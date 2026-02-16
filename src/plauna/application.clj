(ns plauna.application
  (:require [plauna.interfaces :as int]
            [taoensso.telemere :as t]
            [plauna.util.page :as page]))

(defn- filter->sql-clause [filter]
  (cond
    (= filter "enriched-only") {:where [:and [:<> :metadata.category nil] [:<> :metadata.language nil]] :order-by [[:date :desc]]}
    (= filter "without-category") {:where [:= :metadata.category nil] :order-by [[:date :desc]]}
    :else {:order-by [[:date :desc]]}))

(defn- search->sql-clause [search-field search-text]
  (cond
    (= search-field "subject") {:where [:like :headers.subject (str "%" search-text "%")] :order-by [[:date :desc]]}
    :else {:order-by [[:date :desc]]}))

(defn- combine-maps-with [map1 map2 key combination-key]
  (let [val1 (get map1 key)
        val2 (get map2 key)]
    (cond (nil? val1) map2
          (nil? val2) map1
          :else (conj map1 {key [combination-key val1 val2]}))))

(defn categories
  "There is no entry for 'no entry' in the database. This function adds a 'n/a' entry to the actual list."
  [db] (conj (int/fetch-categories db) {:id nil :name "n/a"}))

(defn connect-to-client
  "Returns {:result :ok} or {:result :redirect :provider provider} in case of oauth2"
  [context id]
  (try
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
        (do (int/start-monitor client connection) {:result :ok})))
    (catch Exception e (do (t/log! :error ["There was an error when trying to log in:" e])
                           {:result :error :exception e}))))

(defn fetch-emails
  "Returns a list of emails. Customizable by parameters which can contain the following keys:
   :size, :page, :filter (all, enrieched-only, or without-category), :search-field (subject), :search-text"
  [context parameters]
  (let [db (:db context)
        cat-list (categories db)
        customization-clause (combine-maps-with (filter->sql-clause (:filter parameters)) (search->sql-clause (:search-field parameters) (:search-text parameters)) :where :and)
        result (int/fetch-emails db {:entity :enriched-email :strict false :page (page/page-request (:page parameters) (:size parameters))} customization-clause)]
    {:data (:data result)
     :parameters {:filter (:filter parameters)
                  :total-pages (page/calculate-pages-total (:total result) (:size parameters))
                  :size (:size parameters)
                  :page (:page result)
                  :total (:total result)
                  :search-text (:search-text parameters)}
     :optional {:categories cat-list}}))

(defn create-new-category! [context category]
  (let [db (:db context)
        client (:client context)]
    (int/save-category db category)
    (doseq [connection-data (vals (int/connections client))]
      (int/create-category-directories! client connection-data [category]))))
