(ns plauna.application
  (:require [plauna.interfaces :as int]
            [taoensso.telemere :as t]
            [clojure.core.async :as async]
            [plauna.core.email :as core-email]
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

(defn- success-result [result-type data] (conj {:result result-type} data))

(defn- error-result [exception alert-content] {:result :error :exception exception :message {:type :alert :content alert-content}})

(defn categories
  "There is no entry for 'no entry' in the database. This function adds a 'n/a' entry to the actual list."
  [db] (conj (int/fetch-categories db) {:id nil :name "n/a"}))

(defn connect-to-client
  "Returns {:result :ok} or {:result :redirect :provider provider} in case of oauth2"
  [{:keys [db client] :as context} id]
  (try
    (let [connection (int/fetch-connection db id)]
      (if (= "oauth2" (:auth-type connection))
        (let [auth-provider (int/fetch-auth-provider db (:auth-provider connection))
              oauth-data (int/fetch-oauth-token-data db id)]
          (cond
            (nil? auth-provider) (throw (ex-info "Auth type is 'oauth2' but there is no auth provider." {:connection connection}))
            (or (nil? oauth-data) (nil? (:access-token oauth-data)) (nil? (:refresh-token oauth-data)))
            (do
              (t/log! :warn ["Connection" (:user connection) (:host connection) "is set to use oauth2 but has no tokens in the db. You need to login manually from the 'Connections' page first."])
              (success-result :redirect {:provider (int/fetch-auth-provider db (:auth-provider connection))}))
            :else (do (int/start-monitor client connection context) (success-result :ok nil))))
        (do (int/start-monitor client connection context) {:result :ok})))
    (catch Exception e (do (t/log! :error ["There was an error when trying to log in:" e])
                           (error-result e "There was an error when trying to log in.")))))

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

(defn move-email-to-category
  "Email address of the recipient is usually the 'username' in the connection data. It may be different, if the user is using some kind of email masking service. If the email and the username match, we know where to look for. If not, we have to loop over the connections and try to find the email by id before moving it to its new directory. This all pressupposes that the message-id is really unique."
  [email category {:keys [client] :as context}]
  (let [connections (vals (int/connections client))
        connection-id-guess (int/connection-id-for-email client connections email)
        message-id (-> email :header :message-id)
        old-category (-> email :metadata :category)]
    (try
      (cond (nil? (seq connections))
            (error-result nil "There are no active connections.")

            (some? connection-id-guess)
            (do
              (t/log! :debug ["Email seems to belong to the connection with the id" connection-id-guess])
              (if (true? (int/move-email-between-categories client connection-id-guess message-id old-category category context))
                (success-result :ok nil)
                (error-result nil "Moving email failed. Please check the logs.")))

            :else
            (let [results (for [conn connections
                                :let [id (get-in conn [:config :id])]]
                            (do (t/log! :debug ["Move message-id" message-id])
                                (int/move-email-between-categories client id message-id old-category category context)))]
              (if (some true? results)
                (success-result :ok nil)
                (error-result nil "Moving email failed. Please check the logs."))))

      (catch Exception e (t/log! :error e) (error-result e "Moving email failed. Please check the logs.")))))

(defn- move-message [move? folder connection-id email-message category context]
  (if (and (true? move?) (some? category))
    (do (int/move-email-to-category (:client context) connection-id (:message email-message) folder category)
        (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was successfully moved to the corresponding folder"]))
    (do (t/log! :debug ["move option:" move? "category:" category "the email" (-> email-message :email :header :subject) "will not move moved"])
        :na)))

(defn- incoming-email-workflow [email-message connection-id folder {:keys [move? assigned-category assigned-category-id] :as options} {:keys [client analyzer db] :as context}]
  (if (some? assigned-category)
    (let [language-result (int/detect-language analyzer (:email email-message))
          enriched-email (core-email/construct-enriched-email (:email email-message) {:language (:code language-result) :language-confidence (:confidence language-result)} {:category assigned-category :category-id assigned-category-id :category-confidence 1})]
      (int/save-email db enriched-email)
      (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was successfully saved to the database"])
      (move-message move? folder connection-id email-message assigned-category context))
    (let [enriched-email (int/enrich-email analyzer (:email email-message))
          category (:category (:metadata enriched-email))]
      (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was categorized as" category])
      (int/save-email db enriched-email)
      (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was successfully saved to the database"])
      (move-message move? folder connection-id email-message category context))))

(defn handle-incoming-imap-email
  "Handle incoming emails synchronously on a single thread. Returns a result."
  [parsed-email {:keys [connection-id origin-folder message] :as options} context]
  (try (let [process-result (incoming-email-workflow {:email parsed-email :message message} connection-id origin-folder options context)]
         (success-result :ok {:move process-result}))
       (catch Exception e (error-result e "Error encountered when processing incoming email"))))

(defn read-emails-from-folder
  "Read all emails from a folder and process them. Returns the number of messages in the folder. Emails are processed on another thread."
  [connection-data folder-name options {:keys [client] :as context}]
  (let [messages-result (int/number-of-messages-in-folder client connection-data folder-name)
        folder (:folder messages-result)]
    (if (> (:message-count messages-result) 0)
      (do
        (t/log! :info ["There are" (:message-count messages-result) "emails in" folder-name "The messages will get processed asynchronously"])
        (async/go
          ;; reading email is index 1
          (doseq [n (range 1 (inc (:message-count messages-result)))
                  :let [email-message (int/nth-email-from-folder client n folder)]]
            (incoming-email-workflow email-message (:connection-id messages-result) folder options context))))
      (t/log! :info ["There are no emails in the folder. Doing nothing."]))
    (:message-count messages-result)))
