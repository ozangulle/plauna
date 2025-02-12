(ns plauna.markup
  (:require [clojure.data.json :as json]
            [selmer.parser :refer [render-file set-resource-path!]]
            [selmer.filters :refer [add-filter!]]
            [clojure.java.io :as io])
  (:import
   (java.time LocalDateTime ZoneOffset)))

(set! *warn-on-reflection* true)

(set-resource-path! (io/resource "templates"))

(comment
  (selmer.parser/cache-off!))

(defn timestamp->date [timestamp]
  (if (nil? timestamp)
    nil
    (. LocalDateTime ofEpochSecond timestamp 0 ZoneOffset/UTC)))

(defn type->toast-role [message]
  (cond
    (= :alert (:type message)) (conj message {:path "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5Zm3.707 11.793a1 1 0 1 1-1.414 1.414L10 11.414l-2.293 2.293a1 1 0 0 1-1.414-1.414L8.586 10 6.293 7.707a1 1 0 0 1 1.414-1.414L10 8.586l2.293-2.293a1 1 0 0 1 1.414 1.414L11.414 10l2.293 2.293Z"
                                              :color "text-red-500"
                                              :bg-color "bg-red-100"
                                              :id (str "toast-" (hash message))})
    (= :success (:type message)) (conj message {:path "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5Zm3.707 8.207-4 4a1 1 0 0 1-1.414 0l-2-2a1 1 0 0 1 1.414-1.414L9 10.586l3.293-3.293a1 1 0 0 1 1.414 1.414Z"
                                                :color "text-green-500"
                                                :bg-color "bg-green-100"
                                                :id (str "toast-" (hash message))})
    (= :info (:type message)) (conj message {:path "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5ZM10 15a1 1 0 1 1 0-2 1 1 0 0 1 0 2Zm1-4a1 1 0 0 1-2 0V6a1 1 0 0 1 2 0v5Z"
                                             :color "text-orange-500"
                                             :bg-color "bg-orange-100"
                                             :id (str "toast-" (hash message))})
    :else message))

(defn administration
  ([] (render-file "admin.html" {}))
  ([messages] (render-file "admin.html" {:messages (mapv type->toast-role messages)})))

(defn concat-string [contact]
  (if (nil? (:name contact))
    (:address contact)
    (str (:name contact) " - " (:address contact))))

(defn concat-contacts
  ([key contacts]
   (->> contacts
        (filter (fn [contact] (= (:type contact) key)))
        (reduce (fn [acc el]
                  (if (empty? acc)
                    (str acc (concat-string el))
                    (str acc ", " (concat-string el))))
                "")))
  ([contacts]
   (reduce (fn [acc el] (if (empty? acc)
                          (str acc (:address el))
                          (str acc ", " (:address el)))) "" contacts)))

(add-filter! :concat-senders (partial concat-contacts :sender))

(add-filter! :concat-receivers (partial concat-contacts :receiver))

(add-filter! :concat-cc (partial concat-contacts :cc))

(add-filter! :concat-bcc (partial concat-contacts :bcc))

(add-filter! :double-format-nillable (fn [n & [decimal-places]]
                                       (if (nil? n)
                                         0
                                         (let [n (double n)]
                                           (format (str "%." (if decimal-places decimal-places "1") "f")
                                                   n)))))

(defn list-emails
  ([emails page-info categories]
   (let [last-page {:last-page (quot (:total page-info) (:size page-info))}
         emails-with-java-date (map #(update-in % [:header :date] timestamp->date) emails)]
     (render-file "emails.html" {:emails emails-with-java-date :page (conj page-info last-page) :header "Emails" :categories categories})))
  ([emails page-info categories messages]
   (let [last-page {:last-page (quot (:total page-info) (:size page-info))}
         emails-with-java-date (map #(update-in % [:header :date] timestamp->date) emails)]
     (render-file "emails.html" {:emails emails-with-java-date :page (conj page-info last-page) :header "Emails" :categories categories :messages (mapv type->toast-role messages)}))))

(defn list-email-contents [email-data categories]
  (render-file "email.html" {:email (update-in email-data [:header :date] timestamp->date) :categories categories}))

(defn statistics-contacts [intervals top-from interval-from]
  (let [vega-interval-from {:data {:values interval-from}
                            :mark "bar"
                            :transform [{:filter "datum.interval != null"}
                                        {:aggregate [{:op "sum" :field :count :as :sum}] :groupby [:interval]}]
                            :encoding {:y {:field :sum :type "quantitative"}
                                       :x {:field :interval :type "ordinal" :axis {:labelOverlap "parity" :labelSeparation 10}}
                                       :tooltip {:field :sum :type "nominal"}}}]
    (render-file "statistics.html"
                 {:interval-filter (conj {:url "/statistics/contacts"} intervals)
                  :statistics
                  [{:type :table :header "Top 10 Senders of E-Mails" :data {:headers ["Count" "Address"] :values (map vals top-from)}}
                   {:type :bar-chart :header "Number of Senders per Interval" :id "senders" :json-data (json/write-str vega-interval-from)}]})))

(defn statistics-types [overview-map yearly-mime-types]
  (let [vega-most-common {:data {:values yearly-mime-types}
                          :description "Most common mime types"
                          :mark "bar"
                          :transform [{:aggregate [{:op "sum" :field :count :as :sum}] :groupby [:mime-type]}
                                      {:window [{:op "rank" :as :rank}] :sort [{:field :sum :order "descending"}]}
                                      {:filter "datum.rank <= 50"}]
                          :encoding {:x {:field :mime-type :type "nominal" :title "Mime types" :sort "-y"}
                                     :y {:field :sum :type "quantitative" :title "Count"}
                                     :tooltip [{:field :sum :type "quantitative"} {:field :mime-type :type "nominal"}]}}
        vega-least-common {:data {:values yearly-mime-types}
                           :description "Least common mime types"
                           :mark "bar"
                           :transform [{:aggregate [{:op "sum" :field :count :as :sum}] :groupby [:mime-type]}
                                       {:filter "datum.sum <= 50"}]
                           :encoding {:x {:field :mime-type :type "nominal" :title "Mime types" :sort "-y"}
                                      :y {:field :sum :type "quantitative" :title "Count"}
                                      :tooltip [{:field :sum :type "quantitative"} {:field :mime-type :type "nominal"}]}}]
    (render-file "statistics.html"
                 {:statistics
                  [{:type :table :header "All MIME Types" :data {:headers ["Count" "Mime Type"] :values (map vals overview-map)}}
                   {:type :bar-chart :header "Most Common MIME Types" :id "most-common" :json-data (json/write-str vega-most-common)}
                   {:type :bar-chart :header "Least Common MIME Types" :id "least-common" :json-data (json/write-str vega-least-common)}]})))

(defn statistics-languages [yearly-languages]
  (let [vega-data {:data {:values yearly-languages}
                   :mark {:type "bar" :tooltip true}
                   :encoding {:y {:field :count :type "quantitative"}
                              :x {:field :interval :type "ordinal" :axis {:labelOverlap "parity" :labelSeparation 10}}
                              :color {:field :language :type "nominal"}}}]
    (render-file "statistics.html" {:statistics [{:type :bar-chart :header "Yearly Languages" :id "languages" :json-data (json/write-str vega-data)}]})))

(defn statistics-categories [yearly-categories intervals]
  (let [vega-data {:data {:values yearly-categories}
                   :mark {:type "bar" :tooltip true}
                   :transform [{:filter "datum.interval != null"}]
                   :encoding {:y {:field :count :type "quantitative"}
                              :x {:field :interval :type "ordinal" :axis {:labelOverlap "parity" :labelSeparation 10}}
                              :color {:field :category :type "nominal"}}}]
    (render-file "statistics.html" {:interval-filter (conj {:url "/statistics/categories"} intervals) :statistics [{:type :bar-chart :header "Categories" :id "emails" :json-data (json/write-str vega-data)}]})))

(defn statistics-overall [yearly-emails]
  (let [vega-data {:data {:values yearly-emails}
                   :mark "bar"
                   :transform [{:filter "datum.date != null"}]
                   :encoding {:y {:field :count :type "quantitative"}
                              :x {:field :date :type "ordinal" :axis {:labelOverlap "parity" :labelSeparation 10}}
                              :tooltip {:field :count :type "quantitative"}}}]
    (render-file "statistics.html" {:statistics [{:type :bar-chart :header "Yearly Emails" :id "emails" :json-data (json/write-str vega-data)}]})))

(defn categories-page [categories] (render-file "admin-categories.html" {:categories categories}))

(defn languages-admin-page [language-preferences]
  (render-file "admin-languages.html" {:language-preferences language-preferences}))

(defn watcher-list [clients]
  (let [watchers (map (fn [client] {:id (-> client first first :id) :logged-in (-> client first second :connected) :folder-open (-> client first second :folder) :string (str (-> client first first :host) " - " (-> client first first :user))}) clients)]
    (render-file "watchers.html" {:watchers watchers})))

(defn watcher
  ([client folders] (render-file "watcher.html" {:id (-> client first :id) :host (:host (first client)) :user (:user (first client)) :folders folders}))
  ([client folders messages] (println messages) (render-file "watcher.html" {:id (-> client first :id) :host (:host (first client)) :user (:user (first client)) :folders folders :messages (mapv type->toast-role messages)})))

(defn preferences-page [data] (let [log-levels {:log-level-options [{:key :error :name "Error"} {:key :info :name "Info"} {:key :debug :name "Debug"}]}]
                                (render-file "admin-preferences.html" (conj data log-levels))))
