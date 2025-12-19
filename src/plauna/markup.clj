(ns plauna.markup
  (:require [clojure.data.json :as json]
            [selmer.parser :refer [render-file set-resource-path!]]
            [selmer.filters :refer [add-filter!]]
            [clojure.java.io :as io]
            [ring.util.codec :refer [base64-encode]]
            [scicloj.tableplot.v1.hanami :as hanami]
            [tablecloth.api :as tc]
            [tablecloth.column.api :as tcc]
            [tech.v3.datatype.datetime :as datetime])
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
  ([data] (render-file "admin.html" {:active-nav :admin :data data}))
  ([data messages] (render-file "admin.html" {:messages (mapv type->toast-role messages) :active-nav :admin :data data})))

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

(add-filter! :iconize (fn [pred] (if pred "✓" "⤫")))

(add-filter! :base64-encode (fn [^String string] (base64-encode (.getBytes string))))

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
     (render-file "emails.html" {:emails emails-with-java-date :page (conj page-info last-page) :header "Emails" :categories categories :active-nav :emails})))
  ([emails page-info categories messages]
   (let [last-page {:last-page (quot (:total page-info) (:size page-info))}
         emails-with-java-date (map #(update-in % [:header :date] timestamp->date) emails)]
     (render-file "emails.html" {:emails emails-with-java-date :page (conj page-info last-page) :header "Emails" :categories categories :messages (mapv type->toast-role messages) :active-nav :emails}))))

(defn list-email-contents [email-data categories]
  (render-file "email.html" {:email (update-in email-data [:header :date] timestamp->date) :categories categories :active-nav :emails}))

(defmacro pie-chart [data-values key key-label description]
  `{:data {:values ~data-values}
    :title ~description
    :description ~description
    :transform [{:joinaggregate [{:op "sum" :field :count :as :total}]}
                {:calculate (str "datum.count / datum.total < 0.1 ? 'others' : datum['" ~key "']") :as (keyword ~key)}
                {:aggregate [{:op :sum :field :count :as :count}] :groupby [(keyword ~key)]}]
    :layer [{:mark {:type :arc :outerRadius 10 :stroke "#fff"}}
            {:mark {:type :text :radiusOffset 30}
             :encoding {:text {:field (keyword ~key) :type "nominal"}}}]
    :encoding {:theta {:field :count :type "quantitative" :stack true}
               :radius {:field :count :scale {:type :sqrt :zero true :rangeMin 15}}
               :color {:field (keyword ~key) :legend nil}
               :tooltip [{:field (keyword ~key) :type "nominal" :title ~key-label}
                         {:field :count :type "nominal" :title "Count"}]}
    :config {:background nil}})

(defn transform-into-overview-dataset [dataset key]
  (-> (tc/group-by dataset [(keyword key)])
      (tc/aggregate {:count #(-> % :count tcc/sum)})
      (tc/join-columns :joined [:count (keyword key)] {:result-type :map})
      first
      second))

(defn statistics-overall [yearly-emails yearly-mime-types yearly-languages yearly-categories]
  (let [overall-email (if (seq yearly-emails)
                        (-> yearly-emails
                            (tc/dataset {:key-fn keyword})
                            (tc/map-columns :date #(datetime/instant->local-date-time (datetime/seconds-since-epoch->instant %)))
                            (tc/add-column :interval #(datetime/long-temporal-field :years (:date %)))
                            (tc/group-by [:interval])
                            (tc/aggregate {:total-count #(-> % :count tcc/sum)})
                            (hanami/plot hanami/bar-chart {:=background nil :=x :interval :=x-type :nominal :=x-title "Years" :=y :total-count :=y-title "Number of E-mails" :=title "Yearly E-Mails"}))
                        yearly-emails)
        mime-type-data (tc/dataset yearly-mime-types)
        mime-type-overview (if (seq mime-type-data)
                             (-> mime-type-data
                                 (transform-into-overview-dataset 'mime-type)
                                 (pie-chart 'mime-type "MIME TYPE" "MIME Types Overview"))
                             mime-type-data)
        mime-type-bar-data (if (seq mime-type-data)
                             (-> (tc/group-by mime-type-data [:mime-type])
                                 (tc/aggregate {:sum #(-> % :count tcc/sum)})
                                 (tc/order-by :sum :desc)
                                 (hanami/plot hanami/bar-chart {:=y :mime-type :=y-title "Mime Type" :=x :sum :=x-title "Total Count" :=title "Mime Types by Occurence" :=background nil :=y-sort nil :=x-sort nil})
                                 (update-in [:encoding :y] conj {:sort nil}))
                             mime-type-data)
        language-data (-> (filterv (fn [col] (some? (:interval col))) yearly-languages) tc/dataset)
        language-overview (-> language-data
                              (transform-into-overview-dataset 'language)
                              (pie-chart 'language "LANGUAGE" "Language Overview"))
        language-bar-data (hanami/plot language-data hanami/bar-chart {:=x :interval :=y :count :=background nil :=color :language :=x-title "Year" :=y-title "Lanuages"})

        categories-data  (-> (filterv (fn [col] (some? (:interval col))) yearly-categories) tc/dataset)
        categories-overview (-> categories-data
                                (transform-into-overview-dataset 'category)
                                (pie-chart 'category "Category" "Category Overview"))
        categories-bar-data (hanami/plot categories-data hanami/bar-chart {:=x :interval :=y :count :=background nil :=color :category :=x-title "Year" :=y-title "Categories"})]
    (render-file "statistics.html"
                 {:statistics [{:title "Overall"
                                :contents [{:type :bar-chart :header "" :id "emails" :json-data (json/write-str overall-email)}
                                           {:type :bar-chart :id "overview" :json-data (json/write-str mime-type-overview)}
                                           {:type :bar-chart :id "most-common" :json-data (json/write-str mime-type-bar-data)}]}
                               {:title "Languages"
                                :contents [{:type :bar-chart :id "languages-overview" :json-data (json/write-str language-overview)}
                                           {:type :bar-chart :id "languages" :json-data (json/write-str language-bar-data)}]}
                               {:title "Categories"
                                :contents [{:type :bar-chart :id "categories-overview" :json-data (json/write-str categories-overview)}
                                           {:type :bar-chart :id "categories" :json-data (json/write-str categories-bar-data)}]}]
                  :active-nav :statistics
                  :no-data (empty? yearly-emails)})))

(defn categories-page [categories] (render-file "admin-categories.html" {:categories categories :active-nav :admin}))

(defn languages-admin-page [language-preferences]
  (render-file "admin-languages.html" {:language-preferences language-preferences :active-nav :admin}))

(defn connections-list [connections]
  (render-file "admin-connections.html" {:configs connections :active-nav :admin}))

(defn connection
  ([config folders] (render-file "admin-connection.html" (merge config {:folders folders :active-nav :admin})))
  ([config folders messages] (render-file "admin-connection.html" (merge config {:folders folders :messages (mapv type->toast-role messages) :active-nav :admin}))))

(defn preferences-page [data] (let [log-levels {:log-level-options [{:key :error :name "Error"} {:key :info :name "Info"} {:key :debug :name "Debug"}] :active-nav :admin}]
                                (render-file "admin-preferences.html" (conj data log-levels))))

(defn new-connection [providers]
  (render-file "admin-new-connection.html" {:auth-providers providers}))
