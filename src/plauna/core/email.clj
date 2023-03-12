(ns plauna.core.email
  (:require [clojure.math :as math]
            [clojure.string :as s]
            [clojure.spec.alpha :as sp]
            [plauna.specs :as email-specs]
            ))

(set! *warn-on-reflection* true)

(defrecord Header [message-id in-reply-to subject mime-type date])

(defrecord Body-Part [message-id charset mime-type transfer-encoding original-content sanitized-content name])

(defrecord Participant [address name contact-key type message-id])

(defrecord Email [^Header header body participants])

(defrecord Metadata [message-id language language-modified language-confidence category category-id category-modified category-confidence])

(defrecord EnrichedEmail [^Header header body participants ^Metadata metadata])

(defrecord EnrichedBodyPart [^Body-Part body-part ^Metadata metadata])

(defn construct-body-part [body-part] (map->Body-Part body-part))

(defn construct-participants [participant]
  (let [args ((juxt :address :name :contact-key :type :message-id) participant)]
    (if (keyword? (get args 3))
      (apply ->Participant args)
      (apply ->Participant (assoc args 3 (keyword (s/replace-first (get args 3) ":" "")))))))

(defn construct-header [header] (map->Header header))

(defn construct-email [raw-header raw-body raw-participants]
  ;(sp/conform ::email-specs/body raw-body)
  (let [body-parts (map construct-body-part raw-body)
        participants (map construct-participants raw-participants)
        header (construct-header raw-header)]
    (->Email header body-parts participants)))

(defn construct-enriched-email [email language-metadata category-metadata]
  (->EnrichedEmail (:header email)
                   (:body email)
                   (:participants email)
                   (->Metadata (-> email :header :message-id)
                               (:language language-metadata)
                               (get language-metadata :language-modified nil)
                               (:language-confidence language-metadata)
                               (:category category-metadata)
                               (:category-id category-metadata)
                               (get category-metadata :category-modified nil)
                               (:category-confidence category-metadata))))

(defn iterate-over-all-pages [call-with-pagination fun query sql-query mutates?]
  (let [data-with-current-page (call-with-pagination query sql-query)
        remaining-pages (-> (/ (:total data-with-current-page) (:size (:page query)))
                            math/ceil
                            (- (:page data-with-current-page)))]
    (fun (:data data-with-current-page))
    (if (> remaining-pages 0)
      (recur call-with-pagination fun (if mutates? query (update-in query [:page :page] inc)) sql-query mutates?)
      nil)))

(defn training-content
  "When supplied with an e-mail, select a mime-type and extract its contents for training purposes.
  If the selected mime-type does not exist, it returns the text content of the first mime-type available.
  If the e-mail has no body, returns nil."
  [mime-type email]
  (let [body-parts (filter #(or (= (:mime-type %) "text/html") (= (:mime-type %) "text/plain")) (:body email))]
    (cond (empty? (:body email))
          nil
          (= 1 (count body-parts))
          {:message-id (-> email :header :message-id)
           :training-content (:sanitized-content (first body-parts))
           :language (-> email :metadata :language)
           :category (-> email :metadata :category)
           :subject (-> email :header :subject)}
          :else
          {:message-id (-> email :header :message-id)
           :training-content (:sanitized-content (first (filter #(.equals ^String (:mime-type %) mime-type) body-parts)))
           :language (-> email :metadata :language)
           :category (-> email :metadata :category)
           :subject (-> email :header :subject)})))
