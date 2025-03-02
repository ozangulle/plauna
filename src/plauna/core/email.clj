(ns plauna.core.email
  (:require [clojure.math :as math]
            [clojure.string :as s]))

(set! *warn-on-reflection* true)

(defrecord Header [message-id in-reply-to subject mime-type date])

(defrecord Body-Part [message-id charset mime-type transfer-encoding content filename content-disposition])

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

(defn attachment? [body-part] (or (= "attachment" (:content-disposition body-part)) (some? (:filename body-part))))

(defn text-content? [mime-type] (.startsWith ^String mime-type "text"))

(defn body-text-content? [body-part] (text-content? (:mime-type body-part)))

(defn text-content-type [body-part]
  (let [mime-type (:mime-type body-part)]
    (cond (.endsWith ^String mime-type "html") :html
          (.endsWith ^String mime-type "rtf") :rtf
          :else :plain)))

(defn body-part-for-mime-type
  "When supplied with an e-mail, select a mime-type and extract its contents for training purposes.
  If the selected mime-type does not exist, it returns the text content of the first mime-type available.
  If the e-mail has no body, returns nil."
  [mime-type email]
  (let [body-parts (filter #(and (not (attachment? %)) (body-text-content? %)) (:body email))]
    (cond (empty? (:body email)) ;; is this possible?
          nil
          (= 1 (count body-parts))
          (first body-parts)
          :else
          (let [first-match (first (filter #(.equals ^String (:mime-type %) mime-type) body-parts))]
            (if (some? first-match) first-match (first body-parts))))))
