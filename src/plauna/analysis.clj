(ns plauna.analysis
  (:require [clojure.string :as st]
            [plauna.database :as db]
            [plauna.core.events :as events]
            [plauna.core.email :as core-email]
            [plauna.preferences :as p]
            [plauna.util.text-transform :as tt]
            [clojure.core.async :as async]
            [taoensso.telemere :as t]
            [cld.core :as lang]
            [plauna.files :as files])
  (:import
   (opennlp.tools.util.normalizer AggregateCharSequenceNormalizer NumberCharSequenceNormalizer ShrinkCharSequenceNormalizer CharSequenceNormalizer)
   (opennlp.tools.util MarkableFileInputStreamFactory PlainTextByLineStream TrainingParameters)
   (opennlp.tools.doccat DocumentSampleStream DocumentCategorizerME DoccatFactory DoccatModel)
   (opennlp.tools.ml.naivebayes NaiveBayesTrainer)
   (opennlp.tools.ml.maxent GISTrainer)
   (java.util Locale)
   (java.util.regex Pattern)
   (java.io File OutputStream)))

(set! *warn-on-reflection* true)

(def BracketsNormalizer (reify CharSequenceNormalizer
                          (normalize [_ text] ((comp st/trim #(st/replace % #"\( \)" "")) text))))

(def MailtoNormalizer (reify CharSequenceNormalizer
                        (normalize [_ text] ((comp st/trim #(st/replace % #"(mailto:)?(?<![-+_.0-9A-Za-z])[-+_.0-9A-Za-z]+@[-0-9A-Za-z]+[-.0-9A-Za-z]+" "")) text))))

(def BetterURLNormalizer (reify CharSequenceNormalizer
                           (normalize [_ text] ((comp st/trim #(st/replace % #"https?://[-_.?&~%;+=/#0-9A-Za-z]+" "")) text))))

(def NonCharNormalizer (reify CharSequenceNormalizer
                         (normalize [_ text] (#(st/replace % (Pattern/compile "[^\\s\\w]" Pattern/UNICODE_CHARACTER_CLASS) " ") text))))

(def NonPrintableCharNormalizer (reify CharSequenceNormalizer
                                  (normalize [_ text] (#(st/replace % (Pattern/compile "\\p{C}") " ") text))))

(def ^CharSequenceNormalizer normalizer (new AggregateCharSequenceNormalizer
                                             (into-array CharSequenceNormalizer
                                                         [BetterURLNormalizer
                                                          MailtoNormalizer
                                                          (NumberCharSequenceNormalizer/getInstance)

                                                          BracketsNormalizer
                                                          NonPrintableCharNormalizer
                                                          NonCharNormalizer
                                                          (ShrinkCharSequenceNormalizer/getInstance)])))

(defn normalize [^String text] (.normalize normalizer text))

(defn categorization-algorithm ^String [] (or (:categorization-algorithm (db/fetch-preference :categorization-algorithm)) NaiveBayesTrainer/NAIVE_BAYES_VALUE))

(lang/default-init!)

(defn lang-code-set3 [language]
  (.getISO3Language (new Locale language)))

(defn detect-language [^String text]
  (when (some? text)
    (if (> (count text) 3)
      (let [result (second (lang/detect text))
            confidence (Double/parseDouble (first (vals result)))
            lang-code (lang-code-set3 (first (keys result)))]
        {:code (if (< confidence (p/language-detection-threshold)) "n/a" lang-code)
         :confidence confidence})
      {:code "n/a" :confidence 0.0})))

(defn training-data-stream [file]
  (-> (MarkableFileInputStreamFactory. file)
      (PlainTextByLineStream. "UTF-8")
      (DocumentSampleStream.)))

(defn training-body-part [email] (core-email/body-part-for-mime-type "text/html" email))

(defn format-training-data [data]
  (transduce
   (comp (map (fn [email] [(get-in email [:metadata :category]) (training-body-part email)]))
         (map (fn [[category body-part]] [category
                                          (if (some? (:subject body-part)) (st/trim (:subject body-part)) "")
                                          (tt/clean-text-content (:content body-part) (core-email/text-content-type body-part))]))
         (map (fn [[category subject text-content]] [category subject (normalize text-content)]))
         (map (fn [[category subject normalized-content]] (str category " " subject " " normalized-content "\n"))))
   str
   ""
   data))

(defn training-parameters []
  (doto (new TrainingParameters)
    (.put TrainingParameters/ITERATIONS_PARAM 1000)
    (.put TrainingParameters/CUTOFF_PARAM 0)
    (.put TrainingParameters/ALGORITHM_PARAM (categorization-algorithm))))

(comment NaiveBayesTrainer/NAIVE_BAYES_VALUE
         GISTrainer/MAXENT_VALUE
         "")

(defn serialize-and-write-model! [^DoccatModel model ^OutputStream os]
  (when (some? model) (.serialize model os)))

(defn train-data [training-files]
  (for [tf training-files]
    (try
      {:model
       (DocumentCategorizerME/train (:language tf) (training-data-stream (:file tf)) (training-parameters) (DoccatFactory.))
       :language (:language tf)}
      (catch Exception e (t/log! {:level :error :error e} (.getMessage e))))))

(defn categorize [text ^File model-file]
  (if (.exists model-file)
    (let [doccat (DocumentCategorizerME. (DoccatModel. model-file))
          cat-results (.categorize doccat (into-array String (st/split text #" ")))
          best-category (.getBestCategory doccat cat-results)
          best-probability (get cat-results (.getIndex doccat best-category))]
      (if (> best-probability (p/categorization-threshold))
        {:name best-category :confidence best-probability}
        {:name nil :confidence 0}))
    {:name nil :confidence 0}))

(defn normalize-body-part [body-part]
  (when (some? body-part)
    (normalize (tt/clean-text-content (:content body-part) (core-email/text-content-type body-part)))))

(defn category-for-text [text language-code]
  (when (and (some? text) (some? language-code))
    (let [allowed-languages (mapv :language (filter #(= 1 (:use_in_training %)) (db/get-language-preferences)))]
      (when (some #(= language-code %) allowed-languages) (categorize text (files/model-file language-code))))))

(defn detect-language-and-categorize-event [event]
  (let [email (:payload event)
        body-part-to-train-on (core-email/body-part-for-mime-type "text/html" email)
        training-content (normalize-body-part body-part-to-train-on)
        language-result (detect-language training-content)
        category-result (category-for-text training-content (:code language-result))
        category-id (if (nil? (:name category-result)) nil (:id (db/category-by-name (:name category-result))))]
    (core-email/construct-enriched-email email {:language (:code language-result) :language-confidence (:confidence language-result)} {:category (:name category-result) :category-confidence (:confidence category-result) :category-id category-id})))

(defn detect-language-event [event]
  (let [email (:payload event)
        body-part-to-train-on (core-email/body-part-for-mime-type "text/html" email)
        training-content (normalize-body-part body-part-to-train-on)
        language-result (try (detect-language training-content) (catch Exception e (t/log! {:level :error :error e} [(.getMessage e) "\nText causing the exception:" training-content])))]
    (core-email/construct-enriched-email email {:language (:code language-result) :language-confidence (:confidence language-result)} {:category (-> email :metadata :category) :category-confidence (-> email :metadata :category-confidence) :category-id (-> email :metadata :category-id)})))

(defmulti handle-enrichment :type)

(defmethod handle-enrichment :parsed-enrichable-email [event]
  (events/create-event :enriched-email (detect-language-and-categorize-event event) nil event))

(defmethod handle-enrichment :language-detection-request [event]
  (events/create-event :enriched-email (detect-language-event event) nil event))

(defn enrichment-event-loop
  "Enriches the e-mails. Listens to two events:

  :parsed-enrichable-email    - Detects both the language and the category
  :language-detection-request - Only detects the language"
  [publisher events-channel]
  (let [parsed-enrichable-email-chan (async/chan)
        language-detection-request-chan (async/chan)
        local-chan (async/merge [parsed-enrichable-email-chan language-detection-request-chan])]
    (async/sub publisher :parsed-enrichable-email parsed-enrichable-email-chan)
    (async/sub publisher :language-detection-request language-detection-request-chan)
    (async/pipeline 4
                    events-channel
                    (map handle-enrichment)
                    local-chan
                    true
                    (fn [^Throwable th] (t/log! {:level :error :error th} (.getMessage th))))))
