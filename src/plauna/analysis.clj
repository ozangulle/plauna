(ns plauna.analysis
  (:require [clojure.string :as cs]
            [plauna.database :as db]
            [clojure.core.async :as async]
            [taoensso.telemere :as t]
            [cld.core :as lang]
            [plauna.core.email :as core-email]
            [plauna.files :as files])
  (:import
   (opennlp.tools.util MarkableFileInputStreamFactory PlainTextByLineStream TrainingParameters)
   (opennlp.tools.doccat DocumentSampleStream DocumentCategorizerME DoccatFactory DoccatModel)
   (opennlp.tools.ml.naivebayes NaiveBayesTrainer)
   (opennlp.tools.ml.maxent GISTrainer)
   (java.util Locale)
   (java.io File OutputStream)))

(set! *warn-on-reflection* true)

(defn language-detection-threshold [] (or (db/fetch-preference (name :language-detection-threshold)) 0.65))

(defn categorization-threshold [] (or (db/fetch-preference (name :categorization-threshold)) 0.65))

(defn ^String categorization-algorithm [] (or (:categorization-algorithm (db/fetch-preference :categorization-algorithm)) NaiveBayesTrainer/NAIVE_BAYES_VALUE))

(lang/default-init!)

(defn lang-code-set3 [language]
  (.getISO3Language (new Locale language)))

(defn detect-language [^String text]
  (if (> (count text) 3)
    (let [result (second (lang/detect text))
          confidence (Double/parseDouble (first (vals result)))
          lang-code (lang-code-set3 (first (keys result)))]
      {:code (if (< confidence (language-detection-threshold)) "n/a" lang-code)
       :confidence confidence})
    {:code "n/a" :confidence 0.0}))

(defn training-data-stream [file]
  (-> (MarkableFileInputStreamFactory. file)
      (PlainTextByLineStream. "UTF-8")
      (DocumentSampleStream.)))

(defn format-training-data [data]
  (transduce
   (comp (map #(core-email/training-content "text/html" %))
         (map #(str (:category %) " " (if (some? (:subject %)) (cs/trim (:subject %)) "") " " (:training-content %) "\n")))
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
         ""
        ,)

(defn serialize-model! [^DoccatModel model ^OutputStream os]
  (when (some? model) (.serialize model os)))

(defn train-data [training-files]
  (for [tf training-files]
    (try
      {:model
       (DocumentCategorizerME/train (:language tf) (training-data-stream (:file tf)) (training-parameters) (DoccatFactory.))
       :language (:language tf)}
      (catch Exception e (t/log! :error (.getMessage e))))))

(defn categorize [text ^File model-file]
  (let [doccat (DocumentCategorizerME. (DoccatModel. model-file))
        probabilities (.categorize doccat (into-array String (cs/split text #" ")))]
    {:name (.getBestCategory doccat probabilities) :confidence (get probabilities 0)}))


;; TODO handle errors
(defn detect-language-and-categorize-event [event]
  (let [email (:payload event)
        training-content (:training-content (core-email/training-content "text/html" email))
        allowed-languages (map :language (filter #(= 1 (:use_in_training %)) (db/get-language-preferences)))
        language-result (detect-language training-content)
        category-result (when (some #(= (:code language-result) %) allowed-languages) (categorize training-content (files/model-file (:code language-result))))
        category-id (:id (db/category-by-name (:name category-result)))]
    (core-email/construct-enriched-email email {:language (:code language-result) :language-confidence (:confidence language-result)} {:category (:name category-result) :category-confidence (:confidence category-result) :category-id category-id})))

(defn detect-language-event [event]
  (let [email (:payload event)
        training-content (:training-content (core-email/training-content "text/html" email))
        language-result (try (detect-language training-content) (catch Exception e (t/log! :error [(.getMessage e) "\nText causing the exception:" training-content])))]
    (core-email/construct-enriched-email email {:language (:code language-result) :language-confidence (:confidence language-result)} {:category (-> email :metadata :category) :category-confidence (-> email :metadata :category-confidence) :category-id (-> email :metadata :category-id)}))
  )

(defmulti handle-enrichment :type)

(defmethod handle-enrichment :parsed-enrichable-email [event]
  {:type :enriched-email :options (:options event) :payload (detect-language-and-categorize-event event)})

(defmethod handle-enrichment :language-detection-request [event]
  {:type :enriched-email :options (:options event) :payload (detect-language-event event)})

(defn enrichment-loop [publisher events-channel]
  (let [parsed-enrichable-email-chan (async/chan)
        language-detection-request-chan (async/chan)
        local-chan (async/merge [parsed-enrichable-email-chan language-detection-request-chan])]
    (async/sub publisher :parsed-enrichable-email parsed-enrichable-email-chan)
    (async/sub publisher :language-detection-request language-detection-request-chan)
    (async/pipeline 4
                    events-channel
                    (map handle-enrichment)
                    local-chan)))
