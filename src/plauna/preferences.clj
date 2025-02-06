(ns plauna.preferences
  (:require [plauna.database :as db]))

(def fetch-fn (atom db/fetch-preference))

(def converters {clojure.lang.Keyword (fn [^String s] (keyword (.substring s 1)))
                 java.lang.Double Double/parseDouble})

(defmacro preference-with-default [property pred default]
  `(let [value# (~pred (@fetch-fn ~property) ~default)
         default-type# (class ~default)
         type# (class value#)]
     (if (= default-type# type#)
       value#
       ((get converters default-type#) value#))))

(defn log-level [] (preference-with-default :log-level or :info))

(defn language-detection-threshold [] (preference-with-default :language-detection-threshold or 0.80))

(defn categorization-threshold [] (preference-with-default :categorization-threshold or 0.65))
