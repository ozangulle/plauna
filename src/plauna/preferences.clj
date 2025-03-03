(ns plauna.preferences
  (:require
   [clojure.core.cache.wrapped :as w]
   [plauna.database :as db]))

(def cache (w/ttl-cache-factory {} :ttl 6000))

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

(defn update-preference [key value]
  (db/update-preference key value)
  (w/evict cache key))

(defn log-level [] (w/lookup-or-miss cache
                                     :log-level
                                     (fn [key] (preference-with-default key or :info))))

(defn language-detection-threshold [] (w/lookup-or-miss cache
                                                        :language-detection-threshold
                                                        (fn [key] (preference-with-default key or 0.80))))

(defn categorization-threshold [] (w/lookup-or-miss cache
                                                    :categorization-threshold
                                                    (fn [key] (preference-with-default key or 0.65))))
