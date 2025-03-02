(ns plauna.util.text-transform
  (:require
   [clojure.java.io :refer [input-stream]])
  (:import
   (org.jsoup Jsoup)
   (javax.swing.text.rtf RTFEditorKit)))

(defn html->text [^String html] (.text (Jsoup/parse html "UTF-8")))

(defn rtf->string [^String rtf]
  (let [rtf-parser (new RTFEditorKit)
        document (.createDefaultDocument rtf-parser)]
    (.read rtf-parser (input-stream (.getBytes rtf)) document 0)
    (.getText document 0 (.getLength document))))

(defn clean-text-content [content content-type]
  (cond (= :html content-type) (html->text content)
        (= :rtf content-type) (rtf->string content)
        :else (html->text content)))
