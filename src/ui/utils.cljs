(ns ui.utils)

(defn parse-date [date]
  (let [options {:year :numeric :month :numeric :day :numeric}]
    (.toLocaleDateString (new js/Date (* 1000 date)) js/undefined (clj->js options))))

(defn filter-contacts-for-key [key email] (filterv (fn [participant] (= (:type participant) key)) (:participants email)))

(defn filter-from [email] (filter-contacts-for-key "sender" email))

(defn filter-to [email] (filter-contacts-for-key "receiver" email))

(defn filter-cc [email] (filter-contacts-for-key "cc" email))

(defn filter-bcc [email] (filter-contacts-for-key "bcc" email))

(defn concat-contacts [contacts]
  (reduce (fn [acc el] (if (empty? acc) (str acc "<" (:name el) "> " (:address el)) (str acc ", <" (:name el) "> " (:address el)))) "" contacts))

(defn decimal-place [number dec]
  (if (nil? number)
    nil
    (let [precision (js/Math.pow 10 dec)]
      (/ (js/Math.trunc (* (js/parseFloat number) precision)) precision))))

(defn event-val [event] (-> event .-target .-value))
