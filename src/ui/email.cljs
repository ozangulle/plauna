(ns ui.email
  (:require [ui.backend :as backend]
            [ui.utils :as utils]
            [ui.inputs :as inputs]
            [plauna.core.email :as ce]
            [reagent.core :as r]
            [react :as react]
            [cljs.core.async :refer [take!]]
            [react-router-dom :as rr]
            ["@mui/material" :as material]
            ["@mui/lab" :as lab]
            ["@mui/icons-material/WarningRounded" :default WarningRoundedIcon]
            ["@mui/icons-material/ArrowBack" :default ArrowBackIcon]))

(def email-data (r/atom {}))

(defn event-val [event] (-> event .-target .-value))

(defn fetch-email
  ([id loading?] (backend/fetch-email-data id (fn [response] (reset! email-data (:body response)) (reset! loading? false))))
  ([id] (backend/fetch-email-data (js/btoa id) (fn [response] (reset! email-data (:body response))))))

(defn update-key [keys-val-pairs]
  (swap! email-data
         (fn [old-data]
           (let [result (reduce (fn [result pair] (assoc-in result (first pair) (second pair))) (:data old-data) keys-val-pairs)]
             (assoc old-data :data result))))
  (:data @email-data))

(defn language-update-handler [keys] (fn [event] (update-key [[keys (event-val event)] [[:metadata :language-confidence] 1]])))

(defn save-metadata "Returns a channel" [email] (backend/save-metadata-for-email email false))

(defn category-update-handler [email]
  (fn [event] (update-key [[[:metadata :category-id] (event-val event)]
                            [[:metadata :category] (->> (:categories (:optional @email-data)) (filter #(= (event-val event) (:id %))) first :name)]
                                                  [[:metadata :category-confidence] 1]])))

(defn category-debouncer [] (fn [email] (take! (save-metadata email) (fn [_] (fetch-email (ce/message-id email))))))

(defn body-part->html [body-part]
  (let [value (r/atom "0")
        tab-change (fn [_ new-val] (reset! value new-val))]
    (fn []
      [:> material/List
       [:> material/ListItem "Mime Type:" (:mime-type body-part)]
       [:> material/ListItem "Charset:" (:charset body-part)]
       [:> material/ListItem "Transfer Encoding:" (:transfer-encoding body-part)]
       (when (some? (:filename body-part))
         [:> material/ListItem "File Name:" (:transfer-encoding body-part)])
       (when (some? (:content-disposition body-part))
         [:> material/ListItem "Content Disposition:" (:content-disposition body-part)])
       (if (or (= "text/plain" (:mime-type body-part)) (= "text/html" (:mime-type body-part)))
         [:> lab/TabContext {:value @value}
          [:> material/Box {:sx {:borderBottom 1 :borderColor "divider"}}
           [:> lab/TabList {:on-change tab-change}
            [:> material/Tab {:label "Unsanitized" :value "0"}]
            [:> material/Tab {:label "Sanitized" :value "1"}]]]
          [:> material/Container {:fixed true} [:> lab/TabPanel {:value "0"} (:content body-part)]]
          [:> material/Container {:fixed true} [:> lab/TabPanel {:value "1"} (:sanitized-content body-part)]]]
         [:div "Non-text content"])])))

(defn- contents [body-parts]
  (mapv (fn [x] [:f> (body-part->html x)]) body-parts))

(defn- delete-button [navigate id]
  (r/with-let [open (r/atom false)]
    [:<>
     [:> material/Button {:variant "outlined"
                          :color "error"
                          :onClick #(reset! open true)}
      "Delete Email"]
     [:> material/Dialog {:open @open
                          :onClose #(reset! open false)}
      [:> material/DialogTitle {:sx {:display "flex" :alignItems "center" :gap 1}}
       [:> WarningRoundedIcon]
       "Delete Email"]
      [:> material/Divider]
      [:> material/DialogContent "Are you sure you want to delete this email?"]
      [:> material/DialogActions
       [:> material/Button {:variant "contained"
                            :color "error"
                            :onClick (fn [] (backend/delete-email id nil)
                                       (navigate "/emails")
                                       (reset! open false))}
        "Delete"]
       [:> material/Button {:variant "outlined"
                            :onClick #(reset! open false)}
        "Cancel"]]]]))

(defn email-page []
  (let [navigate (rr/useNavigate)
        params (rr/useParams)
        id (get (js->clj params) "id")]
    (r/with-let [loading? (r/atom true)]
      (when @loading?
        (fetch-email id loading?))
      (if @loading?
        [:p "LOADING"]
        (let [email (:data @email-data)]
          [:div
           [:> material/IconButton {:on-click #(navigate -1)} [:> ArrowBackIcon]]
           [:h2 (ce/subject email)]
           [:> material/Paper
            [:> material/List
             [:> material/ListItem "Message ID:" (ce/message-id email)]
             [:> material/ListItem "Date:" (utils/parse-date (ce/date email))]
             [:> material/ListItem "Senders:" (-> email utils/filter-from utils/concat-contacts)]
             [:> material/ListItem "Recipients:" (-> email utils/filter-to utils/concat-contacts)]
             [:> material/ListItem "CC:" (-> email utils/filter-cc utils/concat-contacts)]
             [:> material/ListItem "BCC:" (-> email utils/filter-bcc utils/concat-contacts)]]]
           [:f> delete-button navigate id]
           [:h3 "Metadata"]
           [:> material/Paper
            [:> material/List
             [:> material/ListItem "Language:" (inputs/editable-language email [:metadata :language]
                                                                         (fn [mail] (save-metadata mail))
                                                                         (language-update-handler [:metadata :language]))]
             [:> material/ListItem "Language Confidence:" (utils/decimal-place (ce/language-confidence email) 4)]
             [:> material/ListItem "Category:" (inputs/category-select email (:categories (:optional @email-data))
                                                                       (category-debouncer)
                                                                       (category-update-handler email))]
             [:> material/ListItem "Category Confidence:" (utils/decimal-place (ce/category-confidence email) 4)]]]
           [:h3 "Content(s)"]
           (into [:> material/Paper] (contents (:body email)))])))))
