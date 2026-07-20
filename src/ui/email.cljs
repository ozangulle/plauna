(ns ui.email
  (:require
   ["@mui/icons-material/ArrowBack" :default ArrowBackIcon]
   ["@mui/icons-material/WarningRounded" :default WarningRoundedIcon]
   ["@mui/lab" :as lab]
   ["@mui/material" :as material]
   [cljs.core.async :refer [take!]]
   [plauna.core.email :as ce]
   [react :as react]
   [react-router-dom :as rr]
   [reagent.core :as r]
   [ui.backend :as backend]
   [ui.inputs :as inputs]
   [ui.utils :as utils]))

(def email-data (r/atom {}))

(def move-email (r/atom true))

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

(defn save-metadata [email move?] (backend/save-metadata-for-email email move?))

(defn category-update-handler [email]
  (fn [event] (update-key [[[:metadata :category-id] (event-val event)]
                           [[:metadata :category] (->> (:categories (:optional @email-data)) (filter #(= (event-val event) (:id %))) first :name)]
                           [[:metadata :category-confidence] 1]])))

(defn category-debouncer [] (fn [email] (take! (save-metadata email @move-email) (fn [_] (fetch-email (ce/message-id email))))))

(defn body-part->html [body-part]
  (let [value (r/atom "0")
        tab-change (fn [_ new-val] (reset! value new-val))]
    (fn []
      [:> material/Paper {:sx {:margin-bottom 3}}
       [:> material/Grid {:container true :spacing 3}
        [:> material/Grid {:xs 12 :sm 4}
         [:> material/List
          [:> material/ListItem [:> material/ListItemText
                                 {:secondary "Mime Type" :primary (:mime-type body-part)}]]
          [:> material/ListItem [:> material/ListItemText
                                 {:secondary "Charset" :primary (:charset body-part)}]]
          [:> material/ListItem [:> material/ListItemText
                                 {:secondary "Transfer Encoding" :primary (:transfer-encoding body-part)}]]
          (when (some? (:filename body-part))
            [:> material/ListItem [:> material/ListItemText
                                   {:secondary "File Name" :primary (:transfer-encoding body-part)}]])
          (when (some? (:content-disposition body-part))
            [:> material/ListItem [:> material/ListItemText
                                   {:secondary "Content Disposition" :primary (:content-disposition body-part)}]])]]
        [:> material/Grid {:xs 12 :sm 8}
         (if (or (= "text/plain" (:mime-type body-part)) (= "text/html" (:mime-type body-part)))
           [:> lab/TabContext {:value @value}
            [:> material/Box {:sx {:borderBottom 1 :borderColor "divider"}}
             [:> lab/TabList {:on-change tab-change}
              [:> material/Tab {:label "Unsanitized" :value "0"}]
              [:> material/Tab {:label "Sanitized" :value "1"}]]]
            [:> material/Container {:fixed true} [:> lab/TabPanel {:value "0"} [:div {:style {:height "50em" :overflow "scroll"}} (:content body-part)]]]
            [:> material/Container {:fixed true} [:> lab/TabPanel {:value "1"} (:sanitized-content body-part)]]]
           [:div "Non-text content"])]]])))

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
       [:> material/Button {:variant :contained
                            :color "error"
                            :onClick (fn [] (backend/delete-email id nil)
                                       (navigate "/emails")
                                       (reset! open false))}
        "Delete"]
       [:> material/Button {:variant :contained
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
        [:> material/LinearProgress {:aria-label "Loading…"}]
        (let [email (:data @email-data)]
          [:div
           [:> material/IconButton {:on-click #(navigate -1)} [:> ArrowBackIcon]]
           [:h2 (ce/subject email)]
           [:> material/Grid {:container true
                              :direction :row
                              :spacing 3
                              :sx {:justifyContent :space-between
                                   :alignItems :flex-start}
                              :columns {:sm 6 :md 6 :lg 12}}
            [:> material/Grid {:size 6}
             [:> material/Paper
              [:> material/List
               [:> material/ListItem [:> material/ListItemText
                                      {:secondary "Message ID" :primary (ce/message-id email) :sx {:overflow :hidden}}]]
               [:> material/ListItem [:> material/ListItemText
                                      {:secondary "Date" :primary (utils/parse-date (ce/date email))}]]
               [:> material/ListItem [:> material/ListItemText
                                      {:secondary "Senders" :primary (-> email utils/filter-from utils/concat-contacts)}]]
               [:> material/ListItem [:> material/ListItemText
                                      {:secondary "Recipients" :primary (-> email utils/filter-to utils/concat-contacts)}]]
               [:> material/ListItem [:> material/ListItemText
                                      {:secondary "CC" :primary (-> email utils/filter-cc utils/concat-contacts)}]]
               [:> material/ListItem [:> material/ListItemText
                                      {:secondary "BCC" :primary (-> email utils/filter-bcc utils/concat-contacts)}]]
               [:> material/ListItem [:f> delete-button navigate id]]]]]
            [:> material/Grid {:size 6}
             (comment [:h3 "Metadata"])
             [:> material/Paper
              [:> material/List
               [:> material/ListItem
                [:f> inputs/debounced-input
                 (get-in email [:metadata :language])
                 "Language"
                 (fn [mail] (save-metadata (:data mail) false))
                 (fn [new-value]
                   (swap! email-data assoc-in [:data :metadata :language] new-value)
                   (swap! email-data assoc-in [:data :metadata :language-confidence] 1))]]
               [:> material/ListItem
                [:> material/ListItemText
                 {:secondary "Language Confidence" :primary
                  (utils/decimal-place (ce/language-confidence email) 4)}]]
               [:> material/ListItem
                (inputs/category-select email
                                        "Category"
                                        (:categories (:optional @email-data))
                                        (category-debouncer)
                                        (category-update-handler email))]
               [:> material/ListItem
                [:> material/ListItemText
                 {:secondary "Category Confidence" :primary
                  (utils/decimal-place (ce/category-confidence email) 4)}]]
               [:> material/ListItem
                [:> material/FormControlLabel {:label "Move this email after update"
                                               :control (r/create-element material/Checkbox
                                                                          #js
                                                                           {:checked @move-email
                                                                            :onChange (fn [_ new] (reset! move-email new))
                                                                            :label "Test"})}]]]]]]
           [:h3 "Content(s)"]
           (into [:<>] (contents (:body email)))])))))
