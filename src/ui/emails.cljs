(ns ui.emails
  (:require
   ["@mui/material" :as material]
   [cljs.core.async :refer [take!]]
   [plauna.core.email :as ce]
   [react-router-dom :as rr]
   [reagent.core :as r]
   [ui.backend :as backend]
   [ui.components :as components]
   [ui.inputs :as inputs]
   [ui.utils :as utils]))

(defonce emails (r/atom {}))

(defn refresh-emails
  ([parameters loading-atom]
   (backend/fetch-emails parameters (fn [response] (reset! emails (:body response)) (reset! loading-atom false))))
  ([parameters] (backend/fetch-emails parameters (fn [response] (reset! emails (:body response))))))

(defn save-metadata  [email] (backend/save-metadata-for-email email false))

(defn email-by-id [id] (->> @emails
                            :data
                            (filter #(= id (ce/message-id %)))
                            first))

(defn update-key [id keys-val-pairs]
  (swap! emails
         (fn [emails-res]
           (let [emails (:data emails-res)
                 updated-emails (mapv (fn [email]
                                        (if (= (ce/message-id email) id)
                                          (let [result (reduce (fn [result pair] (assoc-in result (first pair) (second pair))) email  keys-val-pairs)] result)
                                          email))
                                      emails)]
             (assoc emails-res :data updated-emails))))
  (email-by-id id))

(defn event-val [event] (-> event .-target .-value))

(defn category-update-handler [email]
  (fn [event] (update-key (ce/message-id email) [[[:metadata :category-id] (event-val event)]
                                                 [[:metadata :category] (->> (:categories (:optional @emails)) (filter #(= (event-val event) (:id %))) first :name)]
                                                 [[:metadata :category-confidence] 1]])))

(defn category-debouncer [] (fn [email] (take! (save-metadata email) (refresh-emails (:parameters @emails)))))

(defn extract-email-fields [email index navigate]
  [:> material/TableRow
   {:key (ce/message-id email)
    :on-click (fn [_] (navigate (str "/emails/" (js/btoa (ce/message-id email)))))
    :sx {"&:nth-of-type(odd)" {:backgroundColor "action.hover"}
         :cursor "pointer"
         :z-index 1}}
   [:> material/TableCell  (utils/parse-date (ce/date email))]
   [:> material/TableCell
    {:sx {:maxWidth 300
          :overflow "hidden"
          :textOverflow "ellipsis"
          :whiteSpace "nowrap"}}
    [:> material/Typography {:noWrap true} (ce/subject email)]]
   [:> material/TableCell
    {:sx {:maxWidth 200
          :overflow "hidden"
          :textOverflow "ellipsis"
          :whiteSpace "nowrap"}}
    [:> material/Typography {:noWrap true} (-> email utils/filter-from utils/concat-contacts)]]
   [:> material/TableCell
    {:sx {:maxWidth 200
          :overflow "hidden"
          :textOverflow "ellipsis"
          :whiteSpace "nowrap"}}
    [:> material/Typography {:noWrap true} (-> email utils/filter-to utils/concat-contacts)]]
   [:> material/TableCell [:f> inputs/debounced-input
                           (get-in email [:metadata :language])
                           ""
                           (fn [mail] (save-metadata mail))
                           (fn [new-value] (get (:data (swap! emails assoc-in [:data index :metadata :language] new-value)) index))]]
   [:> material/TableCell (utils/decimal-place (ce/language-confidence email) 4)]
   [:> material/TableCell (inputs/category-select email
                                                  ""
                                                  (:categories (:optional @emails))
                                                  (category-debouncer)
                                                  (category-update-handler email))]
   [:> material/TableCell (utils/decimal-place (ce/category-confidence email) 4)]])

(defn handle-change-size [event]
  (swap! emails (fn [old] (update-in old [:parameters :size] (fn [_] (event-val event)))))
  (refresh-emails (:parameters @emails)))

(defn handle-page-change [event new-page]
  (swap! emails (fn [old] (update-in old [:parameters :page] (fn [_] (inc new-page)))))
  (refresh-emails (:parameters @emails)))

(defn handle-search-input [event]
  (let [input-value (event-val event)]
    (cond
      (> (count input-value) 3)
      (do (swap! emails (fn [old] (update-in old [:parameters :search-text] (fn [_] input-value))))
          (refresh-emails (:parameters @emails)))
      (= (count input-value) 0)
      (do (swap! emails (fn [old] (update-in old [:parameters :search-text] (fn [_] nil))))
          (refresh-emails (:parameters @emails))))))

(defn emails-page []
  (let [navigate (rr/useNavigate)]
    (r/with-let [loading? (r/atom true)]
      (when @loading?
        (refresh-emails (:parameters @emails) loading?))
      (if @loading?
        [:> material/LinearProgress {:aria-label "Loading…"}]
        (if (empty? @emails)
          [:div
           [:h2 "Emails"]
           [:p "Nothing to see here"]]
          [:div
           [:h2 "Emails"]
           [:> material/Box {:sx {:display :flex :justifyContent "left"}}
            [:> material/Button {:variant :contained
                                 :on-click (fn [_] (backend/train-data
                                                    (fn [res] (if (and (= 200 (:status res)) (not= :alert (:type (:body res))))
                                                                (components/show-snackbar "Training was successful" :success)
                                                                (components/show-snackbar (str "There was an error during training " (:content (:body res))) :error nil)))))} "Train using existing data"]]
           [:> material/Box
            {:sx {:display "flex" :justifyContent "center"}}
            [:> material/Tooltip
             {:title "Search in subject, text contents and contacts"
              :placement "top"}
             [:> material/TextField
              {:label "Search"
               :on-change handle-search-input}]]
            [:> material/TablePagination
             {:component "div"
              :count (get-in @emails [:parameters :total-pages])
              :page (dec (get-in @emails [:parameters :page])) ; mui is index 0
              :rowsPerPage (get-in @emails [:parameters :size])
              :onRowsPerPageChange handle-change-size
              :onPageChange handle-page-change}]]
           [:> material/TableContainer {:component material/Paper}
            [:> material/Table
             [:> material/TableHead
              [:> material/TableRow
               [:> material/TableCell "Date"]
               [:> material/TableCell "Subject"]
               [:> material/TableCell "From"]
               [:> material/TableCell "To"]
               [:> material/TableCell "Language"]
               [:> material/TableCell "Confidence"]
               [:> material/TableCell "Category"]
               [:> material/TableCell "Confidence"]]]
             [:> material/TableBody
              (doall
               (for [index (range (count (:data @emails)))
                     :let [email (get (:data @emails) index)]]
                 (extract-email-fields email index navigate)))]]]]))
      (finally (reset! loading? true)))))
