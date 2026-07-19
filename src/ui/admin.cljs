(ns ui.admin
  (:require ["@mui/material" :as material]
            ["@mui/icons-material/Add" :default AddIcon]
            ["@mui/icons-material/DeleteForever" :default DeleteForeverIcon]
            ["@mui/icons-material/WarningRounded" :default WarningRoundedIcon]
            [reagent.core :as r]
            [ui.inputs :as inputs]
            [ui.utils :as utils]
            [ui.backend :as backend]))

(defonce preferences-data (r/atom {}))

(defonce categories-data (r/atom {}))

(defonce languages-data (r/atom {}))

(defn save-preferences [preferences] (backend/save-preferences preferences))

(defn update-and-save-log-level [event]
  (swap! preferences-data (fn [old] (update old :log-level (fn [_] (keyword (utils/event-val event)))
  )))
  (save-preferences @preferences-data))

(defn preferences []
  (fn []
    (r/with-let [loading? (r/atom true)]
      (when @loading? (backend/fetch-preferences (fn [response]
                                                   (reset! preferences-data (:body response))
                                                   (reset! loading? false))))
      (if @loading?
        [:> material/LinearProgress {:aria-label "Loading"}]
        [:<>
         [:h4 "Preferences"]
         [:> material/List
          [:> material/ListItem [:f> inputs/debounced-input
                                 (get @preferences-data :language-detection-threshold)
                                 "Language Detection Threshold"
                                 (fn [_]
                                   (swap! preferences-data update :language-detection-threshold js/Number)
                                   (save-preferences @preferences-data))
                                 (fn [new-value] (swap! preferences-data (fn [old] (update old :language-detection-threshold (fn [_] new-value))))
                                   (get @preferences-data :language-detection-threshold))]]
          [:> material/ListItem [:f> inputs/debounced-input
                                 (get @preferences-data :categorization-threshold)
                                 "Categorization Threshold"
                                 (fn [_]
                                   (swap! preferences-data update :categorization-threshold js/Number)
                                   (save-preferences @preferences-data))
                                 (fn [new-value] (swap! preferences-data (fn [old] (update old :categorization-threshold (fn [_] new-value))))
                                   (get @preferences-data :categorization-threshold))]]
          [:> material/ListItem
           [:> material/FormControl
            [:> material/InputLabel {:id "log-level"} "Log Level"]
            [:> material/Select {:label-id "log-level"
                                 :id "log-level"
                                 :value (get @preferences-data :log-level)
                                 :label "Log Level"
                                 :on-change update-and-save-log-level}
             [:> material/MenuItem {:value :error} "Error"]
             [:> material/MenuItem {:value :info} "Info (default)"]
             [:> material/MenuItem {:value :debug} "Debug"]]]]
          [:> material/ListItem [:f> inputs/debounced-input
                                 (get @preferences-data :client-health-check-interval)
                                 "IMAP Health Check Interval (sec)"
                                 (fn [_]
                                   (swap! preferences-data update :client-health-check-interval js/Number
                                          )
                                   (save-preferences @preferences-data))
                                 (fn [new-value] (swap! preferences-data (fn [old] (update old :client-health-check-interval (fn [_] new-value))))
                                   (get @preferences-data :client-health-check-interval))]]]]))))

(defn- delete-category-button [name id]
  (r/with-let [open (r/atom false)]
    [:<>
     [:> material/Button {:variant "outlined"
                          :color "error"
                          :onClick #(reset! open true)}
      [:> DeleteForeverIcon]]
     [:> material/Dialog {:open @open
                          :onClose #(reset! open false)}
      [:> material/DialogTitle {:sx {:display "flex" :alignItems "center" :gap 1}}
       [:> WarningRoundedIcon]
       (str "Delete Category: " name)]
      [:> material/Divider]
      [:> material/DialogContent "Are you sure you want to delete this category?"]
      [:> material/DialogActions
       [:> material/Button {:variant "contained"
                            :color "error"
                            :onClick (fn [] (backend/delete-category id (fn []
                                                                          (backend/fetch-categories (fn [response]                                                                                                      (reset! categories-data (:body response))
                                                                                                      (reset! open false))))))}
        "Delete"]
       [:> material/Button {:variant "outlined"
                            :onClick #(reset! open false)}
        "Cancel"]]]]))

(defn categories []
  (fn []
    (r/with-let [loading? (r/atom true)
                 new-category (r/atom nil)]
      (when @loading? (backend/fetch-categories (fn [response]
                                                  (reset! categories-data (:body response))
                                                  (reset! loading? false))))
      (if @loading?
        [:> material/LinearProgress {:aria-label "Loading"}]
        [:<>
         [:h4 "Categories"]
         [:> material/List
          (for [category @categories-data]
            [:> material/ListItem [:> material/ListItemText (:name category)] [:> material/ListItemIcon [delete-category-button (:name category) (:id category)]]])
          [:> material/ListItem
           [:> material/ListItemText
            [:> material/TextField {:value @new-category
                                    :on-change (fn [event] (reset! new-category (utils/event-val event)))}]]
           [:> material/ListItemIcon
            [:> material/Fab {:color "primary"
                              :aria-label "add"
                              :on-click (fn [] (backend/add-category
                                                @new-category
                                                (fn [] (backend/fetch-categories (fn [response] (reset! categories-data (:body response)))))))} [:> AddIcon]]]]]]))))

(defn toggle-language [language] (update language :use_in_training (fn [cur] (- 1 cur))))

(defn update-languages [language index] (assoc @languages-data index language))

(defn languages []
  (fn []
    (r/with-let [loading? (r/atom true)]
      (when @loading? (backend/fetch-languages (fn [response]
                                                 (reset! languages-data (:body response))
                                                 (reset! loading? false))))
      (if @loading?
        [:> material/LinearProgress {:aria-label "Loading"}]
        [:<>
         [:h4 "Languages"]
         [:> material/List
          (for [index (range (count @languages-data))
                :let [language (get @languages-data index)]]
            [:> material/ListItem (:language language)
             [:> material/Checkbox {:checked (= 1 (:use_in_training language))
                                    :on-click (fn [_]
                                                (backend/update-languages
                                                 (update-languages (toggle-language language) index)
                                                 (fn [response]
                                                   (reset! languages-data (:body response)))))}]])]]))))

(defn admin-page []
  (fn []
    [:<>
     [:h3 "Administration"]
     [:> material/Grid {:container true :spacing 2 :columns {:sm 4 :md 4 :lg 12}}
      [:> material/Grid {:size 4}
       [:f> categories]]
      [:> material/Grid {:size 4}
       [:f> preferences]]
      [:> material/Grid {:size 4}
       [:f> languages]]]]))

