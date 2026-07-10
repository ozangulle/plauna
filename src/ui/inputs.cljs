(ns ui.inputs
  (:require [plauna.core.email :as ce]
            [reagent.core :as r]
            [ui.utils :as utils]
            ["@mui/material" :as material]))

(defonce debounce-timeout 1000)

(defn make-debouncer
  "Returns a function (call! arg) that will call (f arg) after delay-ms; returns a cancel fn on metadata."
  [f delay-ms]
  (let [timeout-id (atom nil)]
    (with-meta
      (fn [& args]
        ;; clear previous timer
        (when-let [id @timeout-id] (js/clearTimeout id))
        (reset! timeout-id (js/setTimeout (fn [] (apply f args)) delay-ms)))
      {:cancel (fn [] (when-let [id @timeout-id] (js/clearTimeout id) (reset! timeout-id nil)))})))

(defn debounced-input []
  (let [debouncer-ref (atom nil)]
    (r/create-class
     {:component-will-unmount
      (fn []
        (when-let [db @debouncer-ref]
          ((:cancel (meta db)))))

      :reagent-render
      (fn [value label debouncer-action on-change-handler]
        (when (nil? @debouncer-ref)
          (reset! debouncer-ref
                  (make-debouncer
                   (fn [value] (when (some? value) (debouncer-action value)))
                   debounce-timeout)))

        (let [func (fn [event] ((comp @debouncer-ref on-change-handler) (utils/event-val event)))]
          [:> material/TextField
           {:value value
            :label label
            :type "text"
            :on-click #(.stopPropagation %)
            :on-change func
            :sx {"z-index" 2}}]))})))

(defn- handle-na-category-id
  "Backend uses nil as a missing category but frontend needs a number so it give n/a id -1. We need to revert this when we send the data back to the server"
  [email]
  (if (= -1 (-> email :metadata :category))
    (update-in email [:metadata :category] (fn [_] nil))
    email))

(defn category-select [email label categories debouncer-action on-change-handler]
  (let [debouncer (make-debouncer (fn [email] (when (some? email) (debouncer-action email))) debounce-timeout)
        func (fn [event] ((comp debouncer handle-na-category-id on-change-handler) event))
        selected (->> categories (filter #(= (ce/category email) (:name %))) first)]
    [:> material/FormControl
     [:> material/InputLabel {:id "category-select-label"} label]
     (into [:> material/Select {:value (get selected :id -1) :label-id "category-select-label" :on-click #(.stopPropagation %) :on-change func}] (mapv (fn [category] [:> material/MenuItem {:value (:id category)} (:name category)]) categories))]))
