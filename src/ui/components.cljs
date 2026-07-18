(ns ui.components
  (:require ["@mui/material" :as material]
            [clojure.core.async :refer [put! <! chan timeout go-loop]]
            [reagent.core :as r]))

(def snackbar-chan (chan 10))

(def snackbar-state (r/atom {:open false
                             :message ""
                             :severity "info"
                             :duration 3000}))

(def default-open-duration 5000)


(defn start-snackbar-loop []
  (go-loop []
    (let [new-state (<! snackbar-chan)]
      (when new-state
        (reset! snackbar-state (assoc new-state :open true))

        (let [duration (:duration new-state default-open-duration)]
          (<! (timeout duration))
          (swap! snackbar-state assoc :open false))

        (recur)))))

(defn show-snackbar
  ([message] (show-snackbar message "info"))
  ([message severity] (show-snackbar message severity 3000))
  ([message severity duration]
   (put! snackbar-chan {:message message
                        :severity severity
                        :duration duration})))

(defn snackbar-component []
  (let [{:keys [open message severity]} @snackbar-state]
    [:> material/Snackbar
     {:open open
      :autoHideDuration (:duration @snackbar-state)
      :onClose (fn [] (swap! snackbar-state assoc :open false))
      :anchorOrigin {:vertical :top :horizontal :center}}
     [:> material/Alert
      {:onClose (fn [] (swap! snackbar-state assoc :open false))
       :severity severity
       :sx {:width "100%"}}
      message]]))
