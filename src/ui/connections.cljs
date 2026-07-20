(ns ui.connections
  (:require
   ["@mui/icons-material/Add" :default AddIcon]
   ["@mui/icons-material/DeleteForever" :default DeleteForeverIcon]
   ["@mui/icons-material/WarningRounded" :default WarningRoundedIcon]
   ["@mui/material" :as material]
   [react-router-dom :as rr]
   [reagent.core :as r]
   [ui.backend :as backend]
   [ui.components :as components]
   [ui.inputs :as inputs]
   [ui.utils :as utils]))

(defonce connections-data (r/atom []))

(defonce connection-data (r/atom {}))

(defonce new-provider (r/atom {}))

(defonce parse-settings (r/atom {:folder "" :move false :category ""}))

(defn fetch-connections-and-refresh [] (backend/fetch-connections (fn [res] (reset! connections-data (:body res)))))

(defn- delete-button [id]
  (r/with-let [open (r/atom false)]
    [:<>
     [:> material/Button {:variant "outlined"
                          :color "error"
                          :onClick (fn [event]
                                     (.stopPropagation event)
                                     (reset! open true))}
      "Delete"]
     [:> material/Dialog {:open @open
                          :onClose #(reset! open false)}
      [:> material/DialogTitle {:sx {:display "flex" :alignItems "center" :gap 1}}
       [:> WarningRoundedIcon]
       "Delete Connection"]
      [:> material/Divider]
      [:> material/DialogContent "Are you sure you want to delete this connection?"]
      [:> material/DialogActions
       [:> material/Button {:variant :contained
                            :color "error"
                            :onClick (fn [event] (backend/delete-connection id (fn [_] (fetch-connections-and-refresh)))
                                       (.stopPropagation event)
                                       (reset! open false))}
        "Delete"]
       [:> material/Button {:variant :contained
                            :onClick (fn [event]
                                       (.stopPropagation event)
                                       (reset! open false))}
        "Cancel"]]]]))

(defn reconnect-button [id connected]
  (if connected
    [:> material/Button {:color "secondary" :on-click (fn [event] (.stopPropagation event) (backend/post-connection-control id :reconnect nil (fn [_] (fetch-connections-and-refresh))))} "Reconnect"]
    [:> material/Button {:variant :contained :color "secondary" :on-click (fn [event] (.stopPropagation event) (backend/post-connection-control id :connect nil (fn [_] (fetch-connections-and-refresh))))} "Connect"]))

(defn disconnect-button [id connected]
  (if connected
    [:> material/Button {:variant :outlined :color "error" :on-click (fn [event] (.stopPropagation event) (backend/post-connection-control id :disconnect nil (fn [_] (fetch-connections-and-refresh))))} "Disconnect"]
    [:> material/Button {:variant :outlined :color "error" :disabled true :on-click (fn [event] (.stopPropagation event))} "Disconnect"]))

(defn connections-page []
  (fn []
    (let [navigate (rr/useNavigate)]
      (r/with-let [loading? (r/atom true)]
        (when @loading? (backend/fetch-connections (fn [response]
                                                     (reset! connections-data (:body response))
                                                     (reset! loading? false))))
        (if @loading?
          [:> material/LinearProgress {:aria-label "Loading"}]
          [:<>
           [:h2 "Connections"]
           [:> material/TableContainer {:component material/Paper}
            [:> material/Table
             [:> material/TableHead
              [:> material/TableRow
               [:> material/TableCell "Account"]
               [:> material/TableCell "Host"]
               [:> material/TableCell "Connected"]
               [:> material/TableCell "Folder Open"]
               [:> material/TableCell ""]]]
             [:> material/TableBody
              (for [connection @connections-data]
                [:> material/TableRow {:key (:id connection) :on-click (fn [_] (navigate (str "/connections/" (:id connection)))) :sx {:cursor "pointer"}}
                 [:> material/TableCell (:user connection)]
                 [:> material/TableCell (:host connection)]
                 [:> material/TableCell (str (:connected connection))]
                 [:> material/TableCell (str (:folder-open connection))]
                 [:> material/TableCell
                  [reconnect-button (:id connection) (:connected connection)]
                  [disconnect-button (:id connection) (:connected connection)]
                  [delete-button (:id connection)]]])]]]
           [:> material/Button {:variant :contained :on-click (fn [event] (.stopPropagation event) (navigate "/connections/new"))} "Add new"]])))))

(defn- delete-auth-provider-button [name id conn-id]
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
       (str "Delete Authentication Provider: " name)]
      [:> material/Divider]
      [:> material/DialogContent "Are you sure you want to delete this authentication provider?"]
      [:> material/DialogActions
       [:> material/Button {:variant :contained
                            :color "error"
                            :onClick (fn [] (backend/delete-auth-provider id (fn []
                                                                               (backend/fetch-connection conn-id (fn [response]
                                                                                                                   (reset! connection-data (:body response))
                                                                                                                   (reset! open false))))))}
        "Delete"]
       [:> material/Button {:variant :contained
                            :onClick #(reset! open false)}
        "Cancel"]]]]))

(defn update-connection-data-config [key] (fn [event new-value]
                                            (if (and (some? new-value) (boolean? new-value))
                                              (swap! connection-data (fn [old] (update-in old [:config key] (fn [_] new-value))))
                                              (swap! connection-data (fn [old] (update-in old [:config key] (fn [_] (utils/event-val event))))))))

(defn update-new-provider [key] (fn [event]
                                  (swap! new-provider (fn [old] (update old key (fn [_] (utils/event-val event)))))))

(defn update-provider [provider conn-id] (backend/update-auth-provider (:id provider) provider (fn [] (backend/fetch-connection conn-id (fn [response] (reset! connection-data (:body response)))))))

(defn connection-page [mode]
  (fn []
    (let [params (rr/useParams)
          navigate (rr/useNavigate)]
      (r/with-let [loading? (r/atom true)]
        (cond
          (and @loading? (= :edit mode))
          (backend/fetch-connection (get (js->clj params) "id")
                                    (fn [response]
                                      (reset! connection-data (:body response))
                                      (reset! loading? false)))
          (and @loading? (= :new mode)) (backend/fetch-auth-providers
                                         (fn [response]
                                           (swap! connection-data assoc-in [:config :auth-providers] (:body response))
                                           (reset! loading? false))))
        (if @loading?
          [:> material/LinearProgress {:aria-label "Loading"}]
          (let [config (:config @connection-data)]
            [:<>
             [:h2 "IMAP Connection for " (:host config) " - " (:user config)]
             [:> material/Grid {:container true :spacing 2}
              [:> material/Grid {:size 6}
               [:> material/Paper {:sx {:p 3}}
                [:> material/Stack {:spacing 2}
                 [:h3 "Configuration"]
                 [:> material/FormControl {:fullWidth true}
                  [:> material/InputLabel {:id "auth-type-select-label"} "Authentication Type"]
                  [:> material/Select {:labelId "auth-type-select-label"
                                       :value (:auth-type config)
                                       :label "Auth Type"
                                       :on-change (update-connection-data-config :auth-type)}
                   [:> material/MenuItem {:value "basic"} "Basic"]
                   [:> material/MenuItem {:value "oauth2"} "OAUTH2"]]]
                 [:> material/FormControl {:fullWidth true}
                  [:> material/TextField {:value (:host config) :labelId "host-label" :label "Host" :on-change (update-connection-data-config :host)}]]
                 [:> material/FormControl {:fullWidth true}
                  [:> material/TextField {:value (:user config) :label "User" :on-change (update-connection-data-config :user)}]]
                 [:> material/FormControl {:fullWidth true}
                  [:> material/TextField {:value (:secret config) :label "Secret" :type "password" :on-change (update-connection-data-config :secret)}]]
                 [:> material/FormControl {:fullWidth true}
                  [:> material/TextField {:value (:folder config) :label "Folder" :on-change (update-connection-data-config :folder)}]]
                 [:> material/FormControl {:fullWidth true}
                  [:> material/TextField {:value (:port config) :label "Port" :on-change (update-connection-data-config :port)}]]
                 [:> material/FormControl {:fullWidth true}
                  [:> material/InputLabel {:id "security-select-label"} "Authentication Type"]]
                 [:> material/FormControl {:fullWidth true}
                  [:> material/Select {:labelId "security-select-label"
                                       :value (:security config)
                                       :label "Security"
                                       :on-change (update-connection-data-config :security)}
                   [:> material/MenuItem {:value "ssl"} "SSL"]
                   [:> material/MenuItem {:value "starttls"} "STARTTLS"]
                   [:> material/MenuItem {:value "plain"} "PLAIN"]]]
                 [:> material/FormControl {:fullWidth true}
                  [:> material/FormControlLabel {:control (r/create-element material/Checkbox #js {:checked (:check-ssl-certs config) :onChange (update-connection-data-config :check-ssl-certs)}) :label "Check SSL Certificates"}]]
                 [:> material/FormControl {:fullWidth true}
                  [:> material/FormControlLabel {:control (r/create-element material/Checkbox #js {:checked (:debug config) :onChange (update-connection-data-config :debug)}) :label "IMAP Debug"}]]
                 (if (= :new mode)
                   [:> material/Button {:variant :contained
                                        :on-click (fn [] (backend/add-connection
                                                          (:config @connection-data)
                                                          (fn [_] (backend/fetch-connection (get (js->clj params) "id")
                                                                                            (fn [response]
                                                                                              (reset! connection-data (:body response))
                                                                                              (navigate "/connections"))))))} "Add New Connection"]
                   [:> material/Button {:variant :contained
                                        :on-click (fn [] (backend/update-connection
                                                          (get (js->clj params) "id")
                                                          (:config @connection-data)
                                                          (fn [_] (backend/fetch-connection (get (js->clj params) "id")
                                                                                            (fn [response]
                                                                                              (reset! connection-data (:body response)))))))} "Update Connection"])]]]
              [:> material/Grid {:size 6}
               (when (> (count (:folders @connection-data)) 0)
                 [:> material/Paper {:sx {:p 3}}
                  [:> material/Stack {:spacing 2}
                   [:h3 "Parse E-mails from Folders"]
                   [:> material/FormControl {:fullWidth true}
                    [:> material/InputLabel {:id "folders-select-label"} "Folders"]
                    [:> material/Select {:labelId "folders-select-label"
                                         :value (:folder @parse-settings)
                                         :label "Folders"
                                         :on-change (fn [e] (swap! parse-settings assoc-in [:folder] (utils/event-val e)))}
                     (for [folder (:folders @connection-data)]
                       ^{:key folder}
                       [:> material/MenuItem {:value folder} folder])]]
                   [:> material/FormControl {:fullWidth true}
                    [:> material/FormControlLabel {:control (r/create-element material/Checkbox #js {:checked (:move @parse-settings) :onChange (fn [_ new] (swap! parse-settings assoc-in [:move?] new))}) :label "Move e-mails after categorization"}]
                    [:> material/FormControl {:fullWidth true}
                     [:> material/InputLabel {:id "category-select-label"} "Assign following category for all e-mails in folder. Leave blank for automatic category detection."]
                     [:> material/Select {:labelId "category-select-label"
                                          :value (:category @parse-settings)
                                          :label "Assign following category for all e-mails in folder. Leave blank for automatic category detection."
                                          :on-change (fn [e] (swap! parse-settings assoc-in [:category] (utils/event-val e)))}
                      (for [category (:categories @connection-data)]
                        ^{:key (:id category)}
                        [:> material/MenuItem {:value (str (:id category) "-" (:name category))} (:name category)])]]]
                   [:> material/Button {:variant :contained
                                        :on-click (fn [_] (backend/post-connection-control
                                                           (get (js->clj params) "id")
                                                           :parse
                                                           @parse-settings
                                                           (fn [res] (components/show-snackbar (-> res :body :message) (-> res :body :type)))))} "Parse E-Mails"]]])]
              (when (= "oauth2" (:auth-type (:config @connection-data)))
                [:> material/Grid {:size 12}
                 [:h3 "Authentication Providers"]
                 [:<>
                  [:> material/TableContainer {:component material/Paper}
                   [:> material/Table
                    [:> material/TableHead
                     [:> material/TableRow
                      [:> material/TableCell "Name"]
                      [:> material/TableCell "Auth URL"]
                      [:> material/TableCell "Token URL"]
                      [:> material/TableCell "Redirect URL"]
                      [:> material/TableCell "Client ID"]
                      [:> material/TableCell "Client Secret"]
                      [:> material/TableCell "Scope"]
                      [:> material/TableCell ""]]]
                    [:> material/TableBody
                     [:> material/TableRow {:key -1}
                      [:> material/TableCell [:> material/TextField {:value (:name @new-provider) :on-change (update-new-provider :name)}]]
                      [:> material/TableCell [:> material/TextField {:value (:auth-url @new-provider) :on-change (update-new-provider :auth-url)}]]
                      [:> material/TableCell [:> material/TextField {:value (:token-url @new-provider) :on-change (update-new-provider :token-url)}]]
                      [:> material/TableCell [:> material/TextField {:value (:redirect-url @new-provider) :on-change (update-new-provider :redirect-url)}]]
                      [:> material/TableCell [:> material/TextField {:value (:client-id @new-provider) :on-change (update-new-provider :client-id)}]]
                      [:> material/TableCell [:> material/TextField {:value (:client-secret @new-provider) :on-change (update-new-provider :client-secret)}]]
                      [:> material/TableCell [:> material/TextField {:value (:scope @new-provider) :on-change (update-new-provider :scope)}]]
                      [:> material/TableCell
                       [:> material/Fab {:color "primary"
                                         :aria-label "add"
                                         :on-click (fn [] (backend/add-auth-provider @new-provider
                                                                                     (fn [_]
                                                                                       (reset! new-provider {})
                                                                                       (backend/fetch-connection (get (js->clj params) "id")
                                                                                                                 (fn [response]
                                                                                                                   (reset! connection-data (:body response)))))))} [:> AddIcon]]]]
                     (doall
                      (for [index (range (count (:auth-providers (:config @connection-data))))
                            :let [provider (get (:auth-providers (:config @connection-data)) index)]]
                        [:> material/TableRow {:key (:id provider)}
                         [:> material/TableCell [:f> inputs/debounced-input
                                                 (:name provider)
                                                 ""
                                                 (fn [_] (update-provider (get (:auth-providers (:config @connection-data)) index) (get (js->clj params) "id")))
                                                 (fn [new-value] (swap! connection-data assoc-in [:config :auth-providers index :name] new-value))]]
                         [:> material/TableCell [:f> inputs/debounced-input
                                                 (:auth-url provider)
                                                 ""
                                                 (fn [_] (update-provider (get (:auth-providers (:config @connection-data)) index) (get (js->clj params) "id")))
                                                 (fn [new-value] (swap! connection-data assoc-in [:config :auth-providers index :auth-url] new-value))]]
                         [:> material/TableCell [:f> inputs/debounced-input
                                                 (:token-url provider)
                                                 ""
                                                 (fn [_] (update-provider (get (:auth-providers (:config @connection-data)) index) (get (js->clj params) "id")))
                                                 (fn [new-value] (swap! connection-data assoc-in [:config :auth-providers index :token-url] new-value))]]
                         [:> material/TableCell [:f> inputs/debounced-input
                                                 (:redirect-url provider)
                                                 ""
                                                 (fn [_] (update-provider (get (:auth-providers (:config @connection-data)) index) (get (js->clj params) "id")))
                                                 (fn [new-value] (swap! connection-data assoc-in [:config :auth-providers index :redirect-url] new-value))]]
                         [:> material/TableCell [:f> inputs/debounced-input
                                                 (:client-id provider)
                                                 ""
                                                 (fn [_] (update-provider (get (:auth-providers (:config @connection-data)) index) (get (js->clj params) "id")))
                                                 (fn [new-value] (swap! connection-data assoc-in [:config :auth-providers index :client-id] new-value))]]
                         [:> material/TableCell [:f> inputs/debounced-input
                                                 (:client-secret provider)
                                                 ""
                                                 (fn [_] (update-provider (get (:auth-providers (:config @connection-data)) index) (get (js->clj params) "id")))
                                                 (fn [new-value] (swap! connection-data assoc-in [:config :auth-providers index :client-secret] new-value))]]
                         [:> material/TableCell [:f> inputs/debounced-input
                                                 (:scope provider)
                                                 ""
                                                 (fn [_] (update-provider (get (:auth-providers (:config @connection-data)) index) (get (js->clj params) "id")))
                                                 (fn [new-value] (swap! connection-data assoc-in [:config :auth-providers index :scope] new-value))]]
                         [:> material/TableCell [delete-auth-provider-button (:name provider) (:id provider) (get (js->clj params) "id")]]]))]]]]])]]))
        (finally (reset! loading? true) (reset! connection-data {}))))))
