(ns ui.core
  (:require ["@fontsource/roboto/300.css"]
            ["@fontsource/roboto/400.css"]
            ["@fontsource/roboto/500.css"]
            ["@fontsource/roboto/700.css"]
            [ui.emails :as emails]
            [ui.email :as email]
            [ui.admin :as admin]
            [ui.connections :as conn]
            [reagent.dom.client :as rdom]
            [reagent.core :as r]
            [react-router-dom :as rr]
            ["@mui/material" :as material]
            ["@mui/material/styles" :refer [createTheme ThemeProvider]]
            ["@mui/material/CssBaseline" :default CssBaseline]
            [ui.components :as components]))

(defn sidebar []
  [:> material/Drawer
   {:variant "permanent"
    :sx {:width 200
         :flexShrink 0
         "& .MuiDrawer-paper" {:width 200
                               :boxSizing "border-box"
                               :backgroundColor "#fafafa"}}}
   [:> material/Box {:sx {:p 2}}
    [:img {:src "/plauna-banner.png" :width 150}]]
   [:> material/List {:sx {:flex 1}}
    [:> material/ListItem {:disablePadding true}
     [:> material/ListItemButton
      {:component rr/Link :to "/emails" :sx {:pl 2}}
      [:> material/ListItemText {:primary "Emails"}]]]
    [:> material/ListItem {:disablePadding true}
     [:> material/ListItemButton
      {:component rr/Link :to "/admin" :sx {:pl 2}}
      [:> material/ListItemText {:primary "Administration"}]]]
    [:> material/ListItem {:disablePadding true}
     [:> material/ListItemButton
      {:component rr/Link :to "/connections" :sx {:pl 2}}
      [:> material/ListItemText {:primary "IMAP Connections"}]]]]])

(defn main-layout []
  [:> material/Box {:sx {:display "flex" :height "100vh"}}
   [sidebar]
   [:> material/Box {:sx {:flex 1 :overflow "auto" :p 3}}
    [:> rr/Outlet]]])

(defn routes []
  [{:path "/"
    :element (r/as-element [main-layout])
    :children [{:path "emails"
                :element (r/as-element [:f> emails/emails-page])}
               {:path "emails/:id"
                :element (r/as-element [:f> email/email-page])}
               {:path "admin"
                :element (r/as-element [:f> admin/admin-page])}
               {:path "connections"
                :element (r/as-element [:f> conn/connections-page])}
               {:path "connections/:id"
                :element (r/as-element [:f> conn/connection-page :edit])}
               {:path "connections/new"
                :element (r/as-element [:f> conn/connection-page :new])}]}])

(def router 
  (rr/createBrowserRouter (clj->js (routes))))

(defn app []
  (let [theme (createTheme
               (clj->js
                {:palette
                 {:primary
                  {:main "#ff9c0763"}
                  :background
                  {:default "#ffffff"
                   :paper "#ffffff"}
                  :secondary
                  {:main "#a66dd7"}
                  :error
                  {:main "#b90000"}
                  :info
                  {:main "#ff9c03"}
                  :mode "light"
                  :success
                  {:main "#006e09"}}}))]
    [:<>
     [:> CssBaseline {:enableColorScheme true}]
     [:> ThemeProvider {:theme theme}
      [:<>
       [:> rr/RouterProvider {:router router}]
       [components/snackbar-component]]]]))

(defonce root (rdom/create-root (.getElementById js/document "app")))

(defn init
  []
  (components/start-snackbar-loop)
  (rdom/render root [app]))

(defn ^:dev/after-load re-render
  []
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code.
  ;; This function is called implicitly by its annotation.
  (init))
