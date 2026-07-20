(ns ui.core
  (:require
   ["@fontsource/roboto/300.css"]
   ["@fontsource/roboto/400.css"]
   ["@fontsource/roboto/500.css"]
   ["@fontsource/roboto/700.css"]
   ["@mui/material" :as material]
   ["@mui/material/CssBaseline" :default CssBaseline]
   ["@mui/material/styles" :refer [createTheme ThemeProvider]]
   ["@mui/icons-material/Mail" :default MailIcon]
   ["@mui/icons-material/Settings" :default SettingsIcon]
   ["@mui/icons-material/Storage" :default StorageIcon]
   [react-router-dom :as rr]
   [reagent.core :as r]
   [reagent.dom.client :as rdom]
   [ui.admin :as admin]
   [ui.components :as components]
   [ui.connections :as conn]
   [ui.email :as email]
   [ui.emails :as emails]))

(def theme-atom (r/atom nil))

(defn sidebar []
  [:> material/Drawer
   {:variant "permanent"
    :sx {:width 240
         :flexShrink 0
         "& .MuiDrawer-paper" {:width 240
                               :boxSizing "border-box"
                               :backgroundColor (.. @theme-atom -palette -background -default)
                               :borderRight (str "1px solid " (.. @theme-atom -palette -divider))
                               :display "flex"
                               :flexDirection "column"}}}

   [:> material/Box
    {:sx {:p 3
          :display "flex"
          :alignItems "center"
          :justifyContent "center"
          :borderBottom (str "1px solid " (.. @theme-atom -palette -divider))}}
    [:img {:src "/plauna-banner.png" :width 150 :style {:objectFit "contain"}}]]

   [:> material/List
    {:sx {:flex 1
          :pt 2}}
    (doall
     (for [item [{:label "Emails" :path "/emails" :icon MailIcon}
                 {:label "Administration" :path "/admin" :icon SettingsIcon}
                 {:label "IMAP Connections" :path "/connections" :icon StorageIcon}]]
       ^{:key (:path item)}
       [:> material/ListItem
        {:disablePadding true
         :sx {"&:hover" {:backgroundColor (.. @theme-atom -palette -action -hover)}}}
        [:> material/ListItemButton
         {:component rr/Link
          :to (:path item)
          :sx {:pl 2
               :py 1.5
               :color (.. @theme-atom -palette -text -primary)
               :textDecoration "none"
               "&:hover" {:backgroundColor (.. @theme-atom -palette -action -hover)
                          :color (.. @theme-atom -palette -primary -main)}
               "&.active" {:backgroundColor (.. @theme-atom -palette -primary -lighter)
                           :color (.. @theme-atom -palette -primary -main)
                           :fontWeight 600}}}
         [:> material/ListItemIcon
          {:sx {:minWidth 40
                :color "inherit"}}
          [:> (:icon item)]]
         [:> material/ListItemText
          {:primary (:label item)
           :sx {:ml 1}}]]]))]

   [:> material/Box {:sx {:flex 1}}]

   ;; TODO Add the version there later
   [:> material/Box
    {:sx {:p 2
          :borderTop (str "1px solid " (.. @theme-atom -palette -divider))
          :textAlign "center"
          :fontSize "0.75rem"
          :color (.. @theme-atom -palette -text -secondary)}}
    ""]])

(defn main-layout []
  [:> material/Box {:sx {:display "flex" :height "100vh"}}
   [sidebar]
   [:> material/Box {:sx {:flex 1 :overflow "auto" :p 3}}
    [:> rr/Outlet]]])

(defn routes []
  [{:path "/"
    :element (r/as-element [main-layout])
    :children [{:index true
                :element (r/as-element [:> rr/Navigate {:to "emails" :replace true}])}
               {:path "emails"
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
    (reset! theme-atom theme)
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
