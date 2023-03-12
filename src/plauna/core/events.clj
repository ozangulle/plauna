(ns plauna.core.events)

(defrecord MailEvent [type data])

(defn create-email-parsed-event [email]
  (MailEvent. :email-parsed email))

(defn create-event [type options payload]
  {:type type
   :options options
   :payload payload})
