(ns plauna.client.parser
  (:require
   [clojure.string :as s])
  (:import
   (plauna.core.email Header Body-Part Participant EnrichedEmail Metadata)
   (org.eclipse.angus.mail.imap IMAPMessage)
   (jakarta.mail BodyPart Multipart Message$RecipientType)
   (jakarta.mail.internet InternetAddress)))

;; Construct email from message

(defn text? [content-type] (s/starts-with? (s/lower-case content-type) "text"))

(defn mime-type [content-type] (s/lower-case (first (s/split content-type #";"))))

(defonce fallback-charset "us-ascii")

(defn charset [content-type] (if (text? content-type)
                               (s/lower-case (or (second (s/split (second (s/split content-type #";")) #"=")) fallback-charset))
                               fallback-charset))

(defn disposition [disposition] (when (some? disposition) (s/lower-case disposition)))

(defn create-header [^IMAPMessage message]
  (new Header (.getMessageID message) (.getInReplyTo message) (.getSubject message) (mime-type (.getContentType message)) (quot (.getTime (.getSentDate message)) 1000)))

(defmulti create-body-part (fn [body-part _] (type body-part)))

(defmethod create-body-part String [content ^IMAPMessage message]
  (new Body-Part (.getMessageID message) (charset (.getContentType message)) (mime-type (.getContentType message)) (first (.getHeader message "Content-transfer-encoding")) content (.getFileName message) (.getDisposition message)))

(defmethod create-body-part BodyPart [^BodyPart bodypart ^IMAPMessage message]
  (if (instance? Multipart (.getContent bodypart))
    (create-body-part (.getContent bodypart) message)
    (new Body-Part (.getMessageID message) (charset (.getContentType bodypart)) (mime-type (.getContentType bodypart)) (first (.getHeader bodypart "Content-transfer-encoding")) (.getContent bodypart) (.getFileName bodypart) (disposition (.getDisposition bodypart)))))

(defmethod create-body-part Multipart [^Multipart multipart ^IMAPMessage message]
  (for [i (range 0 (.getCount multipart))] (doall (create-body-part (.getBodyPart multipart i) message))))

;; TODO remove duplication with parser.clj
(defn uuid [^String name] (str (java.util.UUID/nameUUIDFromBytes (.getBytes name))))

(defmulti create-participant (fn [address _ _] (type address)))

(defmethod create-participant InternetAddress [^InternetAddress address contact-type message-id]
  (let [name (.getPersonal address)
        address (.getAddress address)
        contact-key (uuid (str name address))]
    (new Participant address name contact-key contact-type message-id)))

(defn create-participants [^IMAPMessage message]
  (let [sender (.getSender message)
        message-id (.getMessageID message)
        sender-participant (create-participant sender :sender message-id)
        recipient-participants (mapv (fn [address] (create-participant address :receiver message-id)) (.getRecipients message Message$RecipientType/TO))
        cc-participants (mapv (fn [address] (create-participant address :cc message-id)) (.getRecipients message Message$RecipientType/CC))
        bcc-participants (mapv (fn [address] (create-participant address :cc message-id)) (.getRecipients message Message$RecipientType/BCC))]
    (flatten [sender-participant recipient-participants cc-participants bcc-participants])))

(defn message->email [^IMAPMessage message connection-id]
  (let [headers (create-header message)]
    (new EnrichedEmail
         headers
         (flatten [(create-body-part (.getContent message) message)])
         (create-participants message)
         (new Metadata (:message-id headers)
              nil
              nil
              nil
              nil
              nil
              nil
              nil
              connection-id))))
