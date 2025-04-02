(ns plauna.parser
  (:require
   [plauna.core.email :refer [construct-email] :as core-email]
   [plauna.core.events :as events]
   [clojure.string :as st]
   [clojure.java.io :refer [input-stream copy]]
   [clojure.core.async :refer [chan sub] :as async]
   [taoensso.telemere :as t])
  (:gen-class)
  (:import
   (java.io InputStream Reader)
   (org.apache.james.mime4j.stream MimeConfig$Builder Field)
   (org.apache.james.mime4j.message MessageImpl MultipartImpl DefaultMessageBuilder BodyPart)
   (org.apache.james.mime4j.dom Message Header TextBody BinaryBody Multipart)
   (org.apache.james.mime4j.dom.address Group Mailbox)))

(set! *warn-on-reflection* true)

(defn stream->bytes [is]
  (let [baos (java.io.ByteArrayOutputStream.)]
    (copy is baos)
    (.toByteArray baos)))

(defn uuid [^String name] (str (java.util.UUID/nameUUIDFromBytes (.getBytes name))))

(defn parse-participants [message-id type mailbox]
  (cond (instance? Mailbox mailbox)
        {:name (.getName ^Mailbox mailbox)
         :address (.getAddress ^Mailbox mailbox)
         :contact-key (uuid (str (.getName ^Mailbox mailbox) (.getAddress ^Mailbox mailbox)))
         :message-id message-id
         :type type}
        (instance? Group mailbox) (map (fn [inner-mailbox] (parse-participants message-id type inner-mailbox)) (.getMailboxes ^Group mailbox))
        :else (do (t/log! :error ["Wrong type of mailbox for participants" (class mailbox)])
                  {:name "n/a" :address "n/a" :contact-key nil :message-id message-id :type type})))

(defn message-id [^Message message]
  (let [message-id (.getMessageId message)]
    (if (some? message-id)
      (st/trim message-id)
      "")))

(defn detect-utf8
  "Some emails have the charset uft-8 in quotation marks or escaped like \\UTF-8
  which throws UnsupportedEncodingException. This function tries to 'sanitize'
  those poorly formatted utf-8 declarations."
  [^String charset-string] (if (.equalsIgnoreCase charset-string "utf-8")
                             charset-string
                             (if (some? (re-matches #"(?i).*utf-8.*" charset-string))
                               "utf-8"
                               charset-string)))

(defn decode-body [^BinaryBody body]
  (try (new String ^bytes (stream->bytes (.getInputStream body)))
       (catch java.io.UnsupportedEncodingException e (t/log! {:level :error :error e} (.getMessage e)) (new String ^bytes (stream->bytes (.getInputStream body))))))

(defn reader->string [^Reader reader]
  (with-open [r reader] (slurp r)))

(defmulti parse-body-content (fn [body] (type body)))

(defmethod parse-body-content TextBody [^TextBody text-body] (reader->string (.getReader text-body)))

(defmethod parse-body-content BinaryBody [^BinaryBody binary-body] (decode-body binary-body))

(defmulti parse-body (fn [_ _ message] (type message)))

(defmethod parse-body MessageImpl [message-id bodies ^Message message]
  (let [body (.getBody message)]
    (if (instance? MultipartImpl body)
      (parse-body message-id bodies body)
      (conj bodies
            {:mime-type (.getMimeType message)
             :charset (.getCharset message)
             :message-id message-id
             :transfer-encoding (.getContentTransferEncoding message)
             :content (when (core-email/text-content? (.getMimeType message)) (parse-body-content body))
             :content-disposition (.getDispositionType message)
             :filename (.getFilename message)}))))

(defmethod parse-body MultipartImpl [message-id bodies ^Multipart message]
  (let [body-parts (.getBodyParts message)
        results (conj bodies (mapv #(parse-body message-id bodies %) body-parts))]
    results))

(defmethod parse-body BodyPart [message-id bodies ^BodyPart body-part]
  (let [body (.getBody body-part)]
    (if (or (instance? MultipartImpl body) (instance? MessageImpl body))
      (parse-body message-id bodies body)
      (conj bodies
            {:mime-type (.getMimeType body-part)
             :charset (.getCharset body-part)
             :message-id message-id
             :transfer-encoding (.getContentTransferEncoding body-part)
             :content (when (core-email/text-content? (.getMimeType body-part)) (parse-body-content (.getBody body-part)))
             :content-disposition (.getDispositionType body-part)
             :filename (.getFilename body-part)}))))

(defn parse-date [^MessageImpl message]
  (let [date (.getDate message)]
    (if (nil? date)
      nil
      (quot (.getTime date) 1000))))

(defn parse-headers [^MessageImpl message]
  (t/log! :debug "Parsing headers.")
  (let [in-reply-to-field ^Field (.getField ^Header (.getHeader message) "In-Reply-To")]
    {:message-id (message-id message)
     :in-reply-to (if (some? in-reply-to-field) (.getBody in-reply-to-field) nil)
     :subject (.getSubject message)
     :date (parse-date message)
     :mime-type (.getMimeType message)}))

(defn parse-email [^InputStream is]
  (t/log! :debug "Parsing new email from input stream")
  (let [mime-config (-> (MimeConfig$Builder.)
                        (.setMaxLineLen -1)
                        (.setMaxHeaderLen -1)
                        (.setMaxHeaderCount -1)
                        (.build))
        message-builder (let [builder (DefaultMessageBuilder.)]
                          (.setMimeEntityConfig builder mime-config) builder)
        message (.parseMessage message-builder is)
        headers (parse-headers message)
        senders (flatten (map (partial parse-participants (message-id message) :sender) (.getFrom message)))
        receivers (flatten (map (partial parse-participants (message-id message) :receiver) (.getTo message)))
        cc (flatten (map (partial parse-participants (message-id message) :cc) (.getCc message)))
        bcc (flatten (map (partial parse-participants (message-id message) :bcc) (.getBcc message)))
        participants (concat senders receivers cc bcc)]
    (construct-email headers (flatten (parse-body (:message-id headers) [] message)) participants)))

(defn with-message-id? [parsed-email]
  (let [message-id (get-in parsed-email [:header :message-id])]
    (if (or (nil? message-id) (empty? message-id))
      (do (t/log! {:level :error} ["Dropping parsed-email with headers" (into {} (:header parsed-email)) "Reason: message-id is empty"])
          false)
      true)))

(defn parsed-email-event [original-event parsed-email]
  (if (true? (:enrich (:options original-event)))
    (events/create-event :parsed-enrichable-email parsed-email nil original-event)
    (events/create-event :parsed-email parsed-email nil original-event)))

(defn parser-event-loop
  "Listens to :received-email.

  Options:
  :enrich - boolean

  If :enrich is true, emits a :parsed-enrichable-email event. Otherwise emits a :parsed-email event."
  [publisher events-channel]
  (let [local-channel (chan 256)]
    (sub publisher :received-email local-channel)
    (async/pipeline 4
                    events-channel
                    (comp (map (fn [event] [event (:payload event)]))
                          (map (fn [[original-event payload]] [original-event (parse-email (input-stream payload))]))
                          (filter (fn [[_ parsed-email]] (with-message-id? parsed-email)))
                          (map (fn [[original-event parsed-email]] (parsed-email-event original-event parsed-email))))
                    local-channel
                    true
                    (fn [^Throwable th]
                      (t/log! {:level :error :error th} (.getMessage th))
                      (.printStackTrace th)))))
