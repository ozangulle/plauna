(ns plauna.parser
  (:require
   [plauna.core.email :refer [construct-email]]
   [plauna.core.events :as events]
   [clojure.string :as st]
   [clojure.java.io :refer [input-stream copy]]
   [clojure.core.async :refer [chan sub] :as async]
   [taoensso.telemere :as t])
  (:gen-class)
  (:import
   (java.io InputStream)
   (org.jsoup Jsoup)
   (opennlp.tools.util.normalizer AggregateCharSequenceNormalizer EmojiCharSequenceNormalizer NumberCharSequenceNormalizer ShrinkCharSequenceNormalizer TwitterCharSequenceNormalizer CharSequenceNormalizer)
   (org.apache.james.mime4j.stream MimeConfig$Builder Field)
   (org.apache.james.mime4j.message MessageImpl MultipartImpl DefaultMessageBuilder)
   (org.apache.james.mime4j.dom SingleBody Entity Message Multipart Header)
   (org.apache.james.mime4j.dom.address Group Mailbox)
   (javax.swing.text.rtf RTFEditorKit)))

(set! *warn-on-reflection* true)

(defn html->text [^String html] (.text (Jsoup/parse html "UTF-8")))

(def BracketsNormalizer (reify CharSequenceNormalizer
                          (normalize [_ text] ((comp st/trim #(st/replace % #"\( \)" "")) text))))

(def MailtoNormalizer (reify CharSequenceNormalizer
                        (normalize [_ text] ((comp st/trim #(st/replace % #"(mailto:)?(?<![-+_.0-9A-Za-z])[-+_.0-9A-Za-z]+@[-0-9A-Za-z]+[-.0-9A-Za-z]+" "")) text))))

(def BetterURLNormalizer (reify CharSequenceNormalizer
                           (normalize [_ text] ((comp st/trim #(st/replace % #"https?://[-_.?&~%;+=/#0-9A-Za-z]+" "")) text))))

(def ^CharSequenceNormalizer normalizer (new AggregateCharSequenceNormalizer
                                             (into-array CharSequenceNormalizer
                                                         [BetterURLNormalizer
                                                          MailtoNormalizer
                                                          (EmojiCharSequenceNormalizer/getInstance)
                                                          (TwitterCharSequenceNormalizer/getInstance)
                                                          (NumberCharSequenceNormalizer/getInstance)
                                                          (ShrinkCharSequenceNormalizer/getInstance)
                                                          BracketsNormalizer
                                                          (ShrinkCharSequenceNormalizer/getInstance)])))

(defn rtf->string [^String rtf]
  (let [rtf-parser (new RTFEditorKit)
        document (.createDefaultDocument rtf-parser)]
    (.read rtf-parser (input-stream (.getBytes rtf)) document 0)
    (.getText document 0 (.getLength document))))

(defn normalize [^String text] (.normalize normalizer text))

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

(defn clean-text-content [content html? rtf?]
  (cond html? (html->text content)
        rtf? (rtf->string content)
        :else (html->text content)))

(defn detect-utf8
  "Some emails have the charset uft-8 in quotation marks or escaped like \\UTF-8
  which throws UnsupportedEncodingException. This function tries to 'sanitize'
  those poorly formatted utf-8 declarations."
  [^String charset-string] (if (.equalsIgnoreCase charset-string "utf-8")
                             charset-string
                             (if (some? (re-matches #"(?i).*utf-8.*" charset-string))
                               "utf-8"
                               charset-string)))

(defn decode-body [^SingleBody body ^String charset]
  (try (new String ^bytes (stream->bytes (.getInputStream body)) ^String (detect-utf8 charset))
       (catch java.io.UnsupportedEncodingException e (t/log! :error e) (new String ^bytes (stream->bytes (.getInputStream body))))))

(defn parse-body [message-id bodies ^Message message]
  (t/log! :debug "Parsing body.")
  (when (nil? message) bodies)
  (cond
    (instance? MultipartImpl (.getBody message))
    (let [body-parts (.getBodyParts ^Multipart (.getBody message))
          results (conj bodies (map #(parse-body message-id bodies %) body-parts))]
      results)

    (instance? MessageImpl (.getBody message))
    (parse-body message-id bodies (.getBody message))

    :else
    (let [content (decode-body (.getBody message) (.getCharset message))
          transfer-encoding (try (.getContentTransferEncoding ^Entity message) (catch Exception _ ""))
          text? (.startsWith ^String (.getMimeType message) "text")
          rtf? (.endsWith ^String (.getMimeType message) "rtf")
          html? (.contains ^String (.getMimeType message) "html")]
      (conj bodies
            {:mime-type (.getMimeType message)
             :charset (.getCharset message)
             :message-id message-id
             :transfer-encoding transfer-encoding
             :original-content (when text? content)
             :sanitized-content (when text? (normalize (clean-text-content content html? rtf?)))
             :name (when (instance? Entity (.getBody message)) (.getFilename ^Entity (.getBody message)))}))))

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
  (let [message-id (-> parsed-email :header :message-id)]
    (if (or (nil? message-id) (empty? message-id))
      (do (t/log! :error ["Dropping parsed-email with headers" (into {} (:header parsed-email)) "Reason: message-id is empty"])
          false)
      true)))

(defn parsed-email-event [original-event parsed-email]
  (if (true? (:enrich (:options original-event)))
    (events/create-event :parsed-enrichable-email parsed-email nil original-event)
    (events/create-event :parsed-email parsed-email nil original-event)))

(defn listen-to-events
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
                    local-channel)))
