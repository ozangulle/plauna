(ns plauna.specs
  (:require [clojure.spec.alpha :as s]))

(s/check-asserts true)

(s/def ::message-id string?) ; Delivery error emails don't have message-ids

(s/def ::in-reply-to (s/or :string string? :nil nil?))

(s/def ::subject (s/or :string string? :nil nil?))

(s/def ::mime-type string?)

;(s/def ::address #(re-matches #".+\@.+\..+" %))
(s/def ::address string?)

(s/def ::direction #(or (= :from %) (= :to %)))

(s/def ::participant-type #(or (= :sender %) (= :receiver %)))

(s/def ::date number?) ; Chat messages don't have date

(s/def ::charset string?)

(s/def ::transfer-encoding string?)

(s/def ::content (s/or :string string? :nil nil?))

(s/def ::name (s/or :string string? :nil nil?))

(s/def ::contact-key string?)

(s/def ::contact (s/keys :req-un [::name ::address ::direction ::contact-key]))

(s/def ::participant (s/keys :req-un [::name ::address ::type ::message-id ::contact-key]))

(s/def ::senders (s/or :s (s/coll-of ::contact) :n empty?))

(s/def ::recipients (s/or :s (s/coll-of ::contact) :n empty?))

(s/def ::participants (s/coll-of ::participant :min-count 2))

(s/def ::headers (s/keys :req-un [::message-id ::in-reply-to ::subject ::senders ::recipients ::mime-type ::date]))

(s/def ::body-part (s/keys :req-un [::message-id ::charset ::mime-type ::transfer-encoding
                                    ::content ::filename]))

(s/def ::body (s/coll-of ::body-part :min-count 1))

(s/def ::email (s/keys :req-un [::body ::headers]))
