(ns plauna.core.common-records)

(defrecord ImapConnection [host user secret folder security port debug check-ssl-certs])
