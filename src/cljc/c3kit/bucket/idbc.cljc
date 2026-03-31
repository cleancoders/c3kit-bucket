(ns c3kit.bucket.idbc)

(defn offline-id?
  "Returns true if id is a negative number (offline-generated ID)."
  [id]
  (and (number? id) (neg? id)))
