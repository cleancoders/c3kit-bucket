(ns c3kit.bucket.idbc
  (:require [c3kit.bucket.api :as db]))

(defn offline-id?
  "Returns true if id is a negative number (offline-generated ID)."
  [id]
  (and (number? id) (neg? id)))

(defn sync-tx
  "Transacts an entity from offline sync. Strips offline IDs so the
   database assigns real ones. Returns the persisted entity."
  [entity]
  (if (offline-id? (:id entity))
    (db/tx (dissoc entity :id))
    (db/tx entity)))
