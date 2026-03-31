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

(defn sync-tx*
  "Batch sync-tx. Returns {:entities [...] :id-map {old-neg-id new-real-id}}
   so callers can remap cross-references between synced entities."
  [entities]
  (reduce (fn [{:keys [entities id-map]} entity]
            (let [old-id (:id entity)
                  result (sync-tx entity)]
              {:entities (conj entities result)
               :id-map   (if (offline-id? old-id)
                           (assoc id-map old-id (:id result))
                           id-map)}))
          {:entities [] :id-map {}}
          entities))

(def ^:private processed-syncs (atom #{}))
(def max-processed-syncs 100)

(defn- trim-processed-syncs! []
  (when (> (count @processed-syncs) max-processed-syncs)
    (reset! processed-syncs #{})))

(defn reset-sync-state!
  "Resets the processed sync set. Useful for test isolation."
  []
  (reset! processed-syncs #{}))

(defn claim-sync!
  "Returns true if this sync-id hasn't been processed yet. Thread-safe.
   Prevents duplicate sync requests from creating duplicate entities.
   Always returns true for nil sync-id."
  [sync-id]
  (if-not sync-id
    true
    (let [claimed? (atom false)]
      (swap! processed-syncs
        (fn [syncs]
          (if (contains? syncs sync-id)
            (do (reset! claimed? false) syncs)
            (do (reset! claimed? true) (conj syncs sync-id)))))
      (trim-processed-syncs!)
      @claimed?)))
