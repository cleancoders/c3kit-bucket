(ns c3kit.bucket.idb-common
  (:refer-clojure :rename {reduce core-reduce})
  (:require [c3kit.apron.log :as log]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-io :as io]
            [c3kit.bucket.memory :as memory]))

;region Offline ID Generation

(def offline-id-counter (atom 0))

(defn ensure-offline-id [entity]
  (if (:id entity)
    entity
    (assoc entity :id (swap! offline-id-counter dec))))

(defn offline-ensure-id [online-fn entity]
  (if (online-fn)
    (memory/ensure-id entity)
    (ensure-offline-id entity)))

;endregion

;region Entity Operations

(defn- with-rollback [promise store-atom old-store]
  (.catch promise (fn [e]
                    (log/warn "IDB write failed, rolling back in-memory store:" e)
                    (reset! store-atom old-store))))

(defn put-entity [idb entity]
  (io/rw-request idb (name (:kind entity))
                     #(.put % (io/clj->js-entity entity))
                     entity))

(defn put-entities [idb entities]
  (if (empty? entities)
    (js/Promise.resolve entities)
    (let [store-names (into #{} (map #(name (:kind %))) entities)]
      (io/batch-tx idb store-names
                       (fn [tx]
                         (doseq [entity entities]
                           (.put (.objectStore tx (name (:kind entity)))
                                 (io/clj->js-entity entity))))
                       entities))))

(defn delete-entity [idb kind id]
  (io/rw-request idb (name kind) #(.delete % id) nil))

(defn clear-store [idb kind]
  (io/rw-request idb (name kind) #(.clear %) nil))

(defn clear-all [idb]
  (let [store-names (io/entity-store-names idb)]
    (if (empty? store-names)
      (js/Promise.resolve nil)
      (io/batch-tx idb store-names
                       (fn [tx]
                         (doseq [store-name store-names]
                           (.clear (.objectStore tx store-name))))
                       nil))))

(defn- rehydrate [idb kinds tx-fn]
  (let [read-promise (if (seq kinds)
                       (-> (js/Promise.all (clj->js (map #(io/read-store idb %) (map name kinds))))
                           (.then (fn [results] (mapcat identity (array-seq results)))))
                       (io/read-all-entities idb))]
    (-> read-promise
        (.then (fn [all-entities]
                 (when (seq all-entities)
                   (tx-fn all-entities)))))))

;endregion

;region Dirty Set Operations

(def read-dirty-set io/read-dirty-set)
(def dirty-chain (atom (js/Promise.resolve nil)))

(defn add-to-dirty-set! [idb entries]
  (swap! dirty-chain
    (fn [chain]
      (-> chain
          (.catch (fn [_] nil))
          (.then (fn [_] (io/read-dirty-set idb)))
          (.then (fn [current] (io/write-dirty-set! idb (merge current entries))))))))

(defn remove-from-dirty-set! [idb ids]
  (swap! dirty-chain
    (fn [chain]
      (-> chain
          (.catch (fn [_] nil))
          (.then (fn [_] (io/read-dirty-set idb)))
          (.then (fn [current] (io/write-dirty-set! idb (apply dissoc current ids))))))))

;endregion

;region Sync Lifecycle

(defn sync! [db callback]
  (let [idb @(.-idb-atom db)]
    (-> (io/read-dirty-set idb)
        (.then (fn [dirty-ids]
                 (if (empty? dirty-ids)
                   (callback [])
                   (-> (io/read-all-entities idb)
                       (.then (fn [all-entities]
                                (callback (filterv #(contains? dirty-ids (:id %)) all-entities)))))))))))

(defn- soft-delete-neg-ids! [db dirty-ids]
  (doseq [neg-id (filter neg? dirty-ids)]
    (when-let [entity (get-in @(.-store db) [:all neg-id])]
      (swap! (.-store db) #(memory/tx-entity @(.-legend db) % (api/soft-delete entity))))))

(defn- delete-dirty-entities [idb dirty-ids]
  (-> (io/read-all-entities idb)
      (.then (fn [all-entities]
               (let [to-delete (filter #(contains? dirty-ids (:id %)) all-entities)]
                 (js/Promise.all (clj->js (map #(delete-entity idb (:kind %) (:id %)) to-delete))))))))

(defn sync-complete! [db dirty-ids server-entities]
  (let [idb @(.-idb-atom db)]
    (soft-delete-neg-ids! db dirty-ids)
    (when (seq server-entities) (memory/tx* db server-entities))
    (-> (delete-dirty-entities idb dirty-ids)
        (.then (fn [_] (remove-from-dirty-set! idb dirty-ids)))
        (.then (fn [_]
                 (if (seq server-entities)
                   (put-entities idb server-entities)
                   (js/Promise.resolve nil)))))))

(defn- purge-neg-entities-from-memory! [db kinds]
  (let [neg-entries (for [kind kinds
                          entity (memory/do-find db kind {})
                          :when (neg? (:id entity))]
                      entity)]
    (doseq [entity neg-entries]
      (swap! (.-store db) #(memory/tx-entity @(.-legend db) % (api/soft-delete (:kind entity) (:id entity)))))
    neg-entries))

(defn- sync-idb-after-refresh! [idb neg-entries server-entities]
  (-> (js/Promise.all
        (clj->js (cond-> []
                   (seq neg-entries) (into (map #(delete-entity idb (:kind %) (:id %)) neg-entries))
                   (seq server-entities) (conj (put-entities idb server-entities)))))
      (.catch (fn [_] nil))))

(defn refresh!
  "Purges all negative-ID entities for the kinds present in server-entities
   from both memory and IDB, then tx*s the server data as clean (not dirty-tracked).
   Use when receiving fresh server data that replaces offline-created entities."
  [db server-entities]
  (if (empty? server-entities)
    []
    (let [idb         @(.-idb-atom db)
          neg-entries (purge-neg-entities-from-memory! db (set (map :kind server-entities)))
          result      (memory/tx* db server-entities)]
      (when idb (sync-idb-after-refresh! idb neg-entries server-entities))
      result)))

;endregion

;region Optimistic Transaction Functions

(defn- offline? [db] (not ((.-online-fn db))))

(defn- prepare-entity
  "Computes new store state with entity applied, without mutating.
   Returns [new-store coerced-entity-or-soft-delete]."
  [db entity]
  (let [online-fn (.-online-fn db)
        entity    (offline-ensure-id online-fn entity)
        new-store (memory/tx-entity @(.-legend db) @(.-store db) entity)
        result    (if (api/delete? entity)
                    (api/soft-delete entity)
                    (get-in new-store [:all (:id entity)]))]
    [new-store result]))

(defn- persist-offline! [idb entity result]
  (let [id      (:id result)
        delete? (api/delete? entity)]
    (if (and delete? (neg? id))
      (-> (delete-entity idb (:kind entity) id)
          (.then (fn [_] (remove-from-dirty-set! idb #{id}))))
      (-> (put-entity idb result)
          (.then (fn [_] (add-to-dirty-set! idb #{id})))))))

(defn- persist-online! [idb entity result]
  (if (api/delete? entity)
    (delete-entity idb (:kind entity) (:id entity))
    (put-entity idb result)))

(defn idb-tx
  "Updates in-memory store optimistically, then persists to IDB in the background.
   When offline, tracks dirty entities and handles tombstones for deletes.
   Returns the saved entity synchronously. Rolls back memory on IDB failure."
  [db entity]
  (let [old-store @(.-store db)
        [new-store result] (prepare-entity db entity)]
    (reset! (.-store db) new-store)
    (when-let [idb @(.-idb-atom db)]
      (with-rollback
        (if (offline? db)
          (persist-offline! idb entity result)
          (persist-online! idb entity result))
        (.-store db) old-store))
    result))

(defn- persist-batch-offline! [idb entities results]
  (let [neg-deletes  (filterv #(and (api/delete? %) (neg? (:id %))) entities)
        pos-deletes  (filterv #(and (:db/delete? %) (pos? (:id %))) results)
        to-persist   (filterv #(not (:db/delete? %)) results)
        dirty-add    (into #{} (map :id) (concat to-persist pos-deletes))
        dirty-remove (into #{} (map :id) neg-deletes)]
    (js/Promise.all
      (clj->js (cond-> []
                 (seq to-persist) (conj (put-entities idb to-persist))
                 (seq pos-deletes) (conj (put-entities idb pos-deletes))
                 (seq neg-deletes) (into (map #(delete-entity idb (:kind %) (:id %)) neg-deletes))
                 (seq dirty-add) (conj (add-to-dirty-set! idb dirty-add))
                 (seq dirty-remove) (conj (remove-from-dirty-set! idb dirty-remove)))))))

(defn- persist-batch-online! [idb entities results]
  (let [to-persist (remove api/delete? results)
        to-delete  (filter api/delete? entities)]
    (js/Promise.all
      (clj->js (cond-> []
                 (seq to-persist) (conj (put-entities idb to-persist))
                 (seq to-delete) (into (map #(delete-entity idb (:kind %) (:id %)) to-delete)))))))

(defn idb-tx*
  "Updates in-memory store optimistically, then persists to IDB in the background.
   When offline, tracks dirty entities and handles tombstones for deletes.
   Returns the results synchronously. Rolls back memory on IDB failure."
  [db entities]
  (let [old-store @(.-store db)
        entities  (map (partial offline-ensure-id (.-online-fn db)) entities)
        new-store (core-reduce #(memory/tx-entity @(.-legend db) %1 %2) @(.-store db) entities)
        results   (mapv (fn [e]
                          (if (api/delete? e)
                            (api/soft-delete e)
                            (get-in new-store [:all (:id e)])))
                        entities)]
    (reset! (.-store db) new-store)
    (when-let [idb @(.-idb-atom db)]
      (with-rollback
        (if (offline? db)
          (persist-batch-offline! idb entities results)
          (persist-batch-online! idb entities results))
        (.-store db) old-store))
    results))

;endregion

;region Initialization

(defn init!
  "Opens the IDB database and optionally rehydrates the in-memory store.
   Must be called and awaited before using the db.
   Returns a js/Promise resolving to the db instance."
  [db & kinds]
  (-> (io/open (.-db-name db) @(.-legend db))
      (.then (fn [idb-instance]
               (reset! (.-idb-atom db) idb-instance)
               db))
      (.then (fn [db]
               (rehydrate @(.-idb-atom db) (seq kinds)
                          (fn [entities] (memory/tx* db entities)))))
      (.then (fn [_] db))))

(defn rehydrate!
  "Rehydrates the in-memory store from IDB. Use after init! has been called.
   Returns a js/Promise resolving to the db instance."
  [db & kinds]
  (-> (rehydrate @(.-idb-atom db) (seq kinds)
                 (fn [entities] (memory/tx* db entities)))
      (.then (fn [_] db))))

;endregion
