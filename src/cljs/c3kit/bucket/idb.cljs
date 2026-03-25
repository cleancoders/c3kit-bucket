(ns c3kit.bucket.idb
  (:refer-clojure :rename {reduce core-reduce})
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as common]
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

(defn put-entity [idb entity]
  (js/Promise.
   (fn [resolve reject]
     (let [store-name (name (:kind entity))
           tx         (.transaction idb #js [store-name] "readwrite")
           store      (.objectStore tx store-name)
           request    (.put store (common/clj->js-entity entity))]
       (set! (.-onsuccess request) (fn [_] (resolve entity)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))

(defn put-entities [idb entities]
  (if (empty? entities)
    (js/Promise.resolve entities)
    (let [store-names (into #{} (map #(name (:kind %))) entities)]
      (js/Promise.
       (fn [resolve reject]
         (let [tx (.transaction idb (clj->js (vec store-names)) "readwrite")]
           (set! (.-oncomplete tx) (fn [_] (resolve entities)))
           (set! (.-onerror tx) (fn [event] (reject (.-error (.-target event)))))
           (set! (.-onabort tx) (fn [event] (reject (.-error (.-target event)))))
           (doseq [entity entities]
             (let [store (.objectStore tx (name (:kind entity)))]
               (.put store (common/clj->js-entity entity))))))))))

(defn delete-entity [idb kind id]
  (js/Promise.
   (fn [resolve reject]
     (let [store-name (name kind)
           tx         (.transaction idb #js [store-name] "readwrite")
           store      (.objectStore tx store-name)
           request    (.delete store id)]
       (set! (.-onsuccess request) (fn [_] (resolve nil)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))

(defn clear-store [idb kind]
  (js/Promise.
   (fn [resolve reject]
     (let [store-name (name kind)
           tx         (.transaction idb #js [store-name] "readwrite")
           store      (.objectStore tx store-name)
           request    (.clear store)]
       (set! (.-onsuccess request) (fn [_] (resolve nil)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))

(defn clear-all [idb]
  (let [store-names (->> (array-seq (.-objectStoreNames idb))
                         (remove #(= "_meta" %)))]
    (if (empty? store-names)
      (js/Promise.resolve nil)
      (js/Promise.
       (fn [resolve reject]
         (let [tx (.transaction idb (clj->js (vec store-names)) "readwrite")]
           (set! (.-oncomplete tx) (fn [_] (resolve nil)))
           (set! (.-onerror tx) (fn [event] (reject (.-error (.-target event)))))
           (doseq [store-name store-names]
             (.clear (.objectStore tx store-name)))))))))

(defn rehydrate [idb kinds tx-fn]
  (let [store-names (if (seq kinds)
                      (map name kinds)
                      (->> (array-seq (.-objectStoreNames idb))
                           (remove #(= "_meta" %))))]
    (-> (js/Promise.all (clj->js (map #(common/read-store idb %) store-names)))
        (.then (fn [results]
                 (let [all-entities (mapcat identity (array-seq results))]
                   (when (seq all-entities)
                     (tx-fn all-entities))))))))

;endregion

;region Dirty Set Operations

(defn read-dirty-set [idb]
  (js/Promise.
   (fn [resolve reject]
     (let [tx      (.transaction idb #js ["_meta"] "readonly")
           store   (.objectStore tx "_meta")
           request (.get store "dirty")]
       (set! (.-onsuccess request)
             (fn [event]
               (let [result (.-result (.-target event))]
                 (resolve (if result (:data (common/js->clj-entity result)) #{})))))
       (set! (.-onerror request)
             (fn [event]
               (reject (.-error (.-target event)))))))))

(defn- write-dirty-set! [idb dirty-set]
  (js/Promise.
   (fn [resolve reject]
     (let [tx      (.transaction idb #js ["_meta"] "readwrite")
           store   (.objectStore tx "_meta")
           request (.put store (common/clj->js-entity {:id "dirty" :data dirty-set}))]
       (set! (.-onsuccess request) (fn [_] (resolve dirty-set)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))

(defn add-to-dirty-set! [idb ids]
  (-> (read-dirty-set idb)
      (.then (fn [current] (write-dirty-set! idb (into current ids))))))

(defn remove-from-dirty-set! [idb ids]
  (-> (read-dirty-set idb)
      (.then (fn [current] (write-dirty-set! idb (reduce disj current ids))))))

;endregion

;region Shared Transaction Helpers

(defn- prepare-entity
  "Computes new store state with entity applied, without mutating.
   Returns [new-store coerced-entity-or-soft-delete]."
  [db entity]
  (let [entity    (memory/ensure-id entity)
        new-store (memory/tx-entity @(.-legend db) @(.-store db) entity)
        result    (if (api/delete? entity)
                    (api/soft-delete entity)
                    (get-in new-store [:all (:id entity)]))]
    [new-store result]))

(defn idb-tx
  "Updates in-memory store optimistically, then persists to IDB in the background.
   Returns the saved entity synchronously. Rolls back memory on IDB failure."
  [db entity]
  (let [old-store @(.-store db)
        [new-store result] (prepare-entity db entity)]
    (reset! (.-store db) new-store)
    (when @(.-idb-atom db)
      (-> (if (api/delete? entity)
            (delete-entity @(.-idb-atom db) (:kind entity) (:id entity))
            (put-entity @(.-idb-atom db) result))
          (.catch (fn [_] (reset! (.-store db) old-store)))))
    result))

(defn idb-tx*
  "Updates in-memory store optimistically, then persists to IDB in the background.
   Returns the results synchronously. Rolls back memory on IDB failure."
  [db entities]
  (let [old-store  @(.-store db)
        entities   (map memory/ensure-id entities)
        new-store  (core-reduce #(memory/tx-entity @(.-legend db) %1 %2) @(.-store db) entities)
        results    (mapv (fn [e]
                           (if (api/delete? e)
                             (api/soft-delete e)
                             (get-in new-store [:all (:id e)])))
                         entities)
        to-persist (remove api/delete? results)
        to-delete  (filter api/delete? entities)]
    (reset! (.-store db) new-store)
    (when @(.-idb-atom db)
      (-> (js/Promise.all
            (clj->js (cond-> []
                       (seq to-persist) (conj (put-entities @(.-idb-atom db) to-persist))
                       (seq to-delete)  (into (map #(delete-entity @(.-idb-atom db) (:kind %) (:id %)) to-delete)))))
          (.catch (fn [_] (reset! (.-store db) old-store)))))
    results))

(defn init!
  "Opens the IDB database and optionally rehydrates the in-memory store.
   Must be called and awaited before using the db.
   Returns a js/Promise resolving to the db instance."
  [db & kinds]
  (-> (common/open (.-db-name db) @(.-legend db))
      (.then (fn [idb-instance]
               (reset! (.-idb-atom db) idb-instance)
               db))
      (.then (fn [db]
               ;; Uses memory/tx* directly because data is already in IDB —
               ;; we're loading it into the read cache, not persisting it.
               (rehydrate @(.-idb-atom db) (seq kinds)
                          (fn [entities] (memory/tx* db entities)))))
      (.then (fn [_] db))))

(defn rehydrate!
  "Rehydrates the in-memory store from IDB. Use after init! has been called.
   Returns a js/Promise resolving to the db instance."
  [db & kinds]
  ;; Uses memory/tx* directly because data is already in IDB —
  ;; we're loading it into the read cache, not persisting it.
  (-> (rehydrate @(.-idb-atom db) (seq kinds)
                 (fn [entities] (memory/tx* db entities)))
      (.then (fn [_] db))))

;endregion
