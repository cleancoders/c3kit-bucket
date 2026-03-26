(ns c3kit.bucket.idb-io
  (:require [cljs.reader]))

;region Schema Hashing

(defn schema-hash [legend]
  (hash (into (sorted-map)
              (map (fn [[k v]] [k (into (sorted-map) v)]))
              legend)))

;endregion

;region Version Management

(defn idb-version [db-name legend]
  (let [current-hash (str (schema-hash legend))
        hash-key     (str db-name "-schema-hash")
        ver-key      (str db-name "-schema-ver")
        stored-hash  (.getItem js/localStorage hash-key)
        stored-ver   (or (some-> (.getItem js/localStorage ver-key) js/parseInt) 0)]
    (if (= current-hash stored-hash)
      stored-ver
      (let [new-ver (inc stored-ver)]
        (.setItem js/localStorage hash-key current-hash)
        (.setItem js/localStorage ver-key (str new-ver))
        new-ver))))

;endregion

;region Serialization

(defn clj->js-entity [entity]
  #js {:id (:id entity) :data (pr-str entity)})

(defn js->clj-entity [js-obj]
  (when js-obj
    (cljs.reader/read-string (.-data js-obj))))

;endregion

;region JS Interop Helpers

(defn event-error [event] (.-error (.-target event)))
(defn event-result [event] (.-result (.-target event)))

(defn on-success! [request callback]
  (set! (.-onsuccess request) callback))

(defn on-error! [request callback]
  (set! (.-onerror request) callback))

(defn request->promise
  "Wraps an IDB request in a promise. Calls result-fn with the request result on success."
  [request result-fn]
  (js/Promise.
   (fn [resolve reject]
     (on-success! request (fn [event] (resolve (result-fn (event-result event)))))
     (on-error! request (fn [event] (reject (event-error event)))))))

(defn entity-store-names
  "Returns non-meta store names from an IDB instance."
  [idb]
  (->> (array-seq (.-objectStoreNames idb))
       (remove #(= "_meta" %))))

;endregion

;region Database Operations

(defn- ensure-object-stores [db legend]
  (let [existing (set (array-seq (.-objectStoreNames db)))
        expected (conj (set (map name (keys legend))) "_meta")]
    (doseq [store-name expected]
      (when-not (contains? existing store-name)
        (.createObjectStore db store-name #js {:keyPath "id"})))
    (doseq [store-name existing]
      (when-not (contains? expected store-name)
        (.deleteObjectStore db store-name)))))

(defn open [db-name legend]
  (let [version (idb-version db-name legend)
        request (.open js/indexedDB db-name version)]
    (set! (.-onupgradeneeded request)
          (fn [event] (ensure-object-stores (event-result event) legend)))
    (request->promise request identity)))

(defn close [db]
  (when db (.close db)))

;endregion

;region Transaction Helpers

(defn rw-request
  "Opens a readwrite transaction on a single store and applies op-fn to the store.
   Returns a promise resolving to result-val on success."
  [idb store-name op-fn result-val]
  (try
    (let [tx      (.transaction idb #js [store-name] "readwrite")
          store   (.objectStore tx store-name)
          request (op-fn store)]
      (request->promise request (constantly result-val)))
    (catch :default e (js/Promise.reject e))))

(defn batch-tx
  "Opens a readwrite transaction across multiple stores. Calls work-fn with the tx,
   then resolves to result-val when the transaction completes."
  [idb store-names work-fn result-val]
  (js/Promise.
   (fn [resolve reject]
     (let [tx        (.transaction idb (clj->js (vec store-names)) "readwrite")
           on-reject (fn [event] (reject (event-error event)))]
       (set! (.-oncomplete tx) (fn [_] (resolve result-val)))
       (on-error! tx on-reject)
       (set! (.-onabort tx) on-reject)
       (work-fn tx)))))

;endregion

;region Dirty Set Operations

(defn read-dirty-set [idb]
  (let [tx      (.transaction idb #js ["_meta"] "readonly")
        store   (.objectStore tx "_meta")
        request (.get store "dirty")]
    (request->promise request
                      (fn [result] (if result (:data (js->clj-entity result)) #{})))))

(defn write-dirty-set! [idb dirty-set]
  (rw-request idb "_meta"
              #(.put % (clj->js-entity {:id "dirty" :data dirty-set}))
              dirty-set))

;endregion

;region Read Operations

(defn read-store [idb store-name]
  (let [tx      (.transaction idb #js [store-name] "readonly")
        store   (.objectStore tx store-name)
        request (.getAll store)]
    (request->promise request (fn [result] (map js->clj-entity (array-seq result))))))

(defn read-entity
  "Reads a single entity by ID from a specific store. Returns a promise of the entity or nil."
  [idb store-name id]
  (let [tx      (.transaction idb #js [store-name] "readonly")
        store   (.objectStore tx store-name)
        request (.get store id)]
    (request->promise request js->clj-entity)))

(defn read-all-entities
  "Reads all entities from all non-meta stores. Returns a promise of a flat seq of entities."
  [idb]
  (let [store-names (entity-store-names idb)]
    (-> (js/Promise.all (clj->js (map #(read-store idb %) store-names)))
        (.then (fn [results] (mapcat identity (array-seq results)))))))

;endregion
