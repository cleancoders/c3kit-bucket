(ns c3kit.bucket.idb-io
  (:require [cljs.reader]))

;region Schema Hashing

(def store-format-version 1)

(defn schema-hash [legend]
  (hash [store-format-version
         (into (sorted-map)
               (map (fn [[k v]] [k (into (sorted-map) v)]))
               legend)]))

;endregion

;region Version Management

(defn get-local-storage [] js/localStorage)

(defn idb-version [db-name legend]
  (try
    (let [storage      (get-local-storage)
          current-hash (str (schema-hash legend))
          hash-key     (str db-name "-schema-hash")
          ver-key      (str db-name "-schema-ver")
          stored-hash  (.getItem storage hash-key)
          ;; Floor at 99 so first version (100) exceeds any pre-existing DB version
          stored-ver   (or (some-> (.getItem storage ver-key) js/parseInt) 99)]
      (if (= current-hash stored-hash)
        stored-ver
        (let [new-ver (inc stored-ver)]
          (.setItem storage hash-key current-hash)
          (.setItem storage ver-key (str new-ver))
          new-ver)))
    ;; localStorage unavailable (e.g., service worker) — open at current version
    (catch :default _ nil)))

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

(defn- ensure-object-stores [db upgrade-tx legend]
  (let [existing (set (array-seq (.-objectStoreNames db)))
        expected (conj (set (map name (keys legend))) "_meta")]
    (doseq [store-name expected]
      (if (contains? existing store-name)
        (let [store (.objectStore upgrade-tx store-name)]
          (when-not (= "id" (.-keyPath store))
            (.deleteObjectStore db store-name)
            (.createObjectStore db store-name #js {:keyPath "id"})))
        (.createObjectStore db store-name #js {:keyPath "id"})))
    (doseq [store-name existing]
      (when-not (contains? expected store-name)
        (.deleteObjectStore db store-name)))))

(defn open [db-name legend]
  (let [version (idb-version db-name legend)
        request (if version
                  (.open js/indexedDB db-name version)
                  (.open js/indexedDB db-name))]
    (set! (.-onupgradeneeded request)
          (fn [event]
            (ensure-object-stores
              (event-result event)
              (.-transaction (.-target event))
              legend)))
    (set! (.-onblocked request)
          (fn [_] (js/console.warn "[IDB] Upgrade blocked by another connection")))
    (-> (request->promise request identity)
        (.then (fn [db]
                 (set! (.-onversionchange db) (fn [_] (.close db)))
                 db)))))

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
                      (fn [result]
                        (let [data (when result (:data (js->clj-entity result)))]
                          (if (or (nil? data) (set? data)) {} data))))))

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
