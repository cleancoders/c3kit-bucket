(ns c3kit.bucket.idb-common
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
  (let [version (idb-version db-name legend)]
    (js/Promise.
     (fn [resolve reject]
       (let [request (.open js/indexedDB db-name version)]
         (set! (.-onupgradeneeded request)
               (fn [event]
                 (let [db (.-result (.-target event))]
                   (ensure-object-stores db legend))))
         (set! (.-onsuccess request)
               (fn [event]
                 (resolve (.-result (.-target event)))))
         (set! (.-onerror request)
               (fn [event]
                 (reject (.-error (.-target event))))))))))

(defn close [db]
  (when db (.close db)))

;endregion

;region Serialization

(defn clj->js-entity [entity]
  #js {:id (:id entity) :data (pr-str entity)})

(defn js->clj-entity [js-obj]
  (when js-obj
    (cljs.reader/read-string (.-data js-obj))))

;endregion

;region Read Operations

(defn read-store [idb store-name]
  (js/Promise.
   (fn [resolve reject]
     (let [tx      (.transaction idb #js [store-name] "readonly")
           store   (.objectStore tx store-name)
           request (.getAll store)]
       (set! (.-onsuccess request)
             (fn [event]
               (resolve (map js->clj-entity (array-seq (.-result (.-target event)))))))
       (set! (.-onerror request)
             (fn [event]
               (reject (.-error (.-target event)))))))))

;endregion
