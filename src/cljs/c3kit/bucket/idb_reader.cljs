(ns c3kit.bucket.idb-reader
  (:require [c3kit.bucket.idb-common :as common]))

;region Reading Dirty Entities

(defn- read-dirty-set [idb]
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

(defn- kind-store-names [idb]
  (->> (array-seq (.-objectStoreNames idb))
       (remove #(= "_meta" %))))

(defn dirty-entities [idb]
  (-> (read-dirty-set idb)
      (.then (fn [dirty-ids]
               (if (empty? dirty-ids)
                 (js/Promise.resolve [])
                 (let [store-names (kind-store-names idb)]
                   (-> (js/Promise.all (clj->js (map #(common/read-store idb %) store-names)))
                       (.then (fn [results]
                                (let [all-entities (mapcat identity (array-seq results))]
                                  (vec (filter #(contains? dirty-ids (:id %)) all-entities))))))))))))

;endregion

;region Clearing Dirty Entities

(defn- write-dirty-set! [idb dirty-set]
  (js/Promise.
   (fn [resolve reject]
     (let [tx      (.transaction idb #js ["_meta"] "readwrite")
           store   (.objectStore tx "_meta")
           request (.put store (common/clj->js-entity {:id "dirty" :data dirty-set}))]
       (set! (.-onsuccess request) (fn [_] (resolve dirty-set)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))

(defn- delete-entities-by-ids [idb ids]
  (let [store-names (kind-store-names idb)]
    (-> (js/Promise.all (clj->js (map #(common/read-store idb %) store-names)))
        (.then (fn [results]
                 (let [all-entities (mapcat identity (array-seq results))
                       to-delete    (filter #(contains? ids (:id %)) all-entities)]
                   (if (empty? to-delete)
                     (js/Promise.resolve nil)
                     (let [store-names-needed (into #{} (map #(name (:kind %))) to-delete)]
                       (js/Promise.
                        (fn [resolve reject]
                          (let [tx (.transaction idb (clj->js (vec store-names-needed)) "readwrite")]
                            (set! (.-oncomplete tx) (fn [_] (resolve nil)))
                            (set! (.-onerror tx) (fn [event] (reject (.-error (.-target event)))))
                            (doseq [entity to-delete]
                              (let [store (.objectStore tx (name (:kind entity)))]
                                (.delete store (:id entity)))))))))))))))

(defn clear-dirty! [idb ids-to-clear]
  (-> (read-dirty-set idb)
      (.then (fn [current]
               (write-dirty-set! idb (reduce disj current ids-to-clear))))
      (.then (fn [_]
               (delete-entities-by-ids idb ids-to-clear)))))

;endregion
