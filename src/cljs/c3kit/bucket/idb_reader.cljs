(ns c3kit.bucket.idb-reader
  (:require [c3kit.bucket.idb-io :as io]))

;region Reading Dirty Entities

(defn dirty-entities [idb]
  (-> (io/read-dirty-set idb)
      (.then (fn [dirty-entries]
               (if (empty? dirty-entries)
                 (js/Promise.resolve [])
                 (-> (js/Promise.all
                       (clj->js (map (fn [[id kind]] (io/read-entity idb (name kind) id)) dirty-entries)))
                     (.then (fn [results]
                              (vec (remove nil? (array-seq results)))))))))))

;endregion

;region Clearing Dirty Entities

(defn clear-dirty! [idb ids-to-clear]
  (let [id-set (set ids-to-clear)]
    (-> (io/read-dirty-set idb)
        (.then (fn [dirty-entries]
                 (let [entries-to-delete (select-keys dirty-entries id-set)
                       remaining         (apply dissoc dirty-entries id-set)]
                   (-> (io/write-dirty-set! idb remaining)
                       (.then (fn [_]
                                (if (empty? entries-to-delete)
                                  (js/Promise.resolve nil)
                                  (let [store-names (into #{} (map (comp name val)) entries-to-delete)]
                                    (io/batch-tx idb store-names
                                                     (fn [tx]
                                                       (doseq [[id kind] entries-to-delete]
                                                         (.delete (.objectStore tx (name kind)) id)))
                                                     nil))))))))))))

;endregion
