(ns c3kit.bucket.idb-reader
  (:require [c3kit.bucket.idb-io :as io]))

;region Helpers

(defn- read-by-entries
  "Reads specific entities from IDB given a dirty entries map {id kind}."
  [idb dirty-entries]
  (-> (js/Promise.all
        (clj->js (map (fn [[id kind]] (io/read-entity idb (name kind) id)) dirty-entries)))
      (.then (fn [results] (vec (remove nil? (array-seq results)))))))

(defn- delete-by-entries
  "Deletes specific entities from IDB given a dirty entries map {id kind}."
  [idb entries]
  (let [store-names (into #{} (map (comp name val)) entries)]
    (io/batch-tx idb store-names
      (fn [tx]
        (doseq [[id kind] entries]
          (.delete (.objectStore tx (name kind)) id)))
      nil)))

;endregion

;region Reading Dirty Entities

(defn dirty-entities [idb]
  (-> (io/read-dirty-set idb)
      (.then (fn [dirty-entries]
               (if (empty? dirty-entries)
                 (js/Promise.resolve [])
                 (read-by-entries idb dirty-entries))))))

;endregion

;region Clearing Dirty Entities

(defn- clear-entries! [idb id-set dirty-entries]
  (let [to-delete (select-keys dirty-entries id-set)
        remaining (apply dissoc dirty-entries id-set)]
    (-> (io/write-dirty-set! idb remaining)
        (.then (fn [_] (when (seq to-delete)
                          (delete-by-entries idb to-delete)))))))

(defn clear-dirty! [idb ids-to-clear]
  (let [id-set (set ids-to-clear)]
    (-> (io/read-dirty-set idb)
        (.then (partial clear-entries! idb id-set)))))

;endregion
