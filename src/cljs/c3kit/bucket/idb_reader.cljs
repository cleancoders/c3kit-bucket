(ns c3kit.bucket.idb-reader
  (:require [c3kit.bucket.idb-io :as io]))

;region Reading Dirty Entities

(defn dirty-entities [idb]
  (-> (io/read-dirty-set idb)
      (.then (fn [dirty-ids]
               (if (empty? dirty-ids)
                 (js/Promise.resolve [])
                 (-> (io/read-all-entities idb)
                     (.then (fn [all-entities]
                              (vec (filter #(contains? dirty-ids (:id %)) all-entities))))))))))

;endregion

;region Clearing Dirty Entities

(defn- delete-entities-by-ids [idb ids]
  (-> (io/read-all-entities idb)
      (.then (fn [all-entities]
               (let [to-delete (filter #(contains? ids (:id %)) all-entities)]
                 (if (empty? to-delete)
                   (js/Promise.resolve nil)
                   (let [store-names (into #{} (map #(name (:kind %))) to-delete)]
                     (io/batch-tx idb store-names
                                      (fn [tx]
                                        (doseq [entity to-delete]
                                          (.delete (.objectStore tx (name (:kind entity))) (:id entity))))
                                      nil))))))))

(defn clear-dirty! [idb ids-to-clear]
  (-> (io/read-dirty-set idb)
      (.then (fn [current]
               (io/write-dirty-set! idb (reduce disj current ids-to-clear))))
      (.then (fn [_]
               (delete-entities-by-ids idb ids-to-clear)))))

;endregion
