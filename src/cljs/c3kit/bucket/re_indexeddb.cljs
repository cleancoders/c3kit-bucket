(ns c3kit.bucket.re-indexeddb
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.indexeddb :as idb]
            [c3kit.bucket.re-memory :as re-memory]
            [reagent.core :as r]))

(defmethod api/-create-impl :re-indexeddb [config schemas]
  (let [legend    (atom (legend/build schemas))
        store     (or (:store config) (r/atom {}))
        idb-atom  (atom nil)
        db-name   (or (:db-name config) "c3kit-bucket")
        online-fn (or (:online? config) (constantly true))]
    (idb/->IndexedDB legend store idb-atom db-name online-fn re-memory/entity re-memory/do-find)))
