(ns c3kit.bucket.re-memory
  (:refer-clojure :rename {find core-file count core-count reduce core-reduce})
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.memory :refer [MemoryDB] :as memory]
            [reagent.core :as r]))

;; db api -----------------------------------
(defn entity
  "kind (optional) in addition to returning nil when kinds don't match,
  will allow coercion of the id to the right id type for the kind."
  ([db id]
   (cond
     (nil? id) nil
     (map? id) @(r/cursor (.-store db) [:all (:id id)])
     :else @(r/cursor (.-store db) [:all id])))
  ([db kind id]
   (if (nil? kind)
     (entity db id)
     (cond
       (nil? id) nil
       (map? id) @(r/cursor (.-store db) [kind (api/-coerced-id @(.-legend db) kind (:id id))])
       :else @(r/cursor (.-store db) [kind (api/-coerced-id @(.-legend db) kind id)])))))

(defn- do-find [db kind options]
  (legend/for-kind @(.-legend db) kind)
  (let [es (or (vals @(r/cursor (.-store db) [kind])) [])]
    (->> (ccc/find-by es (:where options))
         (api/-apply-drop-take options))))

(deftype ReMemoryDB [legend store]
  api/DB
  (-clear [this] (memory/clear this))
  (close [_this] (comment "Nothing to do here"))
  (-count [this kind options] (core-count (do-find this kind options)))
  (-delete-all [this kind] (memory/delete-all this kind))
  (-entity [this kind id] (entity this kind id))
  (-find [this kind options] (do-find this kind options))
  (-reduce [this kind f init options] (core-reduce f init (do-find this kind options)))
  (-tx [this entity] (memory/tx this entity))
  (-tx* [this entities] (memory/tx* this entities)))

(defmethod api/-create-impl :re-memory [config schemas]
  (let [store (or (:store config) (r/atom {}))]
    (ReMemoryDB. (atom (legend/build schemas)) store)))

(defn- slice-db [query]
  (let [slice (get @(.-store @api/impl) (first query))
        vals  (vals slice)
        keys  (last query)]
    (map #(select-keys % keys) vals)))

(defn select-find-by
  "Like rememory/find-by, but takes a keyseq as the second argument, similar to clojure.core/select-keys.
  Used for selecting attributes beyond the attributes used in the search query. Components will re-render if the
  selected or queried attributes change but will ignore changes to other attributes.
  Only returns the selected and queried attributes of the entity."
  ([kind keyseq & kvs]
   (let [kvs        (cond-> kvs (coll? (first kvs)) first)
         options    (api/-kvs->kv-pairs kvs)
         kvs-as-map (ccc/->options kvs)
         kvs-keys   (keys kvs-as-map)
         cursor     (r/cursor slice-db [kind (set (concat keyseq kvs-keys [:id :kind]))])]
     (->> (ccc/find-by @cursor kvs-as-map)
          (api/-apply-drop-take options)))))

(defn find-by
  "Components will only re-render if the attributes that were queried by change.
  Only returns the queried attributes of the entity, in addtition to :kind and :id"
  ([kind & kvs]
   (select-find-by kind [] kvs)))
