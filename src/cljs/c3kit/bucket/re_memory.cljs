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
  will allow coercion of the id to the right id type for the kind.
  Components will only re-render if this entity changes and will ignore all other changes in the db."
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


(defn- slice-by-kind ([kind] (get @(.-store @api/impl) kind)))
(defn- slice-by-ids [ids] (select-keys (get @(.-store @api/impl) :all) ids))
(defn- ids-not-fns? [id] (or (int? id) (and (coll? id) (int? (first (remove nil? id))))))

(defn- slice-db [[kind-or-ids keyseq]]
  (let [slice (if (ids-not-fns? kind-or-ids)
                (slice-by-ids kind-or-ids)
                (slice-by-kind kind-or-ids))]
    (if (seq keyseq)
      (map #(select-keys % keyseq) (vals slice))
      (vals slice))))

(defn- ->keyseq [& colls] (set (conj (apply concat colls) :id :kind)))
(defn ->kind-or-ids [kvs kind]
  (let [id (second (first (take-while #(= :id (first %)) kvs)))]
    (if (some-> id ids-not-fns?) (cond-> id (int? id) vector) kind)))

(defn- really-do-find [cursor options kind]
  (->> (ccc/find-by @cursor (conj (:where options) [:kind kind]))
       (api/-apply-drop-take options)))

(defn- do-find [db kind options]
  (let [cursor (r/cursor slice-db [(->kind-or-ids (:where options) kind) []])]
    (legend/for-kind @(.-legend db) kind)
    (really-do-find cursor options kind)))

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

(defn do-select-find [kind keyseq options]
  (let [where  (:where options)
        cursor (r/cursor slice-db [(->kind-or-ids where kind) (->keyseq keyseq (map first where))])]
    (legend/for-kind @(.-legend @api/impl) kind)
    (really-do-find cursor options kind)))

(defn- coll-not-map? [thing] (or (seq? thing) (vector? thing)))
(defn ->keyseq-and-options [kvs]
  (if (coll-not-map? (first kvs)) [(first kvs) (rest kvs)] [[] kvs]))

(defn select-find [kind & opt-args]
  (let [[keyseq options] (->keyseq-and-options opt-args)]
    (do-select-find kind keyseq (first options))))

(defn select-find-by
  "Like find-by, but components will only re-render if the selected or queried attributes change
  and ignore changes to other attributes.
  Only returns the selected and queried attributes of the entity. Always includes kind and id.
  If the second argument is a keyseq, the component will also listen to changes to those attributes
  and re-render accordingly.
  Supports all filters options as db/find (['>], ['not=], etc.).

  `(select-find-by :thingy :id 12354 :foo 5678)`
  `(select-find-by :thingy [:foo :bar] :id 1234)`"
  ([kind & kvs]
   (let [[keyseq options] (->keyseq-and-options kvs)]
     (do-select-find kind keyseq {:where (api/-kvs->kv-pairs options)}))))

(defn select-ffind-by [kind & kvs]
  (let [[keyseq options] (->keyseq-and-options kvs)]
    (first (do-select-find kind keyseq {:where (api/-kvs->kv-pairs options)}))))

; TODO select-count-by