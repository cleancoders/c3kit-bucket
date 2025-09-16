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
(defn- ids-not-fns? [id] (or (int? id) (int? (first id))))

(defn- slice-db [[kind-or-ids keyseq]]
  (let [slice (if (and (not (keyword? kind-or-ids)) (ids-not-fns? kind-or-ids))
                (slice-by-ids kind-or-ids)
                (slice-by-kind kind-or-ids))]
    (map #(select-keys % keyseq) (vals slice))))

(defn- do-find [db kind options]
  (let [where-map (ccc/->options (apply concat (:where options)))
        id        (:id where-map)
        cursor    (if (ids-not-fns? id)
                    (r/cursor slice-by-ids (cond-> id (int? id) vector))
                    (r/cursor (.-store db) [kind]))]
    (legend/for-kind @(.-legend db) kind)
    (let [es (or (vals @cursor) [])]
      (->> (ccc/find-by es (conj (:where options) [:kind kind]))
           (api/-apply-drop-take options)))))

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

(defn- ->keyseq [& colls]
  (set (conj (apply concat colls) :id :kind)))

(defn ->kind-or-ids [kvs-as-map kind]
  (let [id (:id kvs-as-map)] (if id (cond-> id (int? id) vector) kind)))

(defn do-select-find [kind keyseq kvs]
  (let [kvs-as-map (assoc (ccc/->options kvs) :kind kind)
        kvs-keys   (keys kvs-as-map)
        cursor     (r/cursor slice-db [(->kind-or-ids kvs-as-map kind) (->keyseq keyseq kvs-keys)])]
    (ccc/find-by @cursor kvs-as-map)))

(defn select-find-by
  "Like find-by, but components will only re-render if the selected or queried attributes change
  and ignore changes to other attributes.
  Only returns the selected and queried attributes of the entity.
  If the second argument is a keyseq, the component will also listen to changes to those attributes
  and re-render acccordingly.

  `(select-find-by :thingy :id 12354 :foo 5678)`
  `(select-find-by :thingy [:foo :bar] :id 1234)`"
  ([kind & kvs]
   (let [[keyseq kvs] (if (coll? (first kvs)) [(first kvs) (rest kvs)] [[] kvs])]
     (do-select-find kind keyseq kvs))))

(defn select-ffind-by [kind & kvs]
  (let [[keyseq kvs] (if (coll? (first kvs)) [(first kvs) (rest kvs)] [[] kvs])]
    (first (do-select-find kind keyseq kvs))))

; TODO get commented tests passing
; TODO drop and take
; TODO check for refactor opportunities between do-select-find and do-find