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

; TODO [ARR] - Cache cursors to ensure we aren't creating new cursors with every lookup
(defn- do-find [db kind options]
  (memory/ensure-schema! @(.-legend db) kind)
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


; ---- thoughts ----
;
; possible for db/ffind to only cause state changes when that one entity changes? Maybe not.
; it seems like the kind is as specific as we can get with do-find
;
; do-find creates a new cursor everytime. Is there a way we could create cursors per kind ahead of time and use those?
;
; db/find-in will create a reaction, which needs to be in a with-let or form-2 or form-3 components. can we log a
; warning if it's being used outside of that context?
;
(comment
 (def db (r/atom {}))
 ; magic sauce
 (defn test [query]
   (let [slice (get @db (first query))
         vals (vals slice)
         keys (last query)]
     (map #(select-keys val keys) vals)))
[:id :id]
 {:all {1 {}
        2 {}}
  :story {1 {:id 1}
          2 {:id 2}}
  }

 (defn find-in
   ([k & kvs] (apply find-in k [] kvs))
   ([k v & kvs]
    (let [options (api/-kvs->kv-pairs kvs)
          kvs-keys (keys options)
          cursor (r/cursor test [k (concat v kvs-keys [:id :kind])])]
      (->> (ccc/find-by @cursor {:where options})
           (api/-apply-drop-take options)))))

 (find-in :story [:foo :bar])
 (find-in :story [:title] :foo 123 :bar 543)
 (find-in :story :foo 123 :bar 543)
 (r/cursor test [:story [:foo :bar]])

 {:kind :story
  :id 123
  :title "blah 2"
  :estimate 3.0
  })

{:where {:story 1234 :title "blah"}}