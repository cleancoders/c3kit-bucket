(ns c3kit.bucket.memory
  (:refer-clojure :rename {find core-file count core-count reduce core-reduce})
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.apron.log :as log]
            [c3kit.apron.schema :as schema]
            [c3kit.apron.utilc :as utilc]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.migrator :as migrator]))

(def ^:private id-source (atom 1000))
(defn- gen-id [] (swap! id-source inc))

;; Vector distance functions for similarity search
(defn- dot-product [v1 v2]
  (core-reduce + (map * v1 v2)))

(defn- magnitude [v]
  #?(:clj  (Math/sqrt (dot-product v v))
     :cljs (js/Math.sqrt (dot-product v v))))

(defn- cosine-distance
  "Returns cosine distance (1 - cosine similarity). Range: 0 (identical) to 2 (opposite)."
  [v1 v2]
  (let [dot (dot-product v1 v2)
        mag1 (magnitude v1)
        mag2 (magnitude v2)]
    (if (or (zero? mag1) (zero? mag2))
      1.0
      (- 1.0 (/ dot (* mag1 mag2))))))

(defn- l2-distance
  "Returns Euclidean (L2) distance between two vectors."
  [v1 v2]
  #?(:clj  (Math/sqrt (core-reduce + (map #(Math/pow (- %1 %2) 2) v1 v2)))
     :cljs (js/Math.sqrt (core-reduce + (map #(js/Math.pow (- %1 %2) 2) v1 v2)))))

(defn- inner-product-distance
  "Returns negative inner product (for ORDER BY ASC semantics - higher similarity = lower value)."
  [v1 v2]
  (- (dot-product v1 v2)))

(defn- safe-distance
  "Compute distance, returning MAX_VALUE if either vector is nil."
  [distance-fn v1 v2]
  (if (or (nil? v1) (nil? v2))
    #?(:clj  Double/MAX_VALUE
       :cljs js/Number.MAX_VALUE)
    (distance-fn v1 v2)))

(defn ensure-id [e]
  (if (:id e)
    e
    (assoc e :id (gen-id))))

(defn- merge-with-original [original updated]
  (if original
    (let [retracted-keys (doall (filter #(nil? (get updated %)) (keys original)))
          merged         (merge original updated)]
      (apply dissoc merged retracted-keys))
    updated))

(defn- retract-entity [store entity]
  (let [id   (:id entity)
        kind (get-in store [:all id :kind])]
    (-> store
        (update :all dissoc id)
        (update kind dissoc id))))

(defn check-cas! [entity original]
  (when-let [cas (api/-get-cas entity)]
    (api/-check-cas! cas entity original)))

(defn- install-entity [legend store e]
  (assert (:id e) (str "entity missing id!: " e))
  (let [original (get-in store [:all (:id e)])
        schema   (legend/for-kind legend (:kind e))
        _        (check-cas! e original)
        e        (->> (utilc/keywordize-kind e)
                      ensure-id
                      (merge-with-original original)
                      (schema/coerce! schema))]
    (-> store
        (update :all assoc (:id e) e)
        (update (:kind e) assoc (:id e) e))))

(defn tx-entity [legend store e]
  (if (api/delete? e)
    (retract-entity store e)
    (install-entity legend store e)))

(defn- ensure-key [k]
  (if (set? k)
    (map ensure-key k)
    (->> [(namespace k) (name k)]
         (remove nil?)
         (map keyword)
         vec)))

(defn- ensure-schema! [legend kind] (legend/for-kind legend kind))

(def ^:private vector-operators #{'<-> '<=> '<#>})

(defn- apply-order-by
  "Apply ordering to a collection of entities.
   order-by is a map of {field direction-or-op} where direction-or-op can be:
   - :asc or :desc for standard ordering
   - ['<-> query-vec] for L2 distance
   - ['<=> query-vec] for cosine distance
   - ['<#> query-vec] for inner product"
  [order-by entities]
  (if-not (and order-by (map? order-by) (seq order-by))
    entities
    (let [entry (first order-by)
          field (key entry)
          direction-or-op (val entry)]
      (cond
        ;; Vector similarity ordering
        (and (sequential? direction-or-op)
             (vector-operators (first direction-or-op)))
        (let [[op query-vec] direction-or-op
              distance-fn (case op
                            <-> l2-distance
                            <=> cosine-distance
                            <#> inner-product-distance)]
          (sort-by #(safe-distance distance-fn (get % field) query-vec) entities))

        ;; Standard descending
        (= :desc direction-or-op)
        (sort-by field #(compare %2 %1) entities)

        ;; Standard ascending (default)
        :else
        (sort-by field entities)))))

(defn- do-find [db kind options]
  (ensure-schema! @(.-legend db) kind)
  (let [es (or (vals (get @(.-store db) kind)) [])]
    (->> (ccc/find-by es (:where options))
         (apply-order-by (:order-by options))
         (api/-apply-drop-take options))))

;; db api -----------------------------------

(defn entity
  "kind (optional) in addition to returning nil when kinds don't match,
  will allow coersion of the id to the right id type for the kind."
  ([db id]
   (cond
     (nil? id) nil
     (map? id) (get-in @(.-store db) [:all (:id id)])
     :else (get-in @(.-store db) [:all id])))
  ([db kind id]
   (if (nil? kind)
     (entity db id)
     (cond
       (nil? id) nil
       (map? id) (get-in @(.-store db) [kind (api/-coerced-id @(.-legend db) kind (:id id))])
       :else (get-in @(.-store db) [kind (api/-coerced-id @(.-legend db) kind id)])))))

(defn clear [db]
  (api/-assert-safety-off! "clear")
  (reset! (.-store db) {:all {}}))

(defn reload [db e] (when-let [id (:id e)] (entity db (:kind e) id)))

(defn tx-result [db entity]
  (if (api/delete? entity)
    (api/soft-delete entity)
    (reload db entity)))

(defn tx [db e]
  (let [e (ensure-id e)]
    (swap! (.-store db) #(tx-entity @(.-legend db) % e))
    (tx-result db e)))

(defn tx* [db entities]
  (let [entities (map ensure-id entities)]
    (swap! (.-store db) (fn [store] (core-reduce #(tx-entity @(.-legend db) %1 %2) store entities)))
    (map #(tx-result db %) entities)))

(defn delete-all [db kind]
  (api/-assert-safety-off! "delete-all")
  (let [all-ids (keys (get @(.-store db) kind))]
    (swap! (.-store db) (fn [db]
                          (-> (update db [:all] #(apply dissoc % all-ids))
                              (dissoc kind))))))

(defn- do-install-schema! [db schema]
  (let [schema (api/-normalize-schema schema)
        kind   (api/-schema-kind schema)]
    (swap! (.-legend db) assoc kind schema)))

(defn do-remove-attribute! [db kind attr]
  (if (some? (get @(.-legend db) kind))
    (do (tx* db (map #(dissoc % attr) (do-find db kind [])))
        (swap! (.-legend db) update kind dissoc attr))
    (log/warn "  remove MISSING " (keyword (name kind) (name attr)))))

(defn do-rename-attribute! [db kind attr new-kind new-attr]
  (when-not (= kind new-kind)
    (throw (ex-info "cannot rename attribute kind" {:kind kind :attr attr :new-kind new-kind :new-attr new-attr})))
  (let [spec        (get-in @(.-legend db) [kind attr])
        new-exists? (some? (get-in @(.-legend db) [kind new-attr]))]
    (cond new-exists? (throw (ex-info "rename to existing attribute" {:kind kind :old attr :new new-attr}))
          (some? spec) (do (swap! (.-legend db) #(-> (update % kind dissoc attr) (assoc-in [kind new-attr] spec)))
                           (tx* db (ccc/map-some (fn [e]
                                                   (when-let [v (get e attr)]
                                                     (-> (dissoc e attr)
                                                         (assoc new-attr v))))
                                                 (do-find db kind []))))
          :else (log/warn "  rename FAILED: MISSING " kind attr))))

(deftype MemoryDB [legend store]
  api/DB
  (-clear [this] (clear this))
  (close [_this] (comment "Nothing to do here"))
  (-count [this kind options] (core-count (do-find this kind options)))
  (-delete-all [this kind] (delete-all this kind))
  (-entity [this kind id] (entity this kind id))
  (-find [this kind options] (do-find this kind options))
  (-reduce [this kind f init options] (core-reduce f init (do-find this kind options)))
  (-tx [this entity] (tx this entity))
  (-tx* [this entities] (tx* this entities))
  migrator/Migrator
  (-schema-exists? [_this schema] (contains? @legend (api/-schema-kind schema)))
  (-installed-schema-legend [_this _expected-legend] @legend)
  (-install-schema! [this schema] (do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [_this kind attr spec] (swap! legend assoc-in [kind attr] spec))
  (-remove-attribute! [this kind attr] (do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (do-rename-attribute! this kind attr new-kind new-attr)))

(defmethod api/-create-impl :memory [config schemas]
  (let [store (or (:store config) (atom {}))]
    (MemoryDB. (atom (legend/build schemas)) store)))
