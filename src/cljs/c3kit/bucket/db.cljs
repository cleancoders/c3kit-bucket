(ns c3kit.bucket.db
  "A Simple in-memory database.
   Data pulled from the server is stored here to be easily retrieved some time later."
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.apron.log :as log]
            [c3kit.apron.utilc :as utilc]
            [c3kit.bucket.dbc :as dbc]))

(def ^:private db (atom {:all {}}))
(defn replace-db-atom! [new-atom]
  (reset! new-atom @db)
  (set! db new-atom))

(def ^:private id-source (atom 1000))

(defn- gen-id [] (swap! id-source inc))
(defn- ensure-id [e] (if (:id e) e (assoc e :id (gen-id))))
(defn clear! [] (reset! db {:all {}}))

(defn- merge-with-original [updated original]
  (if original
    (let [retracted-keys (doall (filter #(= nil (get updated %)) (keys original)))
          merged         (merge original updated)]
      (apply dissoc merged retracted-keys))
    updated))

(defn- retract-entity [db entity]
  (let [id   (:id entity)
        kind (get-in db [:all id :kind])]
    (-> db
        (update :all dissoc id)
        (update kind dissoc id))))

(def seq-or-value? (complement (every-pred coll? empty?)))

(defn- remove-empty-seqs [e]
  (reduce-kv (fn [m k v] (cond-> m (seq-or-value? v) (assoc k v))) {} e))

(defn- install-entity [db e]
  (assert (:id e) (str "entity missing id!: " e))
  (let [original (get-in db [:all (:id e)])
        e        (-> (utilc/keywordize-kind e)
                     (merge-with-original original)
                     legend/coerce!
                     remove-empty-seqs)]
    (-> db
        (update :all assoc (:id e) e)
        (update (:kind e) assoc (:id e) e))))

(defn- tx-entity [db e]
  (if (dbc/retract? e)
    (retract-entity db e)
    (install-entity db e)))

(def ^:private multi? (some-fn sequential? set?))

(defn- normal-tester [f v]
  (fn [ev]
    (if (multi? ev)
      (some #(f % v) ev)
      (f ev v))))

(defn- or-tester [values]
  (let [v-set (set values)]
    (fn [ev]
      (if (multi? ev)
        (some v-set ev)
        (v-set ev)))))

(defmulti -tester first)
(defmethod -tester 'not [[_ v]]
  (fn [ev]
    (if (multi? ev)
      (not-any? #(= v %) ev)
      (not= v ev))))
(defmethod -tester '> [[_ v]] (normal-tester > v))
(defmethod -tester '< [[_ v]] (normal-tester < v))
(defmethod -tester '>= [[_ v]] (normal-tester >= v))
(defmethod -tester '<= [[_ v]] (normal-tester <= v))
(defmethod -tester 'or [values] (or-tester (rest values)))
(defmethod -tester :default [values] (or-tester values))

(defn- eq-tester [v] (normal-tester = v))

(defn kv->tester [[k v]]
  (let [tester (cond (set? v) (or-tester v)
                     (sequential? v) (-tester v)
                     :else (eq-tester v))]
    (fn [e] (tester (get e k)))))

(defn- spec->tester [spec]
  (let [testers (map kv->tester spec)]
    (fn [e] (every? #(% e) testers))))

;; db api -----------------------------------

(defn squuid [] (random-uuid))

(defn tempid [] (gen-id))

(defn entity [id]
  (cond
    (nil? id) nil
    (map? id) (get-in @db [:all (:id id)])
    :else (get-in @db [:all id])))

(defn entity! [id] (dbc/entity! (entity id) id))
(defn entity-of-kind! [kind id] (dbc/entity-of-kind! (entity id) kind id))
(defn entity-of-kind [kind id] (dbc/entity-of-kind (entity id) kind))
(defn reload [e] (some-> e :id entity))

(defn tx-result [entity]
  (if (dbc/retract? entity)
    (dbc/soft-retract (:id entity))
    (reload entity)))

(defn tx [& args]
  (let [e (ccc/->options args)]
    (when (seq e)
      (let [e (ensure-id e)]
        (swap! db tx-entity e)
        (tx-result e)))))

(defn tx* [entities]
  (let [entities (ccc/some-map ensure-id entities)]
    (swap! db (fn [db] (reduce tx-entity db entities)))
    (map tx-result entities)))

(defn count-all
  ([kind attr] (count-all kind)) ;; MDM - compatibility with datomic db
  ([kind] (count (get @db kind))))

(defn find-all
  ([kind attr] (find-all kind)) ;; MDM - compatibility with datomic db
  ([kind] (vals (get @db kind))))

(defn- find-by-tester [kvs]
  (let [kv-pairs (partition 2 kvs)]
    (assert (every? keyword? (map first kv-pairs)) "Attributes must be keywords")
    (spec->tester kv-pairs)))

(defn find-by [kind & kvs] (filter (find-by-tester kvs) (find-all kind)))
(defn ffind-by [kind & kvs] (ccc/ffilter (find-by-tester kvs) (find-all kind)))

(defn find-ids-by [kind & kvs]
  (sequence (comp (filter (find-by-tester kvs)) (map :id)) (find-all kind)))

(defn count-by [kind & kvs]
  (let [tester (find-by-tester kvs)]
    (reduce #(if (tester %2) (inc %1) %1) 0 (find-all kind))))

(defn retract [id-or-entity]
  (or (some-> id-or-entity entity dbc/soft-retract tx)
      (log/warn "Attempt to retract missing entity: " id-or-entity)))
