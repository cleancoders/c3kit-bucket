(ns c3kit.bucket.memory
  (:require
    [c3kit.apron.legend :as legend]
    [c3kit.apron.schema :as schema]
    [c3kit.apron.utilc :as utilc]
    [c3kit.bucket.api :as api]
    [clojure.string :as str]))

(def ^:private id-source (atom 1000))
(defn- gen-id [] (swap! id-source inc))

(defn ensure-id [e]
  (if (:id e)
    e
    (assoc e :id (gen-id))))

(defn- merge-with-original [original updated]
  (if original
    (let [retracted-keys (doall (filter #(= nil (get updated %)) (keys original)))
          merged         (merge original updated)]
      (apply dissoc merged retracted-keys))
    updated))

(defn- retract-entity [store entity]
  (let [id   (:id entity)
        kind (get-in store [:all id :kind])]
    (-> store
        (update :all dissoc id)
        (update kind dissoc id))))

(defn- install-entity [legend store e]
  (assert (:id e) (str "entity missing id!: " e))
  (let [original (get-in store [:all (:id e)])
        schema   (legend/for-kind @legend (:kind e))
        e        (->> (utilc/keywordize-kind e)
                      ensure-id
                      (merge-with-original original)
                      (api/-update-timestamps schema)
                      (schema/coerce! schema))]
    (-> store
        (update :all assoc (:id e) e)
        (update (:kind e) assoc (:id e) e))))

(defn- tx-entity [legend store e]
  (if (api/delete? e)
    (retract-entity store e)
    (install-entity legend store e)))

;(defn- multi? [ev] (or (sequential? ev) (set? ev)))

(defn pattern-comparator [v case-sensitive?]
  (let [pattern (-> (str/replace v "%" ".*") (str/replace "_" ".") re-pattern)]
    (fn [ev]
      (when ev
        (let [ev (if case-sensitive? ev (str/upper-case ev))]
          (boolean (re-matches pattern ev)))))))

(defn normal-tester [f v]
  (fn [ev]
    (and (some? ev)
         (f ev v))))

(defmulti -tester first)
(defmethod -tester '> [[_ v]] (if (number? v) (normal-tester > v) (normal-tester #(pos? (compare %1 %2)) v)))
(defmethod -tester '< [[_ v]] (if (number? v) (normal-tester < v) (normal-tester #(neg? (compare %1 %2)) v)))
(defmethod -tester '>= [[_ v]] (if (number? v) (normal-tester >= v) (normal-tester #(>= (compare %1 %2) 0) v)))
(defmethod -tester '<= [[_ v]] (if (number? v) (normal-tester <= v) (normal-tester #(<= (compare %1 %2) 0) v)))
(defmethod -tester 'like [[_ v]] (pattern-comparator v true))
(defmethod -tester 'ilike [[_ v]] (pattern-comparator (str/upper-case v) false))

(defn- -vec-tester [par values]
  (let [v-set (set values)]
    (fn [ev]
      (par #(= % ev) v-set))))
(def -or-tester (partial -vec-tester some))
(def -nor-tester (partial -vec-tester not-any?))

(defmethod -tester 'not= [[_ & values]] (-nor-tester values))
(defmethod -tester '= [[_ & values]] (-or-tester values))
(defmethod -tester :default [values] (-or-tester values))

(def eq-tester (partial normal-tester =))

(defn ensure-key [k]
  (if (set? k)
    (map ensure-key k)
    (->> [(namespace k) (name k)]
         (remove nil?)
         (map keyword)
         vec)))

(defn get-tester-by-type [v]
  (cond (set? v) (-or-tester v)
        (sequential? v) (-tester v)
        (nil? v) nil?
        :else (eq-tester v)))

(defn single-column-tester [k v]
  (let [tester (get-tester-by-type v)]
    (fn [e]
      (let [attr (ensure-key k)
            ev   (get-in e attr)]
        (tester ev)))))

(defn multi-column-tester [k v]
  (fn [e] (some #(% e) (map #(single-column-tester % v) k))))

(defn kv->tester [[k v]]
  (if (set? k)
    (multi-column-tester k v)
    (single-column-tester k v)))

(defn- spec->tester [spec]
  (let [testers (map kv->tester spec)]
    (fn [e] (every? #(% e) testers))))

(defn ensure-schema! [legend kind] (legend/for-kind @legend kind))

;; db api -----------------------------------

(defn- entity [db kind id]
  (cond
    (nil? id) nil
    (map? id) (get-in @(.-store db) [kind (api/coerced-id (.-legend db) kind (:id id))])
    :else (get-in @(.-store db) [kind (api/coerced-id (.-legend db) kind id)])))

(defn- clear [store] (reset! store {:all {}}))
(defn- count-all [kind] (count (api/find-all kind)))

(defn- reload [db e] (when-let [id (:id e)] (entity db (:kind e) id)))

(defn- tx-result [db entity]
  (if (api/delete? entity)
    (api/soft-delete entity)
    (reload db entity)))

(defn- tx [db e]
  (let [e (ensure-id e)]
    (swap! (.-store db) #(tx-entity (.-legend db) % e))
    (tx-result db e)))

(defn- tx* [db entities]
  (let [entities (map ensure-id entities)]
    (swap! (.-store db) (fn [store] (reduce #(tx-entity (.-legend db) %1 %2) store entities)))
    (map #(tx-result db %) entities)))

(defn- find-all [db kind]
  (ensure-schema! (.-legend db) kind)
  (or (vals (get-in @(.-store db) [kind])) []))

(defn every-keyword? [ks]
  (every? (fn [k] (or (keyword? k) (every? keyword? k))) ks))

(defn- filter-by-kvs [kvs kinds]
  (let [kv-pairs (partition 2 kvs)]
    (assert (every-keyword? (map first kv-pairs)) "Attributes must be keywords")
    (filter (spec->tester kv-pairs) kinds)))

(defn- find-by [db kind kvs]
  (ensure-schema! (.-legend db) kind)
  (let [kinds (vals (get @(.-store db) kind))]
    (filter-by-kvs kvs kinds)))

(defn- ffind-by [db kind & kvs]
  (first (apply find-by db kind kvs)))

(defn- delete-all [db kind]
  (let [all-ids (keys (get @(.-store db) kind))]
    (swap! (.-store db) (fn [db]
                          (-> (update db [:all] #(apply dissoc % all-ids))
                              (dissoc kind))))))

(deftype MemoryDB [legend store]
  api/DB
  (-install-schema [_ schemas] (swap! legend merge (legend/build schemas)))
  (-clear [_] (clear store))
  (-count-all [_ kind] (count-all kind))
  (-count-by [this kind kvs] (count (find-by this kind kvs)))
  (-delete-all [this kind] (delete-all this kind))
  (-entity [this kind id] (entity this kind id))
  (-find-all [this kind] (find-all this kind))
  (-find-by [this kind kvs] (find-by this kind kvs))
  (-ffind-by [this kind kvs] (ffind-by this kind kvs))
  (-reduce-by [this kind f val kvs] (reduce f val (find-by this kind kvs)))
  (-tx [this entity] (tx this entity))
  (-tx* [this entities] (tx* this entities))
  )

(defn create-db
  ([] (create-db nil))
  ([legend] (MemoryDB. (atom legend) (atom {}))))
