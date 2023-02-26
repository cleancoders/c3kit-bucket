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

(defn- retract-entity [db entity]
  (let [id   (:id entity)
        kind (get-in db [:all id :kind])]
    (-> db
        (update :all dissoc id)
        (update kind dissoc id))))

(defn- install-entity [db e]
  (assert (:id e) (str "entity missing id!: " e))
  (let [original (get-in db [:all (:id e)])
        schema   (legend/for-kind (:kind e))
        e        (->> (utilc/keywordize-kind e)
                      ensure-id
                      (merge-with-original original)
                      (api/-update-timestamps schema)
                      (schema/coerce! schema))]
    (-> db
        (update :all assoc (:id e) e)
        (update (:kind e) assoc (:id e) e))))

(defn- tx-entity [db e]
  (if (api/delete? e)
    (retract-entity db e)
    (install-entity db e)))

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

(defn ensure-schema! [kind] (legend/for-kind kind))

;; db api -----------------------------------

(defn- entity [store kind id]
  (cond
    (nil? id) nil
    (map? id) (get-in @store [kind (api/coerced-id kind (:id id))])
    :else (get-in @store [kind (api/coerced-id kind id)])))

(defn- skind [schema] (-> schema :kind :value))

(defn- do-clear [store] (reset! store {:all {}}))
(defn- do-count [kind] (count (api/find-all kind)))

(defn- reload [store e] (when-let [id (:id e)] (entity store (:kind e) id)))

(defn- tx-result [store entity]
  (if (api/delete? entity)
    {:kind (:kind entity) :id (:id entity) :db/delete? true}
    (reload store entity)))

(defn- do-tx [store e]
  (let [e (ensure-id e)]
    (swap! store tx-entity e)
    (tx-result store e)))

(defn- do-tx* [store entities]
  (let [entities (map ensure-id entities)]
    (swap! store (fn [db] (reduce #(tx-entity %1 %2) db entities)))
    (map #(tx-result store %) entities)))

(defn- do-find-all [store kind]
  (ensure-schema! kind)
  (or (vals (get-in @store [kind])) []))

(defn every-keyword? [ks]
  (every? (fn [k] (or (keyword? k) (every? keyword? k))) ks))

(defn- filter-by-kvs [kvs kinds]
  (let [kv-pairs (partition 2 kvs)]
    (assert (every-keyword? (map first kv-pairs)) "Attributes must be keywords")
    (filter (spec->tester kv-pairs) kinds)))

(defn- do-find-by [store kind & [kvs]]
  (ensure-schema! kind)
  (let [kinds (vals (get @store kind))]
    (filter-by-kvs kvs kinds)))

(defn- do-ffind-by [store kind & kvs]
  (first (apply do-find-by store kind kvs)))

(defn- do-delete-all [store kind]
  (let [all-ids (keys (get @store kind))]
    (swap! store (fn [db]
                   (-> (update db [:all] #(apply dissoc % all-ids))
                       (dissoc kind))))))

(deftype MemoryDB [store]
  api/DB
  (-clear [_] (do-clear store))
  (-count-all [_ kind] (do-count kind))
  (-count-by [_ kind kvs] (count (do-find-by store kind kvs)))
  (-delete-all [_ kind] (do-delete-all store kind))
  (-entity [_ kind id] (entity store kind id))
  (-find-all [_ kind] (do-find-all store kind))
  (-find-by [_ kind kvs] (do-find-by store kind kvs))
  (-ffind-by [_ kind kvs] (do-ffind-by store kind kvs))
  (-reduce-by [_ kind f val kvs] (reduce f val (do-find-by store kind kvs)))
  (-tx [_ entity] (do-tx store entity))
  (-tx* [_ entities] (do-tx* store entities))
  )

(defn create-db
  ([] (MemoryDB. (atom {})))
  ([atm] (MemoryDB. atm)))
