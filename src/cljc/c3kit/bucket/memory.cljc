(ns c3kit.bucket.memory
  (:refer-clojure :rename {find core-file count core-count reduce core-reduce})
  (:require
    [c3kit.apron.legend :as legend]
    [c3kit.apron.schema :as schema]
    [c3kit.apron.utilc :as utilc]
    [c3kit.bucket.api :as api]
    [clojure.string :as str]))

(def ^:private id-source (atom 1000))
(defn- gen-id [] (swap! id-source inc))

(defn- ensure-id [e]
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

(defn- tx-entity [legend store e]
  (if (api/delete? e)
    (retract-entity store e)
    (install-entity legend store e)))

(defn- pattern-comparator [v case-sensitive?]
  (let [pattern (-> (str/replace v "%" ".*") (str/replace "_" ".") re-pattern)]
    (fn [ev]
      (when ev
        (let [ev (if case-sensitive? ev (str/upper-case ev))]
          (boolean (re-matches pattern ev)))))))

(defn- multi? [v] (or (sequential? v) (set? v)))

(defn- -normal-tester [f v]
  (fn [ev]
    (if (multi? ev)
      (some #(f % v) ev)
      (and (some? ev) (f ev v)))))

(defn- -or-tester [values]
  (let [v-set (set values)]
    (fn [ev]
      (if (multi? ev)
        (some #(contains? v-set %) ev)
        (contains? v-set ev)))))

(defn- -nor-tester [values]
  (let [v-set (set values)]
    (fn [ev]
      (if (multi? ev)
        (not-any? #(contains? v-set %) ev)
        (not (contains? v-set ev))))))

(defn -tester [form]
  (condp = (first form)
    '> (let [v (second form)] (if (number? v) (-normal-tester > v) (-normal-tester #(pos? (compare %1 %2)) v)))
    '< (let [v (second form)] (if (number? v) (-normal-tester < v) (-normal-tester #(neg? (compare %1 %2)) v)))
    '>= (let [v (second form)] (if (number? v) (-normal-tester >= v) (-normal-tester #(>= (compare %1 %2) 0) v)))
    '<= (let [v (second form)] (if (number? v) (-normal-tester <= v) (-normal-tester #(<= (compare %1 %2) 0) v)))
    'like (pattern-comparator (second form) true)
    'ilike (pattern-comparator (str/upper-case (second form)) false)
    'not= (-nor-tester (rest form))
    '= (-or-tester (rest form))
    (-or-tester form)))

(def ^:private eq-tester (partial -normal-tester =))

(defn- ensure-key [k]
  (if (set? k)
    (map ensure-key k)
    (->> [(namespace k) (name k)]
         (remove nil?)
         (map keyword)
         vec)))

(defn- get-tester-by-type [v]
  (cond (set? v) (-or-tester v)
        (sequential? v) (-tester v)
        (nil? v) nil?
        :else (eq-tester v)))

(defn- kv->tester [[k v]]
  (let [tester (get-tester-by-type v)]
    (fn [e]
      (let [attr (ensure-key k)
            ev   (get-in e attr)]
        (tester ev)))))

(defn- spec->tester [spec]
  (let [testers (map kv->tester spec)]
    (fn [e] (every? #(% e) testers))))

(defn- ensure-schema! [legend kind] (legend/for-kind legend kind))

(defn- filter-where [{:keys [where]} entities]
  (if (seq where)
    (let [tester (spec->tester where)]
      (filter tester entities))
    entities))

(defn- do-find [db kind options]
  (ensure-schema! (.-legend db) kind)
  (->> (or (vals (get @(.-store db) kind)) [])
       (filter-where options)
       (api/-apply-drop-take options)))

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
       (map? id) (get-in @(.-store db) [kind (api/-coerced-id (.-legend db) kind (:id id))])
       :else (get-in @(.-store db) [kind (api/-coerced-id (.-legend db) kind id)])))))

(defn clear [db]
  (api/-assert-safety-off! "clear")
  (reset! (.-store db) {:all {}}))

(defn reload [db e] (when-let [id (:id e)] (entity db (:kind e) id)))

(defn- tx-result [db entity]
  (if (api/delete? entity)
    (api/soft-delete entity)
    (reload db entity)))

(defn tx [db e]
  (let [e (ensure-id e)]
    (swap! (.-store db) #(tx-entity (.-legend db) % e))
    (tx-result db e)))

(defn tx* [db entities]
  (let [entities (map ensure-id entities)]
    (swap! (.-store db) (fn [store] (core-reduce #(tx-entity (.-legend db) %1 %2) store entities)))
    (map #(tx-result db %) entities)))

(defn delete-all [db kind]
  (api/-assert-safety-off! "delete-all")
  (let [all-ids (keys (get @(.-store db) kind))]
    (swap! (.-store db) (fn [db]
                          (-> (update db [:all] #(apply dissoc % all-ids))
                              (dissoc kind))))))

(deftype MemoryDB [legend store]
  api/DB
  (-clear [this] (clear this))
  (-count [this kind options] (core-count (do-find this kind options)))
  (-delete-all [this kind] (delete-all this kind))
  (-entity [this kind id] (entity this kind id))
  (-find [this kind options] (do-find this kind options))
  (-reduce [this kind f init options] (core-reduce f init (do-find this kind options)))
  (-tx [this entity] (tx this entity))
  (-tx* [this entities] (tx* this entities))
  )

(defmethod api/-create-impl :memory [config schemas]
  (let [store (or (:store config) (atom {}))]
    (MemoryDB. (legend/build schemas) store)))
