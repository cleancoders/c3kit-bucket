(ns c3kit.bucket.datomic
  (:refer-clojure :rename {find core-file count core-count reduce core-reduce})
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.apron.log :as log]
            [c3kit.bucket.api]
            [c3kit.bucket.api :as api]
            [clojure.set :as set]
            [clojure.string :as str]
            [datomic.api :as datomic]))

;; ---- schema -----

(defn partition-schema
  "Return transact-able form to add a partition with name"
  [partition-name]
  [{:db/id (name partition-name) :db/ident (keyword partition-name)}
   [:db/add :db.part/db :db.install/partition (name partition-name)]])

(defn- apply-uniqueness [schema options]
  (if (:unique-value options)
    (assoc schema :db/unique :db.unique/value)
    (if (:unique-identity options)
      (assoc schema :db/unique :db.unique/identity)
      schema)))

(defn build-attribute [kind [attr-name type & spec]]
  (let [options (set spec)
        type    (if (= :kw-ref type) :ref type)]
    (->
      {
       :db/ident       (keyword (name kind) (name attr-name))
       :db/valueType   (keyword "db.type" (name type))
       :db/cardinality (if (contains? options :many) :db.cardinality/many :db.cardinality/one)
       :db/index       (if (:index options) true false)
       :db/isComponent (if (:component options) true false)
       :db/noHistory   (if (:no-history options) true false)
       :db/fulltext    (if (:fulltext options) true false)
       }
      (apply-uniqueness options))))

(defn ->entity-schema [schema]
  (let [kind (-> schema :kind :value)]
    (assert kind (str "kind missing: " schema))
    (assert (keyword? kind) (str "kind must be keyword: " kind))
    (for [[key spec] (seq (dissoc schema :kind :id :*))]
      (let [type (:type spec)
            db   (:db spec)
            [type db] (if (sequential? type) [(first type) (conj db :many)] [type db])]
        (build-attribute kind (concat [key type] db))))))

(defn ->enum-schema [{:keys [enum values]}]
  (mapv (fn [val] {:db/ident (keyword (name enum) (name val))}) values))

(defn ->db-schema
  "converts the schema into format usable by datomic c3kit.bucket.db"
  [schema]
  (cond
    (:kind schema) (->entity-schema schema)
    (:enum schema) (->enum-schema schema)
    :else (throw (ex-info "Invalid schema" schema))))

;; ^^^^^ schema ^^^^^

(defn connect [uri]
  (datomic/create-database uri)
  (datomic/connect uri))

(defn datomic-db [db] (datomic/db @(.-conn db)))

(defn transact! [connection transaction]
  (datomic/transact connection transaction))

(defn clear [db]
  (api/-assert-safety-off! "clear")
  (let [uri (:uri (.-config db))]
    (datomic/delete-database uri)
    (reset! (.-conn db) (connect uri))
    @(transact! @(.-conn db) (.-db-schema db))))

(defn scope-attribute [kind attr] (keyword (name kind) (name attr)))

(defn scope-attributes [kind attributes]
  (reduce-kv (fn [m k v] (assoc m (scope-attribute kind k) v)) {} attributes))

(defn- kind! [entity]
  (or (:kind entity)
      (throw (Exception. (str ":kind missing for " entity)))))

(defn partition-name [db] (or (:partition (.-config db)) :db.part/user))

(defn tempid [db] (datomic/tempid (partition-name db)))
(def tempid? (comp (fnil neg? 0) :idx))

(defn value-or-id [v]
  (if (and (instance? datomic.query.EntityMap v) (contains? v :db/id))
    (:db/id v)
    v))

(defn attributes->entity
  ([attributes id]
   (when (seq attributes)
     (let [kind (namespace (ffirst attributes))]
       (attributes->entity attributes id kind))))
  ([attributes id kind]
   (reduce-kv
     (fn [m k v]
       (assoc m (keyword (name k))
                (if (set? v)
                  (ccc/map-set value-or-id v)
                  (value-or-id v))))
     {:id id :kind (keyword kind)}
     attributes)))

(defn- id->entity [ddb id]
  (when-let [attributes (seq (datomic/entity ddb id))]
    (attributes->entity attributes id)))

(defn- entity
  "ids are always longs in datomic.
  kind (optional) will ensure the kind matches or return nil."
  ([db id]
   (cond
     (number? id) (id->entity (datomic-db db) id)
     (nil? id) nil
     (string? id) (when-not (str/blank? id) (entity db (Long/parseLong id)))
     (map? id) (entity db (:id id))
     :else (throw (UnsupportedOperationException. (str "Unhandled datomic id: " (pr-str id))))))
  ([db kind id]
   (when-let [e (entity db id)]
     (when (or (nil? kind) (= kind (:kind e)))
       e))))

(defn reload
  "Returns a freshly loaded entity"
  [db e]
  (when-let [id (:id e)] (entity db id)))

(defn- q->entities [db result]
  (let [ddb (datomic-db db)]
    (map #(id->entity ddb (first %)) result)))

(defn- q->ids [result] (map first result))

(defn- id-or-val [thing] (or (:db/id thing) thing))

(defn insert-form [id entity]
  (list (-> entity ccc/remove-nils (assoc :db/id id))))

(defn- retract-field-forms [id original retracted-keys]
  (core-reduce (fn [form key]
                 (let [o-val (get original key)]
                   (if (set? o-val)
                     (core-reduce #(conj %1 [:db/retract id key (id-or-val %2)]) form o-val)
                     (conj form [:db/retract id key (id-or-val o-val)]))))
               [] retracted-keys))

(defn- cardinality-many-retract-forms [updated original]
  (core-reduce (fn [form [key val]]
                 (if (or (set? val) (sequential? val))
                   (let [id      (:db/id updated)
                         o-val   (ccc/map-set id-or-val (get original key))
                         missing (set/difference o-val (set val))]
                     (core-reduce #(conj %1 [:db/retract id key (id-or-val %2)]) form missing))
                   form))
               [] updated))

(defn update-form [db id updated]
  (let [original          (into {} (datomic/entity (datomic/db @(.-conn db)) id))
        retracted-keys    (doall (filter #(= nil (get updated %)) (keys original)))
        updated           (-> (apply dissoc updated retracted-keys)
                              ccc/remove-nils
                              (assoc :db/id id))
        seq-retractions   (cardinality-many-retract-forms updated original)
        field-retractions (retract-field-forms id original retracted-keys)]
    (concat [updated] seq-retractions field-retractions)))

(defn maybe-retract-form [entity]
  (when (api/delete? entity)
    (if-let [id (:id entity)]
      (list (list (:kind entity) id) (list [:db.fn/retractEntity id]))
      (throw (Exception. "Can't retract entity without an :id")))))

(defn maybe-cas-form [entity]
  (when-let [old-vals (:cas (meta entity))]
    (let [kind (:kind entity)
          id   (:id entity)]
      [(list kind id)
       (map (fn [[k v]]
              (vector :db/cas id (scope-attribute kind k) v (get entity k)))
            old-vals)])))

(defn tx-entity-form [db entity]
  (let [kind (kind! entity)
        id   (or (:id entity) (tempid db))
        e    (scope-attributes kind (dissoc entity :kind :id))]
    (if (tempid? id)
      (list (list kind id) (insert-form id e))
      (list (list kind id) (update-form db id e)))))

(defn tx-form [db entity]
  (or (maybe-retract-form entity)
      (maybe-cas-form entity)
      (tx-entity-form db entity)))

(defn resolve-id [result id]
  (if (tempid? id)
    (datomic/resolve-tempid (:db-after result) (:tempids result) id)
    id))

(defn- tx-result [db kind id]
  (if-let [e (entity db id)]
    e
    (api/soft-delete kind id)))

(defn tx [db e]
  (let [[[kind id] form] (tx-form db e)
        result @(datomic/transact @(.-conn db) form)
        id     (resolve-id result id)]
    (tx-result db kind id)))

(defn tx* [db entities]
  (let [id-forms (ccc/some-map #(tx-form db %) entities)
        tx-forms (mapcat second id-forms)
        result   @(datomic/transact @(.-conn db) tx-forms)]
    (map (fn [[kind id]] (tx-result db kind (resolve-id result id))) (map first id-forms))))


(defn- ->attr-kw [kind attr] (keyword (name kind) (name attr)))

(declare where-clause)
(defmulti ^:private seq-where-clause (fn [_attr value] (first value)))
(defmethod seq-where-clause 'not [attr [_ value]]
  (if (nil? value)
    (list ['?e attr])
    (list (list 'not ['?e attr value]))))

(defmethod seq-where-clause 'not= [attr [_ value]]
  (if (nil? value)
    (list ['?e attr])
    (list (list 'not ['?e attr value]))))

(defn- simple-where-fn [attr value f-sym]
  (let [attr-sym (gensym (str "?" (name attr)))]
    (list ['?e attr attr-sym]
          [(list f-sym attr-sym value)])))

(defmethod seq-where-clause '> [attr [_ value]] (simple-where-fn attr value '>))
(defmethod seq-where-clause '< [attr [_ value]] (simple-where-fn attr value '<))
(defmethod seq-where-clause '>= [attr [_ value]] (simple-where-fn attr value '>=))
(defmethod seq-where-clause '<= [attr [_ value]] (simple-where-fn attr value '<=))

(defn- or-where-clause [attr values]
  (let [values (set values)]
    (when (seq values)
      (list (cons 'or (mapcat #(where-clause attr %) values))))))

(defmethod seq-where-clause '= [attr [_ & values]] (or-where-clause attr values))

(defmethod seq-where-clause 'or [attr values] (or-where-clause attr (rest values)))
(defmethod seq-where-clause :default [attr values] (or-where-clause attr values))

(defn where-clause [attr value]
  (cond (nil? value) (list [(list 'missing? '$ '?e attr)])
        (set? value) (or-where-clause attr value)
        (sequential? value) (seq-where-clause attr value)
        :else (list ['?e attr value])))

(defn- where-all-of-kind [db kind]
  (let [schema       (legend/for-kind (.-legend db) kind)
        attrs        (keys (dissoc schema :id :kind))
        scoped-attrs (map #(scope-attribute kind %) attrs)]
    [(cons 'or (map (fn [a] ['?e a]) scoped-attrs))]))

(defn- where-single-clause [kind [attr value]]
  (if (nil? value)
    (do (log/warn (str "search for nil value (" kind " " attr "), returning no results.")) nil)
    (where-clause (->attr-kw kind attr) value)))

(defn- where-multi-clause [kind kv-pairs]
  (let [attrs (map #(->attr-kw kind %) (map first kv-pairs))
        vals  (map second kv-pairs)]
    (mapcat where-clause attrs vals)))

(defn- build-where-datalog [db kind kv-pairs]
  (cond (nil? (seq kv-pairs)) (where-all-of-kind db kind)
        (= 1 (core-count kv-pairs)) (where-single-clause kind (first kv-pairs))
        :else (where-multi-clause kind kv-pairs)))

(defn- do-find [db kind options]
  (if-let [where (seq (build-where-datalog db kind (:where options)))]
    (let [query (concat '[:find ?e :in $ :where] where)]
      (->> (datomic/q query (datomic-db db))
           (api/-apply-drop-take options)
           (q->entities db)))
    []))

(defn- do-count [db kind options]
  (if-let [where (build-where-datalog db kind (:where options))]
    (let [query   (concat '[:find (count ?e) :in $ :where] where)
          results (datomic/q query (datomic-db db))]
      (or (ffirst results) 0))
    0))

(defn delete-all [db kind]
  (api/-assert-safety-off! "delete-all")
  (->> (do-find db kind {})
       (partition-all 100)
       (map (fn [batch] (tx* db (map api/soft-delete batch))))
       doall))

(deftype DatomicDB [db-schema legend config conn]
  api/DB
  (-clear [this] (clear this))
  (-delete-all [this kind] (delete-all this kind))
  (-count [this kind options] (do-count this kind options))
  (-entity [this kind id] (entity this kind id))
  (-find [this kind options] (do-find this kind options))
  (-reduce [this kind f init options] (core-reduce f init (do-find this kind options)))
  (-tx [this entity] (tx this entity))
  (-tx* [this entities] (tx* this entities))
  )

(defmethod api/-create-impl :datomic [config schemas]
  (let [legend     (legend/build schemas)
        db-schemas (->> (flatten schemas) (mapcat ->db-schema))
        connection (connect (:uri config))]
    (DatomicDB. db-schemas legend config (atom connection))))
