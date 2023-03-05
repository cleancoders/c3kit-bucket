(ns c3kit.bucket.datomic
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

(defn do-install-schema [db new-schemas]
  (swap! (.-db-schema db) (fn [existing]
                            (let [db-schemas     (->> (flatten new-schemas) (mapcat ->db-schema))
                                  new-db-schemas (remove #(ccc/index-of % existing) db-schemas)]
                              (concat existing new-db-schemas))))
  (swap! (.-legend db) merge (legend/build new-schemas)))

(defn clear [db]
  (api/-assert-safety-off! "clear")
  (let [uri (:uri (.-config db))]
    (datomic/delete-database uri)
    (reset! (.-conn db) (connect uri))
    @(transact! @(.-conn db) @(.-db-schema db))))

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

(defn- entity
  "ids are always longs in datomic.
  kind (optional) will ensure the kind matches or return nil."
  ([db id]
   (cond
     (number? id) (when-let [attributes (seq (datomic/entity (datomic/db @(.-conn db)) id))]
                    (attributes->entity attributes id))
     (nil? id) nil
     (string? id) (when-not (str/blank? id) (entity db (Long/parseLong id)))
     :else (attributes->entity (seq id) (:db/id id))))
  ([db kind id]
   (when-let [e (entity db id)]
     (when (or (nil? kind) (= kind (:kind e)))
       e))))

(defn reload [db e] (when-let [id (:id e)] (entity db id)))

(defn q->entities [db result] (map #(entity db (first %)) result))
(defn q->ids [result] (map first result))

(defn- id-or-val [thing] (or (:db/id thing) thing))

(defn insert-form [id entity]
  (list (-> entity ccc/remove-nils (assoc :db/id id))))

(defn- retract-field-forms [id original retracted-keys]
  (reduce (fn [form key]
            (let [o-val (get original key)]
              (if (set? o-val)
                (reduce #(conj %1 [:db/retract id key (id-or-val %2)]) form o-val)
                (conj form [:db/retract id key (id-or-val o-val)]))))
          [] retracted-keys))

(defn- cardinality-many-retract-forms [updated original]
  (reduce (fn [form [key val]]
            (if (or (set? val) (sequential? val))
              (let [id      (:db/id updated)
                    o-val   (ccc/map-set id-or-val (get original key))
                    missing (set/difference o-val (set val))]
                (reduce #(conj %1 [:db/retract id key (id-or-val %2)]) form missing))
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
    (if (seq values)
      (list (cons 'or (mapcat #(where-clause attr %) values)))
      [nil])))

(defmethod seq-where-clause '= [attr [_ & values]] (or-where-clause attr values))

(defmethod seq-where-clause 'or [attr values] (or-where-clause attr (rest values)))
(defmethod seq-where-clause :default [attr values] (or-where-clause attr values))

(defn where-clause [attr value]
  (cond (nil? value) (list [(list 'missing? '$ '?e attr)])
        (set? value) (or-where-clause attr value)
        (sequential? value) (seq-where-clause attr value)
        :else (list ['?e attr value])))

(defn find-where
  "Search for all entities that match the datalog 'where' clause passed in."
  [db where]
  (if (some nil? where)
    []
    (let [query  (concat '[:find ?e :in $ :where] where)
          result (datomic/q query (datomic-db db))]
      (q->entities db result))))

(defn find-ids-where
  "Search for ids of entities that match the datalog 'where' clause passed in."
  [db where]
  (if (some nil? where)
    []
    (let [query (concat '[:find ?e :in $ :where] where)]
      (q->ids (datomic/q query (datomic-db db))))))

(defn count-where
  "Count all entities that match the datalog 'where' clause passed in."
  [db where]
  (if (some nil? where)
    0
    (let [query (concat '[:find (count ?e) :in $ :where] where)]
      (or (ffirst (datomic/q query (datomic-db db))) 0))))

(defn- do-search [db q-fn default kind kv-pairs]
  (if (= 1 (count kv-pairs))
    (let [[attr value] (first kv-pairs)]
      (if (nil? value)
        (do (log/warn (str "search for nil value (" kind " " attr "), returning no results.")) default)
        (q-fn db (where-clause (->attr-kw kind attr) value))))
    (let [attrs (map #(->attr-kw kind %) (map first kv-pairs))
          vals  (map second kv-pairs)]
      (q-fn db (mapcat where-clause attrs vals)))))

(defn- query-all [db kind thing]
  (let [schema       (legend/for-kind @(.-legend db) kind)
        attrs        (keys (dissoc schema :id :kind))
        scoped-attrs (map #(scope-attribute kind %) attrs)
        where        (cons 'or (map (fn [a] ['?e a]) scoped-attrs))]
    (conj [:find thing :in '$ :where] where)))

(defn- do-find [db kind options]
  (if-let [where (seq (:where options))]
    (do-search db find-where [] kind where)
    (q->entities db (datomic/q (query-all db kind '?e) (datomic-db db)))))

(defn find-by [db kind kvs] (do-search db find-where [] kind (api/-kvs->kv-pairs kvs)))
(defn ffind-by [db kind kvs] (first (find-by db kind kvs)))
(defn count-by [db kind kvs] (do-search db count-where 0 kind (api/-kvs->kv-pairs kvs)))

(defn find [db kind & opt-args]
  (let [options (ccc/->options opt-args)]
    (do-find db kind options)))

(defn find-all [db kind]
  (let [query (query-all db kind '?e)]
    (q->entities db (datomic/q query (datomic-db db)))))

(defn delete-all [db kind]
  (api/-assert-safety-off! "delete-all")
  (->> (find-all db kind)
       (partition-all 100)
       (map (fn [batch] (tx* db (map api/soft-delete batch))))
       doall))

(defn count-all [db kind]
  (let [query (query-all db kind '(count ?e))]
    (or (ffirst (datomic/q query (datomic-db db))) 0)))

(defn reduce-by [db kind f val kvs]
  (if (seq kvs)
    (reduce f val (find-by db kind kvs))
    (reduce f val (find-all db kind))))

(deftype DatomicDB [db-schema legend config conn]
  api/DB
  (-install-schema [this new-schemas] (do-install-schema this new-schemas))
  (-clear [this] (clear this))
  (-delete-all [this kind] (delete-all this kind))
  (-count-all [this kind] (count-all this kind))
  (-count-by [this kind kvs] (count-by this kind kvs))
  (-entity [this kind id] (entity this kind id))
  (-find [this kind options] (do-find this kind options))
  (-find-all [this kind] (find-all this kind))
  (-find-by [this kind kvs] (find-by this kind kvs))
  (-ffind-by [this kind kvs] (ffind-by this kind kvs))
  (-reduce-by [this kind f val kvs] (reduce-by this kind f val kvs))
  (-tx [this entity] (tx this entity))
  (-tx* [this entities] (tx* this entities))
  )

(defn create-db
  ([config] (create-db config []))
  ([config schemas]
   (let [legend     (legend/build schemas)
         connection (connect (:uri config))]
     (DatomicDB. (atom schemas) (atom legend) config (atom connection)))))
