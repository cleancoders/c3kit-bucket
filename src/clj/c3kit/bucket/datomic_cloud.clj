(ns c3kit.bucket.datomic-cloud
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.apron.log :as log]
            [c3kit.apron.schema :as schema]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.migrator :as migrator]
            [clojure.set :as set]
            [clojure.string :as str]
            [datomic.client.api :as datomic]))

;; ---- schema -----

(defn partition-schema
  "Return transact-able form to add a partition with name"
  [partition-name]
  [{:db/id (name partition-name) :db/ident (keyword partition-name)}
   [:db/add :db.part/db :db.install/partition (name partition-name)]])

(defn spec->attribute [kind attr-name spec]
  (let [spec      (schema/normalize-spec spec)
        type      (:type spec)
        options   (set (:db spec))
        [type many?] (if (= :seq type) [(-> spec :spec :type) true] [type false])
        type      (if (= :kw-ref type) :ref type)
        attribute {:db/ident       (keyword (name kind) (name attr-name))
                   :db/valueType   (keyword "db.type" (name type))
                   :db/cardinality (if many? :db.cardinality/many :db.cardinality/one)
                   :db/id          (if (:id options) true false)
                   :db/isComponent (if (:component options) true false)
                   :db/noHistory   (if (:no-history options) true false)
                   :db/fulltext    (if (:fulltext options) true false)}]
    (cond (:unique-value options) (assoc attribute :db/unique :db.unique/value)
          (:unique-identity options) (assoc attribute :db/unique :db.unique/identity)
          :else attribute)))

(defn ->entity-schema [schema]
  (let [schema (schema/conform-schema! schema)
        kind   (-> schema :kind :value)]
    (assert kind (str "kind missing: " schema))
    (assert (keyword? kind) (str "kind must be keyword: " kind))
    (for [[attr spec] (seq (dissoc schema :kind :id :*))]
      (spec->attribute kind attr spec))))

(defn ->enum-schema [{:keys [enum values]}]
  (mapv (fn [val] {:db/ident (keyword (name enum) (name val))}) values))

(defn ->db-schema
  "converts the schema into format usable by datomic c3kit.bucket.db"
  [schema]
  (cond
    (:kind schema) (->entity-schema schema)
    (:enum schema) (->enum-schema schema)
    :else (throw (ex-info "Invalid schema" schema))))

(defn attribute->spec [attribute]
  (when-let [value-type (get-in attribute [:db/valueType])]
    (let [ident       (:db/ident attribute)
          cardinality (:db/cardinality attribute)
          index?      (:db/index attribute)
          unique      (:db/unique attribute)
          component?  (:db/isComponent attribute)
          no-history? (:db/noHistory attribute)
          fulltext?   (:db/fulltext attribute)
          kind        (keyword (namespace ident))
          attr-name   (keyword (name ident))
          schema-type (keyword (name value-type))
          schema-type (if (= :db.cardinality/many cardinality) [schema-type] schema-type)
          db          (remove nil? [(when index? :index)
                                    (when component? :component)
                                    (when no-history? :no-history)
                                    (when fulltext? :fulltext)
                                    (when (= :db.unique/identity unique) :unique-identity)
                                    (when (= :db.unique/value unique) :unique-value)])
          spec        {:type schema-type}
          spec        (if (seq db) (assoc spec :db db) spec)]
      [kind attr-name spec])))

(defn all-attributes->specs [attributes]
  (->> attributes (map attribute->spec) (remove nil?)))

;; ^^^^^ schema ^^^^^

(def client (atom nil))

(defn connect [config]
  (datomic/create-database @client config)
  (datomic/connect @client config))

(defn datomic-db [impl] (datomic/db @(.-conn impl)))

(defn transact!
  "transact a datomic form. Returns a future, so don't forget to deref it if you need it to execute."
  ([transaction]
   (transact! @api/impl transaction))
  ([db transaction]
   (assert @(.-conn db))
   (datomic/transact @(.-conn db) {:tx-data transaction})))

(defn install-schema! [db]
  (transact! db (.-db-schema db)))

(defn clear [db]
  (api/-assert-safety-off! "clear")
  (let [config (.-config db)]
    (datomic/delete-database @client config)
    (reset! (.-conn db) (connect config))
    (install-schema! db)))

(defn scope-attribute [kind attr] (keyword (name kind) (name attr)))

(defn scope-attributes [kind attributes]
  (reduce-kv (fn [m k v] (assoc m (scope-attribute kind k) v)) {} attributes))

(defn- kind! [entity]
  (or (:kind entity)
      (throw (Exception. (str ":kind missing for " entity)))))

(defn partition-name [db] (or (:partition (.-config db)) :db.part/user))


(defn tempid
  "Temporary id with default instance"
  [] (* -1 (rand-int 100000)))

(defn tempid?
  "Takes an id and determines if it is temporary"
  [id] (neg? id))

(defn db-as-of [t] (datomic/as-of (datomic-db @api/impl) t))

(defn attributes->entity
  ([attributes]
   (when-let [kind (->> attributes keys (remove #(contains? #{:db/id :id} %)) first)]
     (attributes->entity attributes (namespace kind))))
  ([attributes kind]
   (-> attributes
       (update-keys (comp keyword name))
       (assoc :kind (keyword kind)))))

(defn pull-entity [ddb id] (datomic/pull ddb '[:db/id] id))

(defn- id->entity [ddb id]
  (when-let [attributes (pull-entity ddb id)]
    (attributes->entity attributes)))

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
    (->> result (map first) (map #(id->entity ddb %)))))

(defn- q->ids [result] (map first result))

(defn- id-or-val [thing] (or (:db/id thing) thing))

(defn insert-form [id entity]
  (list (-> entity ccc/remove-nils (assoc :db/id (str id)))))

(defn- ->retract-field-form [id original form key]
  (let [o-val (get original key)]
    (if (or (set? val) (sequential? o-val))
      (reduce #(conj %1 [:db/retract id key (id-or-val %2)]) form o-val)
      (conj form [:db/retract id key (id-or-val o-val)]))))

(defn- retract-field-forms [id original retracted-keys]
  (reduce (partial ->retract-field-form id original) [] retracted-keys))

(defn ->cardinality-many-retract-form [updated original form [key val]]
  (if (or (set? val) (sequential? val))
    (let [id      (:db/id updated)
          o-val   (ccc/map-set id-or-val (get original key))
          missing (set/difference o-val (set val))]
      (reduce #(conj %1 [:db/retract id key (id-or-val %2)]) form missing))
    form))

(defn- cardinality-many-retract-forms [updated original]
  (reduce (partial ->cardinality-many-retract-form updated original) [] updated))

(defn update-form [db id updated]
  (let [original          (dissoc (pull-entity (datomic-db db) id) :db/id)
        retracted-keys    (doall (filter #(nil? (get updated %)) (keys original)))
        updated           (-> (apply dissoc updated retracted-keys)
                              ccc/remove-nils
                              (assoc :db/id id))
        seq-retractions   (cardinality-many-retract-forms updated original)
        field-retractions (retract-field-forms id original retracted-keys)]
    (concat [updated] seq-retractions field-retractions)))

(defn maybe-retract-form [entity]
  (when (api/delete? entity)
    (if-let [id (:id entity)]
      (list (list (:kind entity) id) (list [:db/retractEntity id]))
      (throw (Exception. "Can't retract entity without an :id")))))

(defn maybe-cas-form [entity]
  (when-let [cas (api/-get-cas entity)]
    (let [{:keys [id kind]} entity
          cas (dissoc cas :kind :id)]
      [(list kind id)
       (map (fn [[k v]]
              (vector :db/cas id (scope-attribute kind k) v (get entity k)))
            cas)])))

(defn tx-entity-form [db {:keys [id] :as e}]
  (let [kind (kind! e)
        id   (or id (tempid))
        e    (scope-attributes kind (dissoc e :kind :id))]
    (if (empty? (dissoc (entity db kind id) :id))
      (list (list kind id) (insert-form id e))
      (list (list kind id) (update-form db id e)))))

(defn tx-form [db entity]
  (or (maybe-retract-form entity)
      (maybe-cas-form entity)
      (tx-entity-form db entity)))

(defn resolve-id [db result id]
  (or (:id (entity db id))
      (-> result :tx-data last .e)))
(defn resolve-ids* [result]
  (->> result :tx-data rest (partition-by #(.e %)) (map first) (map #(.e %))))

(defn- tx-result [db kind id]
  (if-let [e (entity db kind id)]
    e
    (api/soft-delete kind id)))

(defn tx [db e]
  (let [[[kind id] form] (tx-form db e)
        result (datomic/transact @(.-conn db) {:tx-data form})
        id     (resolve-id db result id)]
    (tx-result db kind id)))

(defn tx* [db entities]
  (let [id-forms (ccc/some-map #(tx-form db %) entities)
        tx-forms (mapcat second id-forms)
        result   (datomic/transact @(.-conn db) {:tx-data tx-forms})
        ids      (resolve-ids* result)]
    (map (fn [[kind] id] (tx-result db kind id)) (map first id-forms) ids)))


(defn- ->attr-kw [kind attr] (keyword (name kind) (name attr)))

(declare where-clause)

(defn attr->sym [attr]
  (gensym (str "?" (name attr))))

(defn- simple-where-fn [attr value f-sym]
  (let [attr-sym (attr->sym attr)]
    (list ['?e attr attr-sym]
          [(list f-sym attr-sym value)])))

(defn- like-query->regex [value]
  (-> (str/replace value "%" ".*")
      (str/replace "_" ".")
      re-pattern))

(defn- like-where-fn [attr value]
  (let [attr-sym (attr->sym attr)]
    (list ['?e attr attr-sym]
          [(list 're-matches (like-query->regex value) attr-sym)])))

(defn- or-where-clause [attr values]
  (let [values (set values)]
    (when (seq values)
      (list (cons 'or (mapcat #(where-clause attr %) values))))))

(defn- not=-where-clause [attr values]
  (map #(if (nil? %) ['?e attr] (list 'not ['?e attr %])) values))

;(defn not-where-clause [attr [value]]
;  (if (nil? value)
;    (list ['?e attr])
;    (list (list 'not ['?e attr value]))))

(defn- seq-where-clause [attr values]
  (condp = (first values)
    ;'not (not-where-clause attr (rest values))
    'not= (not=-where-clause attr (rest values))
    '> (simple-where-fn attr (second values) '>)
    '< (simple-where-fn attr (second values) '<)
    '>= (simple-where-fn attr (second values) '>=)
    '<= (simple-where-fn attr (second values) '<=)
    '= (or-where-clause attr (rest values))
    'or (or-where-clause attr (rest values))
    'like (like-where-fn attr (second values))
    (or-where-clause attr values)))

(defn where-clause [attr value]
  (cond (nil? value) (list [(list 'missing? '$ '?e attr)])
        (set? value) (or-where-clause attr value)
        (sequential? value) (seq-where-clause attr value)
        :else (list ['?e attr value])))

(defn- where-all-of-kind [db kind]
  (let [schema       (legend/for-kind @(.-legend db) kind)
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
        (= 1 (count kv-pairs)) (where-single-clause kind (first kv-pairs))
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

(def reserved-attr-namespaces #{"db" "db.alter" "db.attr" "db.bootstrap" "db.cardinality" "db.entity" "db.excise"
                                "db.fn" "db.install" "db.lang" "db.part" "db.sys" "db.type" "db.unique" "fressian"
                                "deleted" "garbage"})

(defn installed-schema-idents
  "Returns a list of all the fully qualified idents in the schema."
  ([] (installed-schema-idents @api/impl))
  ([db]
   (->> (datomic/q '[:find ?ident ?e :where [?e :db/ident ?ident]] (datomic-db db))
        (map first)
        (filter (comp not reserved-attr-namespaces namespace))
        sort)))

(defn schema-exists? [db schema]
  (let [kind (-> schema :kind :value name)]
    (boolean (some #(= kind (namespace %)) (installed-schema-idents db)))))

(defn installed-schema-legend
  ([] (installed-schema-legend @api/impl))
  ([db]
   (let [ddb (datomic-db db)]
     (->> (installed-schema-idents db)
          (map #(->> % (datomic/pull ddb '[*]) (into {})))
          (map #(update % :db/valueType :db/ident))
          (map attribute->spec)
          (reduce (fn [result [kind attr spec]] (assoc-in result [kind attr] spec)) {})))))

(defn- do-install-schema! [db schema]
  (let [kind (-> schema :kind :value)]
    (log/info (str "  installing schema " kind))
    (transact! db (->db-schema schema))))

(defn- schema-attr-id [datomic-db key]
  (first (map first (datomic/q '[:find ?e :in $ ?ident :where [?e :db/ident ?ident]] datomic-db key))))

(defn- do-add-attribute! [db kind attr spec]
  (let [qualified-attr (keyword (name kind) (name attr))
        attr-id        (schema-attr-id (datomic-db db) qualified-attr)]
    (if attr-id
      (log/warn "  add attribute ALREADY EXISTS " qualified-attr)
      (let [attribute (spec->attribute kind attr spec)]
        (log/info "  adding attribute " qualified-attr)
        (transact! db (list attribute))))))

(defn retract-attribute-values [db kind attr]
  (let [qualified-attr (keyword (name kind) (name attr))
        ddb            (datomic-db db)
        attr-id        (schema-attr-id ddb qualified-attr)]
    (when attr-id
      (log/info "  retracting all values for " qualified-attr)
      (doall (->> (datomic/q '[:find ?e ?v :in $ ?attr :where [?e ?attr ?v]] ddb qualified-attr)
                  (map (fn [[id v]] [:db/retract id qualified-attr v]))
                  (partition-all 100)
                  (map (partial transact! db)))))))

(defn trash-attribute [db kind attr]
  (let [qualified-attr (keyword (name kind) (name attr))
        ddb            (datomic-db db)
        attr-id        (schema-attr-id ddb qualified-attr)]
    (if attr-id
      (do
        (log/info "  removing " qualified-attr)
        (let [new-name (keyword "garbage" (str (name kind) "." (name attr) "_" (System/currentTimeMillis)))]
          (transact! db [{:db/id attr-id :db/ident new-name}])))
      (log/warn "  remove: MISSING " qualified-attr))))

(defn- do-remove-attribute! [db kind attr]
  (retract-attribute-values db kind attr)
  (trash-attribute db kind attr))

(defn- do-rename-attribute! [db kind attr new-kind new-attr]
  (let [qualified-old (keyword (name kind) (name attr))
        qualified-new (keyword (name new-kind) (name new-attr))
        ddb           (datomic-db db)
        old-id        (schema-attr-id ddb qualified-old)
        new-id        (schema-attr-id ddb qualified-new)]
    (cond (some? new-id) (throw (ex-info "rename to existing attribute" {:old qualified-old :new qualified-new}))
          (nil? old-id) (log/warn "  rename FAILED: MISSING " qualified-old)
          :else (do (log/info (str "  renaming " qualified-old " to " qualified-new))
                    (transact! db [{:db/id old-id :db/ident qualified-new}])))))

(deftype DatomicDB [db-schema legend config conn]
  api/DB
  (close [_this] (comment "Nothing to do here"))
  (-clear [this] (clear this))
  (-delete-all [this kind] (delete-all this kind))
  (-count [this kind options] (do-count this kind options))
  (-entity [this kind id] (entity this kind id))
  (-find [this kind options] (do-find this kind options))
  (-reduce [this kind f init options] (reduce f init (do-find this kind options)))
  (-tx [this entity] (tx this entity))
  (-tx* [this entities] (tx* this entities))
  migrator/Migrator
  (-schema-exists? [this schema] (schema-exists? this schema))
  (-installed-schema-legend [this _expected-legend] (installed-schema-legend this))
  (-install-schema! [this schema] (do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [this kind attr spec] (do-add-attribute! this kind attr spec))
  (-remove-attribute! [this kind attr] (do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (do-rename-attribute! this kind attr new-kind new-attr)))

(defmethod api/-create-impl :datomic-cloud [config schemas]
  (reset! client (datomic/client config))
  (let [legend     (atom (legend/build schemas))
        db-schemas (->> (flatten schemas) (mapcat ->db-schema))
        connection (connect config)
        db         (DatomicDB. db-schemas legend config (atom connection))]
    db))

(defmethod migrator/migration-schema :datomic-cloud [_]
  (merge-with merge migrator/default-migration-schema {:name {:db [:unique-value]}}))

(defn tx-ids-
  "Same as td-ids but with explicit db instance."
  [impl eid]
  (->> (datomic/q
        '[:find ?tx
          :in $ ?e
          :where
          [?e _ _ ?tx _]]
        (datomic/history (datomic-db impl)) eid)
       (sort-by first)
       (map first)))

(defn tx-ids
  "Returns a sorted list of all the transaction ids in which the entity was updated."
  [eid]
  (tx-ids- @api/impl eid))

(defn entity-as-of-tx
  "Loads the entity as it existed when the transaction took place, adding :db/tx (transaction id)
   and :db/instant (date) attributes to the entity."
  [datomic-db eid kind txid]
  (let [tx         (pull-entity datomic-db txid)
        timestamp  (:db/txInstant tx)
        attributes (pull-entity (datomic/as-of datomic-db txid) eid)]
    (when (seq attributes)
      (-> attributes
          (attributes->entity kind)
          (assoc :db/tx txid :db/instant timestamp)))))

(defn history-
  "Same as history but with explicit db instance"
  [impl entity]
  (let [id   (:id entity)
        kind (:kind entity)]
    (assert id)
    (assert kind)
    (reduce #(conj %1 (entity-as-of-tx (datomic-db impl) id kind %2)) [] (tx-ids- impl (:id entity)))))

(defn history
  "Returns a list of every version of the entity form creation to current state,
  with :db/tx and :db/instant attributes."
  [entity] (history- @api/impl entity))

(defn ->eid
  "Returns the entity id"
  [id-or-entity]
  (if (number? id-or-entity) id-or-entity (:id id-or-entity)))

(defn created-at-
  "Same as created-at but with explicit db"
  [impl id-or-entity]
  (let [eid (->eid id-or-entity)]
    (ffirst (datomic/q '[:find (min ?inst)
                         :in $ ?e
                         :where [?e _ _ ?tx]
                         [?tx :db/txInstant ?inst]] (datomic/history (datomic-db impl)) eid))))

(defn created-at
  "Returns the instant (java.util.Date) the entity was created."
  [id-or-entity]
  (created-at- @api/impl id-or-entity))

(defn updated-at-
  "Same as updated-at but with explicit db"
  [impl id-or-entity]
  (let [eid (->eid id-or-entity)]
    (ffirst (datomic/q '[:find (max ?inst)
                         :in $ ?e
                         :where [?e _ _ ?tx]
                         [?tx :db/txInstant ?inst]] (datomic/history (datomic-db impl)) eid))))

(defn updated-at
  "Returns the instant (java.util.Date) this entity was last updated."
  [id-or-entity]
  (updated-at- @api/impl id-or-entity))

(defn with-timestamps-
  "Same as with-timestamps but with explicit db"
  [impl entity]
  (assoc entity :db/created-at (created-at- impl entity) :db/updated-at (updated-at- impl entity)))

(defn with-timestamps
  "Adds :created-at and :updated-at timestamps to the entity."
  [entity]
  (with-timestamps- @api/impl entity))

(defn excise!-
  "Same as excise! but with explicit db"
  [impl id-or-e]
  (let [id (if-let [id? (:id id-or-e)] id? id-or-e)]
    (transact! impl [{:db/excise id}])))

(defn excise!
  "Remove entity from database history."
  [id-or-e]
  (excise!- @api/impl id-or-e))

(defn q
  "Raw datalog query."
  [query & args]
  (apply datomic/q query (datomic-db @api/impl) args))

(defn find-datalog
  "Run a datalog query (for full entities) returning the results as entities on default instance."
  [query & args]
  (q->entities @api/impl (apply q query args)))
