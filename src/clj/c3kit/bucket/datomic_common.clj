(ns c3kit.bucket.datomic-common
  (:require [c3kit.apron.legend :as legend]
            [c3kit.apron.log :as log]
            [c3kit.apron.schema :as schema]
            [c3kit.bucket.api :as api]
            [clojure.string :as str]))

(defn partition-schema
  "Return transact-able form to add a partition with name"
  [partition-name]
  [{:db/id (name partition-name) :db/ident (keyword partition-name)}
   [:db/add :db.part/db :db.install/partition (name partition-name)]])

(defn spec->attribute [kind attr-name spec index-allowed?]
  (let [spec      (schema/normalize-spec spec)
        type      (:type spec)
        options   (set (:db spec))
        [type many?] (if (= :seq type) [(-> spec :spec :type) true] [type false])
        type      (if (= :kw-ref type) :ref type)
        attribute {:db/ident       (keyword (name kind) (name attr-name))
                   :db/valueType   (keyword "db.type" (name type))
                   :db/cardinality (if many? :db.cardinality/many :db.cardinality/one)
                   :db/isComponent (if (:component options) true false)
                   :db/noHistory   (if (:no-history options) true false)
                   :db/fulltext    (if (:fulltext options) true false)}
        attribute (if index-allowed? (assoc attribute :db/index (boolean (:index options))) attribute)]
    (cond (:unique-value options) (assoc attribute :db/unique :db.unique/value)
          (:unique-identity options) (assoc attribute :db/unique :db.unique/identity)
          :else attribute)))

(defn ->entity-schema [schema index-allowed?]
  (let [schema (schema/conform-schema! schema)
        kind   (-> schema :kind :value)]
    (assert kind (str "kind missing: " schema))
    (assert (keyword? kind) (str "kind must be keyword: " kind))
    (for [[attr spec] (seq (dissoc schema :kind :id :*))]
      (spec->attribute kind attr spec index-allowed?))))

(defn ->enum-schema [{:keys [enum values]}]
  (mapv (fn [val] {:db/ident (keyword (name enum) (name val))}) values))

(defn ->db-schema
  "converts the schema into format usable by datomic c3kit.bucket.db"
  [schema index-allowed?]
  (cond
    (:kind schema) (->entity-schema schema index-allowed?)
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

(defprotocol DatomicApi
  (connect [this])
  (db [this])
  (transact [this transaction])
  (delete-database [this])
  (as-of [this t])
  (q [this query] [this query db args])
  (history [this])
  (do-find [this db kind options])
  (tx [this db e])
  (tx* [this db entities])
  (d-entity [this ddb eid]))

(defn datomic-db [impl]
  (db (.-api impl)))

(defn entity-
  ([db id id->entity attributes->entity]
   (cond
     (number? id) (id->entity db id attributes->entity)
     (nil? id) nil
     (string? id) (when-not (str/blank? id) (entity- db (Long/parseLong id) id->entity attributes->entity))
     (map? id) (entity- db (:id id) id->entity attributes->entity)
     :else (throw (UnsupportedOperationException. (str "Unhandled datomic id: " (pr-str id))))))
  ([db kind id _id->entity _attributes->entity]
   (when-let [e (entity- db id _id->entity _attributes->entity)]
     (when (or (nil? kind) (= kind (:kind e)))
       e))))

(defn reload
  "Returns a freshly loaded entity"
  [db e id->entity attributes->entity]
  (when-let [id (:id e)] (entity- db id id->entity attributes->entity)))

(defn transact!
  "transact a datomic form. Returns a future, so don't forget to deref it if you need it to execute."
  ([transaction]
   (transact! @api/impl transaction))
  ([impl transaction]
   (let [api        (.-api impl)
         connection @(.-conn api)]
     (assert (some? connection))
     (transact api transaction))))

(defn install-schema! [impl]
  (transact! impl (.-db-schema impl)))

(defn clear [impl]
  (api/-assert-safety-off! "clear")
  (let [api (.-api impl)]
    (delete-database api)
    (connect api)
    (install-schema! impl)))

(defn scope-attribute [kind attr] (keyword (name kind) (name attr)))

(defn scope-attributes [kind attributes]
  (reduce-kv (fn [m k v] (assoc m (scope-attribute kind k) v)) {} attributes))

(defn kind! [entity]
  (or (:kind entity)
      (throw (Exception. (str ":kind missing for " entity)))))

(defn partition-name [db] (or (:partition (.-config db)) :db.part/user))

(defn id-or-val [thing] (or (:db/id thing) thing))

(defn maybe-retract-form [entity retract-entity]
  (when (api/delete? entity)
    (if-let [id (:id entity)]
      (list (list (:kind entity) id) (list [retract-entity id]))
      (throw (Exception. "Can't retract entity without an :id")))))

(defn maybe-cas-form [entity]
  (when-let [cas (api/-get-cas entity)]
    (let [{:keys [id kind]} entity
          cas (dissoc cas :kind :id)]
      [(list kind id)
       (map (fn [[k v]]
              (vector :db/cas id (scope-attribute kind k) v (get entity k)))
            cas)])))

(defn tx-form [db entity tx-entity-form retract-entity]
  (or (maybe-retract-form entity retract-entity)
      (maybe-cas-form entity)
      (tx-entity-form db entity)))

(defn ->attr-kw [kind attr] (keyword (name kind) (name attr)))

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
  (let [attr-sym (attr->sym attr)
        regex    (like-query->regex value)]
    (list ['?e attr attr-sym]
          [(list 're-matches regex attr-sym)])))

(defn- ilike-where-fn [attr value]
  (let [attr-sym  (attr->sym attr)
        upper-sym (gensym "?u")
        regex     (-> value str/upper-case like-query->regex)]
    (list
      ['?e attr attr-sym]
      [(list 'clojure.string/upper-case attr-sym) upper-sym]
      [(list 're-matches regex upper-sym)])))

(defn- or-where-clause [attr values]
  (let [values (set values)]
    (when (seq values)
      (list (cons 'or (mapcat #(where-clause attr %) values))))))

(defn- not=-where-clause [attr values]
  (map #(if (nil? %) ['?e attr] (list 'not ['?e attr %])) values))

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
    'ilike (ilike-where-fn attr (second values))
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

(defn build-where-datalog [db kind kv-pairs]
  (cond (nil? (seq kv-pairs)) (where-all-of-kind db kind)
        (= 1 (count kv-pairs)) (where-single-clause kind (first kv-pairs))
        :else (where-multi-clause kind kv-pairs)))

(def reserved-attr-namespaces #{"db" "db.alter" "db.attr" "db.bootstrap" "db.cardinality" "db.entity" "db.excise"
                                "db.fn" "db.install" "db.lang" "db.part" "db.sys" "db.type" "db.unique" "fressian"
                                "deleted" "garbage"})

(defn do-count [db kind options]
  (if-let [where (build-where-datalog db kind (:where options))]
    (let [query   (concat '[:find (count ?e) :in $ :where] where)
          results (q (.-api db) query)]
      (or (ffirst results) 0))
    0))

(defn delete-all [db kind]
  (api/-assert-safety-off! "delete-all")
  (->> (do-find (.-api db) db kind {})
       (partition-all 100)
       (map (fn [batch] (tx* (.-api db) db (map api/soft-delete batch))))
       doall))

(defn installed-schema-idents
  "Returns a list of all the fully qualified idents in the schema."
  ([] (installed-schema-idents @api/impl))
  ([db]
   (->> (q (.-api db) '[:find ?ident ?e :where [?e :db/ident ?ident]])
        (map first)
        (filter (comp not reserved-attr-namespaces namespace))
        sort)))

(defn schema-exists? [db schema]
  (let [kind (-> schema :kind :value name)]
    (boolean (some #(= kind (namespace %)) (installed-schema-idents db)))))

(defn schema-attr-id [db datomic-db key]
  (first (map first (q (.-api db) '[:find ?e :in $ ?ident :where [?e :db/ident ?ident]] datomic-db [key]))))

(defn do-add-attribute! [db kind attr spec index-allowed?]
  (let [qualified-attr (keyword (name kind) (name attr))
        attr-id        (schema-attr-id db (datomic-db db) qualified-attr)]
    (if attr-id
      (log/warn "  add attribute ALREADY EXISTS " qualified-attr)
      (let [attribute (spec->attribute kind attr spec index-allowed?)]
        (log/info "  adding attribute " qualified-attr)
        (transact! db (list attribute))))))

(defn retract-attribute-values [db kind attr]
  (let [qualified-attr (keyword (name kind) (name attr))
        ddb            (datomic-db db)
        attr-id        (schema-attr-id db ddb qualified-attr)]
    (when attr-id
      (log/info "  retracting all values for " qualified-attr)
      (doall (->> (q (.-api db) '[:find ?e ?v :in $ ?attr :where [?e ?attr ?v]] ddb [qualified-attr])
                  (map (fn [[id v]] [:db/retract id qualified-attr v]))
                  (partition-all 100)
                  (map (partial transact! db)))))))

(defn trash-attribute [db kind attr]
  (let [qualified-attr (keyword (name kind) (name attr))
        ddb            (datomic-db db)
        attr-id        (schema-attr-id db ddb qualified-attr)]
    (if attr-id
      (do
        (log/info "  removing " qualified-attr)
        (let [new-name (keyword "garbage" (str (name kind) "." (name attr) "_" (System/currentTimeMillis)))]
          (transact! db [{:db/id attr-id :db/ident new-name}])))
      (log/warn "  remove: MISSING " qualified-attr))))

(defn do-remove-attribute! [db kind attr]
  (retract-attribute-values db kind attr)
  (trash-attribute db kind attr))

(defn do-rename-attribute! [db kind attr new-kind new-attr]
  (let [qualified-old (keyword (name kind) (name attr))
        qualified-new (keyword (name new-kind) (name new-attr))
        ddb           (datomic-db db)
        old-id        (schema-attr-id db ddb qualified-old)
        new-id        (schema-attr-id db ddb qualified-new)]
    (cond (some? new-id) (throw (ex-info "rename to existing attribute" {:old qualified-old :new qualified-new}))
          (nil? old-id) (log/warn "  rename FAILED: MISSING " qualified-old)
          :else (do (log/info (str "  renaming " qualified-old " to " qualified-new))
                    (transact! db [{:db/id old-id :db/ident qualified-new}])))))

(defn do-install-schema! [db schema]
  (let [kind (-> schema :kind :value)]
    (log/info (str "  installing schema " kind))
    (transact! db (->db-schema schema false))))

(defn tx-ids-
  "Same as td-ids but with explicit db instance."
  [impl eid]
  (let [api (.-api impl)]
    (->> (q api '[:find ?tx :in $ ?e :where [?e _ _ ?tx _]] (history api) [eid])
         (sort-by first)
         (map first))))

(defn entity-as-of-tx
  "Loads the entity as it existed when the transaction took place, adding :db/tx (transaction id)
   and :db/instant (date) attributes to the entity."
  [impl eid kind txid attributes->entity]
  (let [tx         (d-entity (.-api impl) (datomic-db impl) txid)
        timestamp  (:db/txInstant tx)
        attributes (d-entity (.-api impl) (as-of (.-api impl) txid) eid)]
    (when (seq attributes)
      (-> attributes
          (attributes->entity kind)
          (assoc :db/tx txid :db/instant timestamp)))))

(defn history-
  "Same as history but with explicit db instance"
  [impl entity attributes->entity]
  (let [id   (:id entity)
        kind (:kind entity)]
    (assert id)
    (assert kind)
    (reduce #(conj %1 (entity-as-of-tx impl id kind %2 attributes->entity)) [] (tx-ids- impl (:id entity)))))

(defn ->eid
  "Returns the entity id"
  [id-or-entity]
  (if (number? id-or-entity) id-or-entity (:id id-or-entity)))

(defn created-at-
  "Same as created-at but with explicit db"
  [impl id-or-entity]
  (let [eid (->eid id-or-entity)
        api (.-api impl)]
    (ffirst (q
              api
              '[:find (min ?inst)
                :in $ ?e
                :where [?e _ _ ?tx]
                [?tx :db/txInstant ?inst]] (history api) [eid]))))

(defn updated-at-
  "Same as updated-at but with explicit db"
  [impl id-or-entity]
  (let [eid (->eid id-or-entity)
        api (.-api impl)]
    (ffirst (q
              api
              '[:find (max ?inst)
                :in $ ?e
                :where [?e _ _ ?tx]
                [?tx :db/txInstant ?inst]] (history api) [eid]))))

(defn with-timestamps-
  "Same as with-timestamps but with explicit db"
  [impl entity]
  (assoc entity :db/created-at (created-at- impl entity) :db/updated-at (updated-at- impl entity)))

(defn excise!-
  "Same as excise! but with explicit db"
  [impl id-or-e]
  (let [id (if-let [id? (:id id-or-e)] id? id-or-e)]
    (transact! impl [{:db/excise id}])))
