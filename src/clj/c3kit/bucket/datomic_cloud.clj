(ns c3kit.bucket.datomic-cloud
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.migrator :as migrator]
            [clojure.set :as set]
            [datomic.client.api :as datomic]
            [c3kit.bucket.datomic-common :as common-api]))

(defn connect [config client]
  (datomic/create-database client config)
  (datomic/connect client config))

(defn tempid
  "Temporary id with default instance"
  [] (* -1 (rand-int 100000)))

(defn tempid?
  "Takes an id and determines if it is temporary"
  [id] (neg? id))

(defn db-as-of [t] (common-api/as-of (.-api @api/impl) t))

(defn update-refs [entity]
  (update-vals entity (fn [v] (:db/id v v))))

(defn attributes->entity
  ([attributes]
   (when-let [kind (->> attributes keys (remove #(contains? #{:db/id} %)) first)]
     (attributes->entity attributes (namespace kind))))
  ([attributes kind]
   (-> attributes
       (update-keys (comp keyword name))
       update-refs
       (assoc :kind (keyword kind)))))

(defn pull-entity [ddb id] (datomic/pull ddb '[*] id))

(defn- id->entity [db id attributes->entity]
  (when-let [attributes (common-api/d-entity (.-api db)  (common-api/datomic-db db) id)]
    (attributes->entity attributes)))

(defn- entity
  "ids are always longs in datomic.
  kind (optional) will ensure the kind matches or return nil."
  ([db id]
   (common-api/entity- db id id->entity attributes->entity))
  ([db kind id]
   (common-api/entity- db kind id id->entity attributes->entity)))

(defn reload
  "Returns a freshly loaded entity"
  [db e]
  (common-api/reload db e id->entity attributes->entity))

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
  (let [original          (dissoc (common-api/d-entity (.-api db) (common-api/datomic-db db) id) :db/id)
        retracted-keys    (doall (filter #(nil? (get updated %)) (keys original)))
        updated           (-> (apply dissoc updated retracted-keys)
                              ccc/remove-nils
                              (assoc :db/id id))
        seq-retractions   (cardinality-many-retract-forms updated original)
        field-retractions (retract-field-forms id original retracted-keys)]
    (concat [updated] seq-retractions field-retractions)))

(defn tx-entity-form [db {:keys [id] :as e}]
  (let [kind (common-api/kind! e)
        id   (or id (tempid))
        e    (common-api/scope-attributes kind (dissoc e :kind :id))]
    (if (empty? (dissoc (entity db kind id) :id))
      (list (list kind id) (insert-form id e))
      (list (list kind id) (update-form db id e)))))

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
  (let [[[kind id] form] (common-api/tx-form db e tx-entity-form :db/retractEntity)
        result (common-api/transact (.-api db) form)
        id     (resolve-id db result id)]
    (tx-result db kind id)))

(defn tx* [db entities]
  (let [id-forms (ccc/some-map #(common-api/tx-form db % tx-entity-form :db/retractEntity) entities)
        tx-forms (mapcat second id-forms)
        result   (common-api/transact (.-api db) tx-forms)
        ids      (resolve-ids* result)]
    (map (fn [[kind] id] (tx-result db kind id)) (map first id-forms) ids)))

(defn q->cloud-entities [results]
  (map attributes->entity (map first results)))

(defn do-find [db kind options]
  (if-let [where (seq (common-api/build-where-datalog db kind (:where options)))]
    (let [query (concat '[:find (pull ?e [*]) :in $ :where] where)]
      (->> (common-api/q (.-api db) query)
           (api/-apply-drop-take options)
           (q->cloud-entities)))
    []))

(defn installed-schema-legend
  ([] (installed-schema-legend @api/impl))
  ([db]
   (let [ddb (common-api/datomic-db db)]
     (->> (common-api/installed-schema-idents db)
          (map #(->> % (datomic/pull ddb '[*]) (into {})))
          (map #(update % :db/valueType :db/ident))
          (map common-api/attribute->spec)
          (reduce (fn [result [kind attr spec]] (assoc-in result [kind attr] spec)) {})))))

(deftype DatomicCloudDB [db-schema legend config api]
  api/DB
  (close [_this] (comment "Nothing to do here"))
  (-clear [this] (common-api/clear this))
  (-delete-all [this kind] (common-api/delete-all this kind))
  (-count [this kind options] (common-api/do-count this kind options))
  (-entity [this kind id] (entity this kind id))
  (-find [this kind options] (common-api/do-find api this kind options))
  (-reduce [this kind f init options] (reduce f init (common-api/do-find api this kind options)))
  (-tx [this entity] (common-api/tx api this entity))
  (-tx* [this entities] (common-api/tx* api this entities))
  migrator/Migrator
  (-schema-exists? [this schema] (common-api/schema-exists? this schema))
  (-installed-schema-legend [this _expected-legend] (installed-schema-legend this))
  (-install-schema! [this schema] (common-api/do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [this kind attr spec] (common-api/do-add-attribute! this kind attr spec false))
  (-remove-attribute! [this kind attr] (common-api/do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (common-api/do-rename-attribute! this kind attr new-kind new-attr)))

(deftype DatomicCloudApi [config client conn]
  common-api/DatomicApi
  (connect [this] (reset! conn (connect config client)))
  (db [this] (datomic/db @conn))
  (transact [this transaction] (datomic/transact @conn {:tx-data transaction}))
  (delete-database [this]
    (datomic/delete-database client config))
  (as-of [this t] (datomic/as-of (datomic/db @conn) t))
  (q [this query] (datomic/q query (datomic/db @conn)))
  (q [this query db args]
    (apply datomic/q query db args))
  (history [this]
    (datomic/history (datomic/db @conn)))
  (do-find [this db kind options] (do-find db kind options))
  (tx [this db e] (tx db e))
  (tx* [this db entities] (tx* db entities))
  (d-entity [this ddb eid] (pull-entity ddb eid)))

(defmethod api/-create-impl :datomic-cloud [config schemas]
  (let [datomic-client (datomic/client config)
        legend         (atom (legend/build schemas))
        db-schemas     (->> (flatten schemas) (mapcat #(common-api/->db-schema % false)))
        api            (->DatomicCloudApi config datomic-client (atom nil))
        db             (DatomicCloudDB. db-schemas legend config api)]
    (common-api/connect api)
    db))

(defmethod migrator/migration-schema :datomic-cloud [_]
  (merge-with merge migrator/default-migration-schema {:name {:db [:unique-value]}}))

(defn tx-ids
  "Returns a sorted list of all the transaction ids in which the entity was updated."
  [eid]
  (common-api/tx-ids- @api/impl eid))

(defn history
  "Returns a list of every version of the entity form creation to current state,
  with :db/tx and :db/instant attributes."
  [entity] (common-api/history- @api/impl entity attributes->entity))

(defn created-at
  "Returns the instant (java.util.Date) the entity was created."
  [id-or-entity]
  (common-api/created-at- @api/impl id-or-entity))

(defn updated-at
  "Returns the instant (java.util.Date) this entity was last updated."
  [id-or-entity]
  (common-api/updated-at- @api/impl id-or-entity))

(defn with-timestamps
  "Adds :created-at and :updated-at timestamps to the entity."
  [entity]
  (common-api/with-timestamps- @api/impl entity))

(defn excise!
  "Remove entity from database history."
  [id-or-e]
  (common-api/excise!- @api/impl id-or-e))

(defn q
  "Raw datalog query."
  [query & args]
  (apply common-api/q query (common-api/datomic-db @api/impl) args))

(defn find-datalog
  "Run a datalog query (for full entities) returning the results as entities on default instance."
  [query & args]
  (q->cloud-entities (apply q query args)))
