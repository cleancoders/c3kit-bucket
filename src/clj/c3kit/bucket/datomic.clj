(ns c3kit.bucket.datomic
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.datomic-common :as common-api]
            [c3kit.bucket.history :as history]
            [c3kit.bucket.migrator :as migrator]
            [clojure.set :as set]
            [datomic.api :as datomic]))

(defn connect [uri]
  (datomic/create-database uri)
  (datomic/connect uri))

(defn tempid-
  "Temporary id with specified instance"
  [db] (datomic/tempid (common-api/partition-name db)))

(defn tempid
  "Temporary id with default instance"
  [] (tempid- @api/impl))

(defn tempid?
  "Takes an id and determines if it is temporary"
  [id]
  (if-let [idx (:idx id)]
    (neg? idx)
    false))

(defn value-or-id [v]
  (if (and (instance? datomic.query.EntityMap v) (contains? v :db/id))
    (:db/id v)
    v))

(defn db-as-of [t] (common-api/-as-of-db @api/impl t))

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

(defn- id->entity [db id attributes->entity]
  (when-let [attributes (seq (common-api/-d-entity db (common-api/datomic-db db) id))]
    (attributes->entity attributes id)))

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

(defn- q->entities [db result]
  (map #(id->entity db (first %) attributes->entity) result))

(defn insert-form [id entity]
  (list (-> entity ccc/remove-nils (assoc :db/id id))))

(defn- retract-field-forms [id original retracted-keys]
  (reduce (fn [form key]
            (let [o-val (get original key)]
              (if (set? o-val)
                (reduce #(conj %1 [:db/retract id key (common-api/id-or-val %2)]) form o-val)
                (conj form [:db/retract id key (common-api/id-or-val o-val)]))))
          [] retracted-keys))

(defn- cardinality-many-retract-forms [updated original]
  (reduce (fn [form [key val]]
            (if (or (set? val) (sequential? val))
              (let [id      (:db/id updated)
                    o-val   (ccc/map-set common-api/id-or-val (get original key))
                    missing (set/difference o-val (set val))]
                (reduce #(conj %1 [:db/retract id key (common-api/id-or-val %2)]) form missing))
              form))
          [] updated))

(defn update-form [db id updated]
  (let [original          (into {} (common-api/-d-entity db (common-api/datomic-db db) id))
        retracted-keys    (doall (filter #(nil? (get updated %)) (keys original)))
        updated           (-> (apply dissoc updated retracted-keys)
                              ccc/remove-nils
                              (assoc :db/id id))
        seq-retractions   (cardinality-many-retract-forms updated original)
        field-retractions (retract-field-forms id original retracted-keys)]
    (concat [updated] seq-retractions field-retractions)))

(defn tx-entity-form [db entity]
  (let [kind (common-api/kind! entity)
        id   (or (:id entity) (tempid- db))
        e    (common-api/scope-attributes kind (dissoc entity :kind :id))]
    (if (tempid? id)
      (list (list kind id) (insert-form id e))
      (list (list kind id) (update-form db id e)))))

(defn resolve-id [result id]
  (if (tempid? id)
    (datomic/resolve-tempid (:db-after result) (:tempids result) id)
    id))

(defn- tx-result [db kind id]
  (if-let [e (entity db id)]
    e
    (api/soft-delete kind id)))

(defn tx [db e]
  (let [[[kind id] form] (common-api/tx-form db e tx-entity-form :db.fn/retractEntity)
        result @(common-api/-transact db form)
        id     (resolve-id result id)]
    (tx-result db kind id)))

(defn tx* [db entities]
  (let [id-forms (ccc/some-map #(common-api/tx-form db % tx-entity-form :db.fn/retractEntity) entities)
        tx-forms (mapcat second id-forms)
        result   @(common-api/-transact db tx-forms)]
    (map (fn [[kind id]] (tx-result db kind (resolve-id result id))) (map first id-forms))))

(defn do-find [db kind options]
  (if-let [where (seq (common-api/build-where-datalog db kind (:where options)))]
    (let [query (concat '[:find ?e :in $ :where] where)]
      (->> (common-api/-q db query)
           (api/-apply-drop-take options)
           (q->entities db)))
    []))

(defn installed-schema-legend
  ([] (installed-schema-legend @api/impl))
  ([db]
   (let [ddb (common-api/datomic-db db)]
     (->> (common-api/installed-schema-idents db)
          (map #(->> % (common-api/-d-entity db ddb) (into {})))
          common-api/attributes->legend))))

(deftype DatomicAsOfView [impl aodb]
  common-api/DatomicApi
  (-connect [_] (throw (ex-info "as-of view is read-only" {})))
  (-db [_] aodb)
  (-transact [_ _] (throw (ex-info "as-of view is read-only" {})))
  (-delete-database [_] (throw (ex-info "as-of view is read-only" {})))
  (-q [_ query] (common-api/-q impl query aodb []))
  (-q [_ query db args] (common-api/-q impl query db args))
  (-d-entity [_ ddb eid] (common-api/-d-entity impl ddb eid))
  (-history-db [_] (common-api/-history-db impl))
  (-as-of-db [_ t] (common-api/-as-of-db impl t))
  api/DB
  (close [_] nil)
  (-legend [_] (api/-legend impl))
  (-entity [this kind id] (entity this kind id))
  (-find [this kind options] (do-find this kind options))
  (-count [this kind options] (common-api/do-count this kind options))
  (-reduce [this kind f init options] (reduce f init (do-find this kind options)))
  (-tx [_ _] (throw (ex-info "as-of view is read-only" {})))
  (-tx* [_ _] (throw (ex-info "as-of view is read-only" {})))
  (-clear [_] (throw (ex-info "as-of view is read-only" {})))
  (-delete-all [_ _] (throw (ex-info "as-of view is read-only" {}))))

(deftype DatomicDB [db-schema legend config conn]
  api/DB
  (close [_this] nil)
  (-legend [_this] legend)
  (-clear [this] (common-api/clear this))
  (-delete-all [this kind] (common-api/delete-all this kind))
  (-count [this kind options] (common-api/do-count this kind options))
  (-entity [this kind id] (entity this kind id))
  (-find [this kind options] (do-find this kind options))
  (-reduce [this kind f init options] (reduce f init (do-find this kind options)))
  (-tx [this entity] (tx this entity))
  (-tx* [this entities] (tx* this entities))
  common-api/DatomicApi
  (-connect [_this] (reset! conn (connect (:uri config))))
  (-db [_this] (datomic/db @conn))
  (-transact [_this transaction] (datomic/transact @conn transaction))
  (-delete-database [_this] (datomic/delete-database (:uri config)))
  (-q [_this query] (datomic/q query (datomic/db @conn)))
  (-q [_this query db args] (apply datomic/q query db args))
  (-d-entity [_this ddb eid] (datomic/entity ddb eid))
  (-history-db [_this] (datomic/history (datomic/db @conn)))
  (-as-of-db [_this t] (datomic/as-of (datomic/db @conn) t))
  history/HistoryDB
  (-history [this entity] (common-api/history-versions- this entity attributes->entity))
  (-as-of [this t] (history/->ReadOnlyDB (DatomicAsOfView. this (common-api/-as-of-db this t))))
  (-created-at [this id-or-entity] (common-api/created-at- this id-or-entity))
  (-updated-at [this id-or-entity] (common-api/updated-at- this id-or-entity))
  (-excise! [this id-or-entity] (common-api/excise!- this id-or-entity))
  migrator/Migrator
  (-schema-exists? [this schema] (common-api/schema-exists? this schema))
  (-installed-schema-legend [this _expected-legend] (installed-schema-legend this))
  (-install-schema! [this schema] (common-api/do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [this kind attr spec] (common-api/do-add-attribute! this kind attr spec true))
  (-remove-attribute! [this kind attr] (common-api/do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (common-api/do-rename-attribute! this kind attr new-kind new-attr)))

(defmethod api/-create-impl :datomic [config schemas]
  (let [legend     (atom (legend/build schemas))
        db-schemas (->> (flatten schemas) (mapcat #(common-api/->db-schema % true)))
        db         (DatomicDB. db-schemas legend config (atom nil))]
    (common-api/-connect db)
    db))

(defmethod migrator/migration-schema :datomic [_]
  (merge-with merge migrator/default-migration-schema {:name {:db [:unique-value]}}))

(defn find-max-of-all-
  "Finds the entity with the max attribute for a given kind with specific db instance"
  [db kind attr]
  (->> (common-api/-q
         db
         '[:find (max ?e) :in $ ?attribute
           :where [?e ?attribute]]
         (common-api/datomic-db db)
         [(common-api/->attr-kw kind attr)])
       (q->entities db)
       first))

(defn find-max-of-all
  "Finds the entity with the max attribute for a given kind with default db instance"
  [kind attr]
  (find-max-of-all- @api/impl kind attr))

(defn find-max-val-of-all-
  "Finds the max value of a kind/attr with specific db instance"
  [db kind attr]
  (-> (find-max-of-all- db kind attr) (get attr)))

(defn find-max-val-of-all
  "Finds the max value of a kind/attr with default db instance"
  [kind attr]
  (find-max-val-of-all- @api/impl kind attr))

(defn find-min-of-all-
  "Finds the entity with the min attribute for a given kind with specific db instance"
  [db kind attr]
  (->> (common-api/-q
         db
         '[:find (min ?e) :in $ ?attribute
           :where [?e ?attribute]]
         (common-api/datomic-db db)
         [(common-api/->attr-kw kind attr)])
       (q->entities db)
       first))

(defn find-min-of-all
  "Finds the entity with the min attribute for a given kind with default db instance"
  [kind attr]
  (find-min-of-all- @api/impl kind attr))

(defn find-min-val-of-all-
  "Finds the min value of a kind/attr with specific db instance"
  [db kind attr]
  (-> (find-min-of-all- db kind attr) (get attr)))

(defn find-min-val-of-all
  "Finds the min value of a kind/attr with default db instance"
  [kind attr]
  (find-min-val-of-all- @api/impl kind attr))

(defn history
  "Returns a list of every version of the entity form creation to current state,
  with :db/tx and :db/instant attributes."
  [entity] (common-api/history- @api/impl entity attributes->entity))

(defn ->eid
  "Returns the entity id"
  [id-or-entity]
  (common-api/->eid id-or-entity))

(defn created-at-
  "Same as created-at but with explicit db"
  [impl id-or-entity]
  (common-api/created-at- impl id-or-entity))

(defn created-at
  "Returns the instant (java.util.Date) the entity was created."
  [id-or-entity]
  (created-at- @api/impl id-or-entity))

(defn updated-at-
  "Same as updated-at but with explicit db"
  [impl id-or-entity]
  (common-api/updated-at- impl id-or-entity))

(defn updated-at
  "Returns the instant (java.util.Date) this entity was last updated."
  [id-or-entity]
  (updated-at- @api/impl id-or-entity))

(defn with-timestamps-
  "Same as with-timestamps but with explicit db"
  [impl entity]
  (common-api/with-timestamps- impl entity))

(defn with-timestamps
  "Adds :created-at and :updated-at timestamps to the entity."
  [entity]
  (with-timestamps- @api/impl entity))

(defn excise!-
  "Same as excise! but with explicit db"
  [impl id-or-e]
  (common-api/excise!- impl id-or-e))

(defn excise!
  "Remove entity from database history."
  [id-or-e]
  (excise!- @api/impl id-or-e))

(defn q
  "Raw datalog query."
  [query & args]
  (apply datomic/q query (common-api/datomic-db @api/impl) args))

(defn find-datalog
  "Run a datalog query (for full entities) returning the results as entities on default instance."
  [query & args]
  (q->entities @api/impl (apply q query args)))

(def squuid datomic/squuid)
