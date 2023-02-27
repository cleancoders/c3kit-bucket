(ns c3kit.bucket.datomic
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [datomic.api :as datomic]))

(def development? (atom false))

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

(defn connect [uri]
  (datomic/create-database uri)
  (datomic/connect uri))

(defn transact! [connection transaction]
  (datomic/transact connection transaction))

(defn do-install-schema [schemas legend new-schemas]
  (swap! schemas (fn [existing]
                   (let [new-schemas (remove #(ccc/index-of % existing) new-schemas)]
                     (concat existing new-schemas))))
  (swap! legend merge (legend/build new-schemas)))

(defn do-clear [uri legend conn]
  (assert @development? "Refuse to clear non-development database")
  (datomic/delete-database uri)
  (reset! conn (connect uri))
  #_(let [schema (mapcat ->db-schema schemas)]
    @(transact! @conn schema)))

(deftype DatomicDB [schemas legend uri conn]
  api/DB
  (-install-schema [_ new-schemas] (do-install-schema schemas legend new-schemas))
  (-clear [_] (do-clear uri legend conn))
  ;(-delete-all [_ kind] (do-delete-all dialect ds kind mappings))
  ;(-count-all [_ kind] (do-count dialect ds mappings kind))
  ;(-count-by [_ kind kvs] (do-count-by dialect ds mappings kind kvs))
  ;(-entity [_ kind id] (fetch-entity dialect ds (key-map mappings kind) kind id))
  ;(-find-all [_ kind] (do-find-all dialect ds mappings kind))
  ;(-find-by [_ kind kvs] (do-find-by dialect ds mappings kind kvs))
  ;(-ffind-by [_ kind kvs] (do-ffind-by dialect ds mappings kind kvs))
  ;(-reduce-by [_ kind f val kvs] (do-reduce-by dialect ds mappings kind f val kvs))
  ;(-tx [_ entity] (do-tx dialect ds mappings entity))
  ;(-tx* [_ entities] (do-tx* dialect ds mappings entities))
  )

(defn create-db
  ([uri] (create-db uri []))
  ([uri schemas] (DatomicDB. (atom schemas) (atom (legend/build schemas)) uri (atom (connect uri)))))
