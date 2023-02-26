(ns c3kit.bucket.jdbc
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.apron.log :as log]
            [c3kit.apron.schema :as schema]
            [c3kit.apron.time :as time]
            [c3kit.bucket.api :as api]
            [clojure.set :as set]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import (java.sql ResultSet SQLDataException)))

(def development? (atom false))

(defn execute-one!
  ([ds command] (execute-one! ds command {}))
  ([ds command options]
   (try
     (jdbc/execute-one! ds command options)
     (catch Exception e
       (log/error "Choked on SQL: " command)
       (throw e)))))

(defn execute!
  ([ds command] (execute! ds command {}))
  ([ds command options]
   (try
     (jdbc/execute! ds command options)
     (catch Exception e
       (log/error "Choked on SQL: " command)
       (throw e)))))

(defn table-name [schema] (or (-> schema :kind :db :table) (-> schema :kind :value name)))

(defn- drop-table-from-schema [ds schema]
  (execute! ds [(str "DROP TABLE IF EXISTS " (table-name schema))]))

;; Postgres
;(def schema-type->db-type {:int     "int4"
;                           :long    "int4"
;                           :boolean "bool"})
;; MSSQL
(def schema-type->db-type {:long      "bigint"
                           :int       "int"
                           :uuid      "uniqueidentifier"
                           :instant   "datetime2"
                           :timestamp "datetime2"
                           :boolean   "bit"})

(defn column-name
  ([[key spec]] (column-name key spec))
  ([key spec] (or (-> spec :db :column) (name key))))

(defn sql-table-col [key spec]
  (let [type (:type spec)]
    (str "\"" (column-name key spec) "\" "
         (or (-> spec :db :type) (schema-type->db-type type) (name type)))))

(defn sql-add-column [schema attr]
  (str "ALTER TABLE " (table-name schema)
       " ADD COLUMN " (sql-table-col attr (get schema attr))))

(defn sql-create-table [schema]
  (str "CREATE TABLE " (table-name schema) " ("
       (str/join "," (map (fn [[key spec]] (sql-table-col key spec)) (dissoc schema :kind)))
       ")"))

(defn add-column [ds schema attr] (execute! ds [(sql-add-column schema attr)]))

(defn create-table-from-schema [ds schema]
  (let [sql (sql-create-table schema)]
    (execute! ds [sql])))

(defn- select
  ([table] (select "*" table))
  ([what table] (str "SELECT " what " FROM " table)))

(def ^:private count-all (partial select "COUNT(*)"))

(defn- ->field-name [{:keys [table key->col]} k]
  (if (namespace k)
    (get key->col k)
    (str table "." (get key->col k))))

(defn spec->db-type [spec]
  (let [type    (:type spec)
        db-type (get-in spec [:db :type])]
    (if (and (= :string type) db-type (str/starts-with? db-type "json"))
      :json
      type)))

(defmulti <-sql-value (fn [type _] type))
(defmethod <-sql-value :json [_ value] (.getValue value))
(defmethod <-sql-value :date [_ value] (time/->local value)) ;; Dates come out of the db with TZ offset added. Remove it.
(defmethod <-sql-value :default [_ value] value)

(defmulti ->sql-value (fn [type _] type))
(defmethod ->sql-value :default [_ value] value)
(defmethod ->sql-value :date [_ value] (schema/->sql-date value))
(defmethod ->sql-value :timestamp [_ value] (schema/->timestamp value))
(defmethod ->sql-value :instant [_ value] (schema/->timestamp value))

(defmulti ->sql-param identity)
(defmethod ->sql-param :default [_] "?")
(defmethod ->sql-param :uuid [_] "CAST(? AS uuid)")
(defmethod ->sql-param :json [_] "CAST(? AS jsonb)")
(defmethod ->sql-param :date [_] "CAST(? AS date)")
(defmethod ->sql-param :timestamp [_] "CAST(? AS datetime2)")
(defmethod ->sql-param :instant [_] "CAST(? AS datetime2)")


(defn -add-type [result [key spec]] (assoc result key (spec->db-type spec)))
(defn- get-full-key-name [k]
  (if-let [ns (namespace k)] (str ns "." (name k)) (name k)))

(defn compile-mapping [schema]
  (let [k->c (reduce (fn [m [k s]] (assoc m k (or (-> s :db :column) (get-full-key-name k)))) {} (dissoc schema :kind))
        c->k (reduce (fn [m [k c]] (assoc m c k)) {} k->c)]
    {:table       (table-name schema)
     :id-strategy (get-in schema [:id :strategy] :db-generated)
     :key->col    k->c
     :col->key    c->k
     :key->type   (reduce -add-type {} (dissoc schema :kind))
     }))

(defn- key-map [mappings kind]
  (if-let [m (get @mappings kind)]
    m
    (let [schema (legend/for-kind kind)
          m      (compile-mapping schema)]
      (swap! mappings assoc kind m)
      m)))

(defrecord MapResultSetOptionalBuilder [^ResultSet rs rs-meta cols key->type]
  rs/RowBuilder
  (->row [_] (transient {}))
  (column-count [_] (count cols))
  (with-column [_ row i]
    (let [v (.getObject rs ^Integer i)]
      (if (nil? v)
        row
        (rs/with-column-value _ row (nth cols (dec i)) (rs/read-column-by-index v rs-meta i)))))
  (with-column-value [_ row col v]
    (if (nil? v)
      row
      (let [v (<-sql-value (key->type col) v)]
        (assoc! row col v))))
  (row! [_ row] (persistent! row))
  rs/ResultSetBuilder
  (->rs [_] (transient []))
  (with-row [_ mrs row] (conj! mrs row))
  (rs! [_ mrs] (persistent! mrs)))

(defn- col->key-builder [{:keys [col->key key->type]} rs _opts]
  (let [rs-meta   (.getMetaData rs)
        col-range (range 1 (inc (if rs-meta (.getColumnCount rs-meta) 0)))
        cols      (mapv (fn [^Integer i]
                          (let [col-name (.getColumnLabel rs-meta i)]
                            (get col->key col-name))) col-range)]
    (->MapResultSetOptionalBuilder rs rs-meta cols key->type)))

(defn ->sql-args [key->col key->type entity k]
  (let [v    (get entity k)
        type (get key->type k)]
    {:column (get key->col k)
     :param  (->sql-param type)
     :value  (->sql-value type v)}))

(defn build-fetch-sql [t-map id]
  (let [{:keys [table key->col key->type]} t-map
        column (:id key->col)
        type   (:id key->type)
        param  (->sql-param type)
        value  (->sql-value type id)]
    [(str (select table) " WHERE " column " = " param) value]))

(defn- fetch-entity [dialect ds t-map kind id]
  (when-let [id (if (map? id) (:id id) id)]
    (try
      (let [command (build-fetch-sql t-map id)
            result  (execute-one! ds command {:builder-fn (partial col->key-builder t-map)})]
        (when result
          (assoc result :kind kind)))
      (catch SQLDataException e
        (log/warn "fetch-entity failed" e)
        nil))))

(defn build-update-sql [t-map {:keys [id] :as entity}]
  (let [{:keys [table key->col key->type]} t-map
        entity     (api/-update-timestamps key->col entity)
        ->sql-args (partial ->sql-args key->col key->type entity)
        id-arg     (->sql-args :id)
        used-keys  (disj (set/intersection (set (keys entity)) (set (keys key->col))) :id)
        sql-args   (map ->sql-args used-keys)
        cols       (->> (map :column sql-args) (map #(str "\"" % "\"")))]
    (cons (str "UPDATE " table " "
               "SET " (str/join ", " (map #(str %1 " = " %2) cols (map :param sql-args))) " "
               "WHERE " (:column id-arg) " = " (:param id-arg))
          (concat (map :value sql-args) [(:value id-arg)]))))

(defn build-insert-sql [t-map entity]
  (let [{:keys [table key->col key->type]} t-map
        entity        (api/-update-timestamps key->col entity)
        used-key->col (select-keys key->col (keys entity))
        ->sql-args    (partial ->sql-args used-key->col key->type entity)
        sql-args      (->> used-key->col keys (map ->sql-args))
        cols          (->> (map :column sql-args) (map #(str "\"" % "\"")))]
    (cons (str "INSERT INTO " table " (" (str/join ", " cols) ") "
               ;"OUTPUT Inserted." (:id key->col) " "
               "VALUES (" (str/join ", " (map :param sql-args)) ")")
          (map :value sql-args))))

(defmulti build-upsert-sql (fn [dialect _t-map _entity] dialect))
(defmethod build-upsert-sql :default [_dialect t-map {:keys [id] :as entity}]
  (let [[fetch-sql & fetch-params] (build-fetch-sql t-map id)
        [insert-sql & insert-params] (build-insert-sql t-map entity)
        [update-sql & update-params] (build-update-sql t-map entity)]
    (cons (str "IF NOT EXISTS (" fetch-sql ") " insert-sql " ELSE " update-sql)
          (concat fetch-params insert-params update-params))))

(defmethod build-upsert-sql :h2 [_dialect t-map {:keys [id] :as entity}]
  (let [{:keys [table key->col key->type]} t-map
        entity        (api/-update-timestamps key->col entity)
        used-key->col (select-keys key->col (keys entity))
        ->sql-args    (partial ->sql-args used-key->col key->type entity)
        sql-args      (->> used-key->col keys (map ->sql-args))
        cols          (->> (map :column sql-args) (map #(str "\"" % "\"")))]
    (cons (str "MERGE INTO " table " (" (str/join ", " cols) ") "
               ;"OUTPUT Inserted." (:id key->col) " "
               "VALUES (" (str/join ", " (map :param sql-args)) ")")
          (map :value sql-args))))

;(defmethod build-upsert-sql :h2 [_ds t-map {:keys [id] :as entity}]
;  (let [{:keys [table key->col key->type]} t-map
;        id-col   (:id key->col)
;        entity   (api/-update-timestamps key->col entity)
;        key->col (select-keys key->col (keys entity))
;        sql-args (->> (if id key->col (dissoc key->col :id))
;                      keys
;                      (map (partial ->sql-args key->col key->type entity)))
;        cols     (map :column sql-args)
;        cols     (map #(str "\"" % "\"") cols)]
;    (cons (str "INSERT INTO " table " (" (str/join ", " cols) ") "
;               "VALUES (" (str/join ", " (map :param sql-args)) ") "
;               "ON CONFLICT (" id-col ") DO UPDATE SET "
;               (str/join ", " (map #(str % " = excluded." %) cols)) " "
;               "RETURNING " id-col)
;          (map :value sql-args))))

(defn- build-delete-sql [t-map entity]
  (let [{:keys [table key->col key->type]} t-map
        {:keys [column param value]} (->sql-args key->col key->type entity :id)]
    [(str "DELETE FROM " table " WHERE " column " = " param) value]))

(defn- delete-entity [ds t-map entity]
  (let [sql    (build-delete-sql t-map entity)
        result (execute-one! ds sql)]
    (when-not (= 1 (:next.jdbc/update-count result)) (throw (ex-info "delete failed" {:entity entity :result result})))
    {:kind (:kind entity) :id (:id entity) :db/delete? true}))

(defn- update-entity [dialect ds t-map entity]
  (let [command (build-update-sql t-map entity)]
    (execute-one! ds command)
    (fetch-entity dialect ds t-map (:kind entity) (:id entity))))

(defn- insert-entity [dialect ds t-map entity]
  (let [command (build-insert-sql t-map entity)
        result  (execute-one! ds command {:return-keys true})
        id      (first (vals result))]
    (fetch-entity dialect ds t-map (:kind entity) id)))

(defn- upsert-entity [dialect ds t-map entity]
  (let [command (build-upsert-sql dialect t-map entity)]
    (execute-one! ds command)
    (fetch-entity dialect ds t-map (:kind entity) (:id entity))))

(defmulti upsert-by-id-strategy (fn [dialect _ds t-map _entity] (:id-strategy t-map)))

(defmethod upsert-by-id-strategy :default [dialect _ds t-map {:keys [kind] :as _entity}]
  (throw (ex-info "Unhandled id strategy" {:kind kind :id-strategy (:id-strategy t-map)})))

(defmethod upsert-by-id-strategy :db-generated [dialect ds t-map entity]
  (if (:id entity)
    (update-entity dialect ds t-map entity)
    (insert-entity dialect ds t-map entity)))

(defmethod upsert-by-id-strategy :pre-populated [dialect ds t-map entity]
  (upsert-entity dialect ds t-map entity))

(defn- do-tx [dialect ds mappings entity]
  (let [t-map (key-map mappings (:kind entity))]
    (if (api/delete? entity)
      (delete-entity ds t-map entity)
      (upsert-by-id-strategy dialect ds t-map entity))))

(defn- do-tx* [dialect ds mappings entities]
  (jdbc/with-transaction [tx ds]
    (doall (map #(do-tx dialect tx mappings %) entities))))

(defn- do-find-all [dialect ds mappings kind]
  (let [t-map (key-map mappings kind)
        sql   (select (:table t-map))]
    (map #(ccc/remove-nils (assoc % :kind kind)) (execute! ds [sql] {:builder-fn (partial col->key-builder t-map)}))))

(defn clause-with-operator [{:keys [key->type] :as t-map} k v operator]
  (let [type (get key->type k)]
    [(str (->field-name t-map k) " " operator " " (->sql-param type))
     (->sql-value type v)]))

(defn- -equality-clause [t-map k v]
  (if (nil? v)
    [(str (->field-name t-map k) " IS NULL")]
    (clause-with-operator t-map k v "=")))

(defn- -build-parity-or-clause [not? {:keys [key->type] :as t-map} k v]
  (let [type       (get key->type k)
        is-null?   (some nil? v)
        v          (->> v set (remove nil?))
        in?        (not (empty? v))
        field-name (->field-name t-map k)
        num-vals   (count v)]
    (cons (str "("
               (when in? (str field-name (when not? " NOT") " IN (" (str/join "," (repeat num-vals (->sql-param type))) ")"))
               (when (and in? is-null?) (if not? " AND " " OR "))
               (when is-null? (str field-name " IS" (when not? " NOT") " NULL"))
               ")")
          (map (partial ->sql-value type) v))))

(def -build-or-clause (partial -build-parity-or-clause false))
(def -build-nor-clause (partial -build-parity-or-clause true))

(defn -clause [dialect t-map k v]
  (condp = (first v)
    '= (-build-or-clause t-map k (rest v))
    'not= (-build-nor-clause t-map k (rest v))
    '> (clause-with-operator t-map k (second v) ">")
    '< (clause-with-operator t-map k (second v) "<")
    '>= (clause-with-operator t-map k (second v) ">=")
    '<= (clause-with-operator t-map k (second v) "<=")
    'like (clause-with-operator t-map k (second v) "LIKE")
    'ilike (clause-with-operator t-map k (second v) (if (= :mssql dialect) "LIKE" "ILIKE"))
    (-build-or-clause t-map k v)))


(defn -build-or-clause-by-column [->sql ks v]
  (let [clause-pairs       (map #(->sql [% v]) ks)
        clauses            (map first clause-pairs)
        interposed-clauses (str/join (interpose " OR " clauses))]
    (cons (str "(" interposed-clauses ")") (mapcat rest clause-pairs))))

(defn ->sql-clause [dialect t-map [k v]]
  (cond
    (set? k) (-build-or-clause-by-column (partial ->sql-clause dialect t-map) k v)
    (set? v) (-build-or-clause t-map k v)
    (sequential? v) (-clause dialect t-map k v)
    :else (-equality-clause t-map k v)))

(defn ->sql-clauses [dialect t-map kv-pairs]
  (map (partial ->sql-clause dialect t-map) kv-pairs))

(defn -build-find-by-sql [dialect sql t-map kv-pairs]
  (if-let [sql-conditions (seq (->sql-clauses dialect t-map kv-pairs))]
    (cons (apply str sql " WHERE " (interpose " AND " (map first sql-conditions)))
          (mapcat rest sql-conditions))
    [sql]))

(defmulti -build-find-by-query (fn [dialect _t-map _params _entity] dialect))
(defmethod -build-find-by-query :default [dialect t-map {[limit] :keys :as params} kvs]
  (let [kv-pairs (partition 2 kvs)
        {:keys [table] :as t-map} t-map
        select   (if limit (select (str "TOP " limit " *") table) (select table))
        [sql & args] (-build-find-by-sql dialect select t-map kv-pairs)
        ;sql      (if limit (str sql " LIMIT " limit) sql)
        ]
    (assert (every? keyword? (map first kv-pairs)) "Attributes must be keywords")
    {:query      (cons sql args)
     :builder-fn (partial col->key-builder t-map)}))

(defmethod -build-find-by-query :h2 [dialect t-map {[limit] :keys :as params} kvs]
  (let [kv-pairs (partition 2 kvs)
        {:keys [table] :as t-map} t-map
        select   (select table)
        [sql & args] (-build-find-by-sql dialect select t-map kv-pairs)
        sql      (if limit (str sql " LIMIT " limit) sql)
        ]
    (assert (every? keyword? (map first kv-pairs)) "Attributes must be keywords")
    {:query      (cons sql args)
     :builder-fn (partial col->key-builder t-map)}))

(defn- do-find-by
  ([dialect ds mappings kind kvs] (do-find-by dialect ds mappings {} kind kvs))
  ([dialect ds mappings params kind kvs]
   ;(let [{:keys [query builder-fn]} (->find-by-args mappings params kind kvs)]
   (let [{:keys [query builder-fn]} (-build-find-by-query ds (key-map mappings kind) params kvs)]
     (->> (execute! ds query {:builder-fn builder-fn})
          (map #(ccc/remove-nils (assoc % :kind kind)))))))


(defn- do-ffind-by [dialect ds mappings kind & kvs]
  (first (apply do-find-by dialect ds mappings {:limit 1} kind kvs)))

(defn- do-reduce-by [dialect ds mappings kind f val kvs]
  (let [{:keys [query builder-fn]} (-build-find-by-query ds (key-map mappings kind) {} kvs)
        connection (jdbc/get-connection ds)]
    (.setHoldability connection ResultSet/CLOSE_CURSORS_AT_COMMIT)
    (reduce
      (fn [a b] (f a (ccc/remove-nils (assoc b :kind kind))))
      val
      (jdbc/plan connection query {:builder-fn  builder-fn
                                   :fetch-size  1000
                                   :concurrency :read-only
                                   :cursors     :close
                                   :result-type :forward-only}))))

(defn- do-count-by [dialect ds mappings kind kvs]
  (let [kv-pairs (partition 2 kvs)
        {:keys [table] :as t-map} (key-map mappings kind)
        query    (-build-find-by-sql dialect (count-all table) t-map kv-pairs)]
    ;(:count (jdbc/execute-one! ds query))
    (first (vals (execute-one! ds query)))))

(defn- do-count [dialect ds mappings kind]
  (let [sql (count-all (:table (key-map mappings kind)))]
    ;(:count (execute-one! ds [sql]))
    (first (vals (execute-one! ds [sql])))))

(defn- do-clear [_dialect ds]
  (assert development? "Refuse to clear non-development database")
  (doseq [[_ schema] legend/index]
    (drop-table-from-schema ds schema)
    (create-table-from-schema ds schema)))

(defn- do-delete-all [dialect ds kind mappings]
  (assert development? "Refuse to delete-all on non-development database")
  (let [{:keys [table]} (key-map mappings kind)
        sql (str "DELETE FROM " table " WHERE 1 = 1")]
    (execute-one! ds [sql])))

(deftype JDBCDB [dialect ds mappings]
  api/DB
  (-clear [_] (do-clear dialect ds))
  (-delete-all [_ kind] (do-delete-all dialect ds kind mappings))
  (-count-all [_ kind] (do-count dialect ds mappings kind))
  (-count-by [_ kind kvs] (do-count-by dialect ds mappings kind kvs))
  (-entity [_ kind id] (fetch-entity dialect ds (key-map mappings kind) kind id))
  (-find-all [_ kind] (do-find-all dialect ds mappings kind))
  (-find-by [_ kind kvs] (do-find-by dialect ds mappings kind kvs))
  (-ffind-by [_ kind kvs] (do-ffind-by dialect ds mappings kind kvs))
  (-reduce-by [_ kind f val kvs] (do-reduce-by dialect ds mappings kind f val kvs))
  (-tx [_ entity] (do-tx dialect ds mappings entity))
  (-tx* [_ entities] (do-tx* dialect ds mappings entities))
  )

(defn create-db [config]
  (let [dialect (:dialect config)
        ds      (jdbc/get-datasource config)]
    (JDBCDB. dialect ds (atom {}))))

