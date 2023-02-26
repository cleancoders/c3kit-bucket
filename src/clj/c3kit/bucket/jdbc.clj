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
  (:import (java.sql ResultSet)))

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

(defmulti schema->db-type-map identity)

(defn schema-type->db-type [dialect type]
  (get (schema->db-type-map dialect) type))

(defn column-name
  ([[key spec]] (column-name key spec))
  ([key spec] (or (-> spec :db :column) (name key))))

(defn sql-table-col [dialect key spec]
  (let [type (:type spec)]
    (str "\"" (column-name key spec) "\" "
         (or (-> spec :db :type) (schema-type->db-type dialect type) (name type)))))

(defn sql-add-column [dialect schema attr]
  (str "ALTER TABLE " (table-name schema)
       " ADD COLUMN " (sql-table-col dialect attr (get schema attr))))

(defn sql-create-table [dialect schema]
  (str "CREATE TABLE " (table-name schema) " ("
       (str/join "," (map (fn [[key spec]] (sql-table-col dialect key spec)) (dissoc schema :kind)))
       ")"))

(defn add-column [dialect ds schema attr] (execute! ds [(sql-add-column dialect schema attr)]))

(defn create-table-from-schema [dialect ds schema]
  (let [sql (sql-create-table dialect schema)]
    (execute! ds [sql])))

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

(defn <-sql-value [type value]
  (case type
    :json (.getValue value)
    :date (time/->local value)
    value))

(defn ->sql-value [type value]
  (case type
    :date (schema/->sql-date value)
    :timestamp (schema/->timestamp value)
    :instant (schema/->timestamp value)
    value))

(defn ->sql-param [dialect type]
  (let [type-map (schema->db-type-map dialect)]
    (if-let [db-type (get type-map type)]
      (str "CAST(? AS " db-type ")")
      "?")))

(defn -add-type [result [key spec]] (assoc result key (spec->db-type spec)))
(defn- get-full-key-name [k]
  (if-let [ns (namespace k)] (str ns "." (name k)) (name k)))

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

(defn col->key-builder [{:keys [col->key key->type]} rs _opts]
  (let [rs-meta   (.getMetaData rs)
        col-range (range 1 (inc (if rs-meta (.getColumnCount rs-meta) 0)))
        cols      (mapv (fn [^Integer i]
                          (let [col-name (.getColumnLabel rs-meta i)]
                            (get col->key col-name))) col-range)]
    (->MapResultSetOptionalBuilder rs rs-meta cols key->type)))

(defn compile-mapping [schema]
  (let [k->c (reduce (fn [m [k s]] (assoc m k (or (-> s :db :column) (get-full-key-name k)))) {} (dissoc schema :kind))
        c->k (reduce (fn [m [k c]] (assoc m c k)) {} k->c)
        k->t (reduce -add-type {} (dissoc schema :kind))]
    {:table       (table-name schema)
     :id-strategy (get-in schema [:id :strategy] :db-generated)
     :key->col    k->c
     :col->key    c->k
     :key->type   k->t
     :builder-fn  (partial col->key-builder {:col->key c->k :key->type k->t})
     }))

(defn- key-map [mappings kind]
  (if-let [m (get @mappings kind)]
    m
    (let [schema (legend/for-kind kind)
          m      (compile-mapping schema)]
      (swap! mappings assoc kind m)
      m)))

(defn ->sql-args [dialect key->col key->type entity k]
  (let [v    (get entity k)
        type (get key->type k)]
    {:column (get key->col k)
     :param  (->sql-param dialect type)
     :value  (->sql-value type v)}))

(defn build-fetch-sql [dialect t-map id]
  (let [{:keys [table key->col key->type]} t-map
        column (:id key->col)
        type   (:id key->type)
        param  (->sql-param dialect type)
        value  (->sql-value type id)]
    [(str "SELECT * FROM " table " WHERE " column " = " param) value]))

(defn- fetch-entity [dialect ds t-map kind id]
  (when-let [id (if (map? id) (:id id) id)]
    (let [id      (api/coerced-id kind id)
          command (build-fetch-sql dialect t-map id)
          result  (execute-one! ds command {:builder-fn (:builder-fn t-map)})]
      (when result
        (assoc result :kind kind)))))

(defn build-update-sql [dialect t-map {:keys [id] :as entity}]
  (let [{:keys [table key->col key->type]} t-map
        ->sql-args (partial ->sql-args dialect key->col key->type entity)
        id-arg     (->sql-args :id)
        used-keys  (disj (set/intersection (set (keys entity)) (set (keys key->col))) :id)
        sql-args   (map ->sql-args used-keys)
        cols       (->> (map :column sql-args) (map #(str "\"" % "\"")))]
    (cons (str "UPDATE " table " "
               "SET " (str/join ", " (map #(str %1 " = " %2) cols (map :param sql-args))) " "
               "WHERE " (:column id-arg) " = " (:param id-arg))
          (concat (map :value sql-args) [(:value id-arg)]))))

(defn build-insert-sql [dialect t-map entity]
  (let [{:keys [table key->col key->type]} t-map
        used-key->col (select-keys key->col (keys entity))
        ->sql-args    (partial ->sql-args dialect used-key->col key->type entity)
        sql-args      (->> used-key->col keys (map ->sql-args))
        cols          (->> (map :column sql-args) (map #(str "\"" % "\"")))]
    (cons (str "INSERT INTO " table " (" (str/join ", " cols) ") "
               "VALUES (" (str/join ", " (map :param sql-args)) ")")
          (map :value sql-args))))

(defmulti build-upsert-sql (fn [dialect _t-map _entity] dialect))

(defn- build-delete-sql [dialect t-map entity]
  (let [{:keys [table key->col key->type]} t-map
        {:keys [column param value]} (->sql-args dialect key->col key->type entity :id)]
    [(str "DELETE FROM " table " WHERE " column " = " param) value]))

(defn- delete-entity [dialect ds t-map entity]
  (let [sql    (build-delete-sql dialect t-map entity)
        result (execute-one! ds sql)]
    (when-not (= 1 (:next.jdbc/update-count result)) (throw (ex-info "delete failed" {:entity entity :result result})))
    {:kind (:kind entity) :id (:id entity) :db/delete? true}))

(defn- update-entity [dialect ds t-map entity]
  (let [command (build-update-sql dialect t-map entity)]
    (execute-one! ds command)
    (fetch-entity dialect ds t-map (:kind entity) (:id entity))))

(defn- insert-entity [dialect ds t-map entity]
  (let [command (build-insert-sql dialect t-map entity)
        result  (execute-one! ds command {:return-keys true})
        id      (first (vals result))]
    (fetch-entity dialect ds t-map (:kind entity) id)))

(defn- upsert-entity [dialect ds t-map entity]
  (let [command (build-upsert-sql dialect t-map entity)]
    (execute-one! ds command)
    (fetch-entity dialect ds t-map (:kind entity) (:id entity))))

(defmulti upsert-by-id-strategy (fn [_dialect _ds t-map _entity] (:id-strategy t-map)))

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
      (delete-entity dialect ds t-map entity)
      (upsert-by-id-strategy dialect ds t-map entity))))

(defn- do-tx* [dialect ds mappings entities]
  (jdbc/with-transaction [tx ds]
    (doall (map #(do-tx dialect tx mappings %) entities))))

(defn- do-find-all [_dialect ds mappings kind]
  (let [t-map (key-map mappings kind)
        sql   (str "SELECT * FROM " (:table t-map))]
    (map #(ccc/remove-nils (assoc % :kind kind)) (execute! ds [sql] {:builder-fn (:builder-fn t-map)}))))

(defn clause-with-operator [dialect {:keys [key->type] :as t-map} k v operator]
  (let [type (get key->type k)]
    [(str (->field-name t-map k) " " operator " " (->sql-param dialect type))
     (->sql-value type v)]))

(defn- -equality-clause [dialect t-map k v]
  (if (nil? v)
    [(str (->field-name t-map k) " IS NULL")]
    (clause-with-operator dialect t-map k v "=")))

(defn- -build-parity-or-clause [not? dialect {:keys [key->type] :as t-map} k v]
  (let [type       (get key->type k)
        is-null?   (some nil? v)
        v          (->> v set (remove nil?))
        in?        (not (empty? v))
        field-name (->field-name t-map k)
        num-vals   (count v)]
    (cons (str "("
               (when in? (str field-name (when not? " NOT") " IN (" (str/join "," (repeat num-vals (->sql-param dialect type))) ")"))
               (when (and in? is-null?) (if not? " AND " " OR "))
               (when is-null? (str field-name " IS" (when not? " NOT") " NULL"))
               ")")
          (map (partial ->sql-value type) v))))

(def -build-or-clause (partial -build-parity-or-clause false))
(def -build-nor-clause (partial -build-parity-or-clause true))

(defn -clause [dialect t-map k v]
  (condp = (first v)
    '= (-build-or-clause dialect t-map k (rest v))
    'not= (-build-nor-clause dialect t-map k (rest v))
    '> (clause-with-operator dialect t-map k (second v) ">")
    '< (clause-with-operator dialect t-map k (second v) "<")
    '>= (clause-with-operator dialect t-map k (second v) ">=")
    '<= (clause-with-operator dialect t-map k (second v) "<=")
    'like (clause-with-operator dialect t-map k (second v) "LIKE")
    'ilike (clause-with-operator dialect t-map k (second v) (if (= :mssql dialect) "LIKE" "ILIKE"))
    (-build-or-clause dialect t-map k v)))

(defn -build-or-clause-by-column [->sql ks v]
  (let [clause-pairs       (map #(->sql [% v]) ks)
        clauses            (map first clause-pairs)
        interposed-clauses (str/join (interpose " OR " clauses))]
    (cons (str "(" interposed-clauses ")") (mapcat rest clause-pairs))))

(defn ->sql-clause [dialect t-map [k v]]
  (cond
    (set? k) (-build-or-clause-by-column (partial ->sql-clause dialect t-map) k v)
    (set? v) (-build-or-clause dialect t-map k v)
    (sequential? v) (-clause dialect t-map k v)
    :else (-equality-clause dialect t-map k v)))

(defn ->sql-clauses [dialect t-map kv-pairs]
  (map (partial ->sql-clause dialect t-map) kv-pairs))

(defn -build-where [dialect sql t-map kv-pairs]
  (if-let [sql-conditions (seq (->sql-clauses dialect t-map kv-pairs))]
    (cons (apply str sql " WHERE " (interpose " AND " (map first sql-conditions)))
          (mapcat rest sql-conditions))
    [sql]))

(defmulti -build-find-by-query (fn [dialect _t-map _params _entity] dialect))

(defmethod -build-find-by-query :default [dialect t-map {[limit] :keys :as params} kvs]
  (let [kv-pairs (partition 2 kvs)
        select   (str "SELECT * FROM " (:table t-map))
        [sql & args] (-build-where dialect select t-map kv-pairs)
        sql      (if limit (str sql " LIMIT " limit) sql)]
    (assert (every? keyword? (map first kv-pairs)) "Attributes must be keywords")
    (cons sql args)))

(defn- do-find-by
  ([dialect ds mappings kind kvs] (do-find-by dialect ds mappings {} kind kvs))
  ([dialect ds mappings params kind kvs]
   (let [t-map (key-map mappings kind)
         query (-build-find-by-query dialect t-map params kvs)]
     (->> (execute! ds query {:builder-fn (:builder-fn t-map)})
          (map #(ccc/remove-nils (assoc % :kind kind)))))))

(defn- do-ffind-by [dialect ds mappings kind & kvs]
  (first (apply do-find-by dialect ds mappings {:limit 1} kind kvs)))

(defn- do-reduce-by [_dialect ds mappings kind f val kvs]
  (let [t-map      (key-map mappings kind)
        query      (-build-find-by-query ds t-map {} kvs)
        connection (jdbc/get-connection ds)]
    (.setHoldability connection ResultSet/CLOSE_CURSORS_AT_COMMIT)
    (reduce
      (fn [a b] (f a (ccc/remove-nils (assoc b :kind kind))))
      val
      (jdbc/plan connection query {:builder-fn  (:builder-fn t-map)
                                   :fetch-size  1000
                                   :concurrency :read-only
                                   :cursors     :close
                                   :result-type :forward-only}))))

(defn- do-count-by [dialect ds mappings kind kvs]
  (let [kv-pairs (partition 2 kvs)
        {:keys [table] :as t-map} (key-map mappings kind)
        query    (-build-where dialect (str "SELECT COUNT(*) FROM " table) t-map kv-pairs)]
    (first (vals (execute-one! ds query)))))

(defn- do-count [_dialect ds mappings kind]
  (let [t-map (key-map mappings kind)
        sql   (str "SELECT COUNT(*) FROM " (:table t-map))]
    (first (vals (execute-one! ds [sql])))))

(defn- do-clear [dialect ds]
  (assert development? "Refuse to clear non-development database")
  (doseq [[_ schema] legend/index]
    (drop-table-from-schema ds schema)
    (create-table-from-schema dialect ds schema)))

(defn- do-delete-all [_dialect ds kind mappings]
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

