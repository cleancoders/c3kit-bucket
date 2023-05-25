(ns c3kit.bucket.jdbc
  (:refer-clojure :rename {find core-file count core-count reduce core-reduce})
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

(defn execute-one-conn!
  ([conn command] (execute-one-conn! conn command {}))
  ([conn command options]
   (try
     (jdbc/execute-one! conn command options)
     (catch Exception e
       (log/error "Choked on SQL: " command)
       (throw e)))))

(defn- execute-conn!
  ([conn command] (execute-conn! conn command {}))
  ([conn command options]
   (try
     (log/debug "executing query:" command)
     (jdbc/execute! conn command options)
     (catch Exception e
       (prn "Choked on SQL: " command)
       (log/error "Choked on SQL: " command)
       (throw e)))))

(defn- maybe-str->command [command] (if (string? command) [command] command))

(defn execute-one!
  "Execute SQL returning 1 raw result"
  ([db command] (execute-one-conn! (.-ds db) (maybe-str->command command) {}))
  ([db command options] (execute-one-conn! (.-ds db) (maybe-str->command command) options)))

(defn execute!
  "Execute SQL returning all results."
  ([db command] (execute-conn! (.-ds db) (maybe-str->command command) {}))
  ([db command options] (execute-conn! (.-ds db) (maybe-str->command command) options)))

(defn table-name [schema] (or (-> schema :kind :db :table) (-> schema :kind :value name)))

(defn drop-table [db table-name]
  (execute-conn! (.-ds db) [(str "DROP TABLE IF EXISTS " table-name)]))

(defn- drop-table-from-schema [db schema]
  (drop-table db (table-name schema)))

(defmulti schema->db-type-map identity)

(defmethod schema->db-type-map :default [_]
  {:int     "int4"
   :long    "int4"
   :boolean "bool"
   :instant "timestamp without time zone"})

(defn dialect [db] (.-dialect db))

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

(defn add-column [db schema attr]
  (execute-conn! (.-ds db) [(sql-add-column (.-dialect db) schema attr)]))

(defn create-table-from-schema [db schema]
  (let [dialect (.-dialect db)
        ds (.-ds db)
        sql (sql-create-table dialect schema)]
    (execute-conn! ds [sql])))

(defn- ->field-name [{:keys [table key->col]} k]
  (if (namespace k)
    (get key->col k)
    (str table "." (get key->col k))))

(defn spec->db-type [spec]
  (let [type (:type spec)
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
  (column-count [_] (core-count cols))
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
  (let [rs-meta (.getMetaData rs)
        col-range (range 1 (inc (if rs-meta (.getColumnCount rs-meta) 0)))
        cols (mapv (fn [^Integer i]
                     (let [col-name (.getColumnLabel rs-meta i)]
                       (get col->key col-name))) col-range)]
    (->MapResultSetOptionalBuilder rs rs-meta cols key->type)))

(defn compile-mapping [schema]
  (let [k->c (core-reduce (fn [m [k s]] (assoc m k (or (-> s :db :column) (get-full-key-name k)))) {} (dissoc schema :kind))
        c->k (core-reduce (fn [m [k c]] (assoc m c k)) {} k->c)
        k->t (core-reduce -add-type {} (dissoc schema :kind))]
    {:table       (table-name schema)
     :id-type     (get-in schema [:id :type])
     :id-strategy (get-in schema [:id :strategy] :db-generated)
     :key->col    k->c
     :col->key    c->k
     :key->type   k->t
     :builder-fn  (partial col->key-builder {:col->key c->k :key->type k->t})
     }))

(defn- key-map
  ([db kind] (key-map (.-legend db) (.-mappings db) kind))
  ([legend mappings kind]
   (if-let [m (get @mappings kind)]
     m
     (let [schema (legend/for-kind legend kind)
           m (compile-mapping schema)]
       (swap! mappings assoc kind m)
       m))))

(defn ->sql-args [dialect key->col key->type entity k]
  (let [v (get entity k)
        type (get key->type k)]
    {:column (get key->col k)
     :param  (->sql-param dialect type)
     :value  (->sql-value type v)}))

(defn build-fetch-sql [dialect t-map id]
  (let [{:keys [table key->col key->type]} t-map
        column (:id key->col)
        type (:id key->type)
        param (->sql-param dialect type)
        value (->sql-value type id)]
    [(str "SELECT * FROM " table " WHERE " column " = " param) value]))

(defn- fetch-entity [db conn t-map kind id]
  (when-let [id (if (map? id) (:id id) id)]
    (let [{:keys [id-type builder-fn]} t-map
          id (api/-coerced-id id-type id)
          command (build-fetch-sql (.-dialect db) t-map id)
          result (execute-one-conn! conn command {:builder-fn builder-fn})]
      (when result
        (assoc result :kind kind)))))

(defn entity [db kind id]
  (if (nil? kind)
    (throw (UnsupportedOperationException. "JDBC/entity requires a kind"))
    (fetch-entity db (.-ds db) (key-map db kind) kind id)))

(defn build-update-sql [dialect t-map entity]
  (let [{:keys [table key->col key->type]} t-map
        ->sql-args (partial ->sql-args dialect key->col key->type entity)
        id-arg (->sql-args :id)
        used-keys (disj (set/intersection (set (keys entity)) (set (keys key->col))) :id)
        sql-args (map ->sql-args used-keys)
        cols (->> (map :column sql-args) (map #(str "\"" % "\"")))]
    (cons (str "UPDATE " table " "
               "SET " (str/join ", " (map #(str %1 " = " %2) cols (map :param sql-args))) " "
               "WHERE " (:column id-arg) " = " (:param id-arg))
          (concat (map :value sql-args) [(:value id-arg)]))))

(defn build-insert-sql [dialect t-map entity]
  (let [{:keys [table key->col key->type]} t-map
        used-key->col (select-keys key->col (keys entity))
        ->sql-args (partial ->sql-args dialect used-key->col key->type entity)
        sql-args (->> used-key->col keys (map ->sql-args))
        cols (->> (map :column sql-args) (map #(str "\"" % "\"")))]
    (cons (str "INSERT INTO " table " (" (str/join ", " cols) ") "
               "VALUES (" (str/join ", " (map :param sql-args)) ")")
          (map :value sql-args))))

(defmulti build-upsert-sql (fn [dialect _t-map _entity] dialect))

(defn- build-delete-sql [dialect t-map entity]
  (let [{:keys [table key->col key->type]} t-map
        {:keys [column param value]} (->sql-args dialect key->col key->type entity :id)]
    [(str "DELETE FROM " table " WHERE " column " = " param) value]))

(defn- delete-entity [dialect conn t-map entity]
  (let [sql (build-delete-sql dialect t-map entity)
        result (execute-one-conn! conn sql)]
    (when-not (= 1 (:next.jdbc/update-count result)) (throw (ex-info "delete failed" {:entity entity :result result})))
    (api/soft-delete entity)))

(defn- update-entity [db conn t-map entity]
  (let [command (build-update-sql (.-dialect db) t-map entity)]
    (execute-one-conn! conn command)
    (fetch-entity db conn t-map (:kind entity) (:id entity))))

(defn- insert-entity [db conn t-map entity]
  (let [command (build-insert-sql (.-dialect db) t-map entity)
        result (execute-one-conn! conn command {:return-keys true})
        id (first (vals result))]
    (fetch-entity db conn t-map (:kind entity) id)))

(defn- upsert-entity [db conn t-map entity]
  (let [command (build-upsert-sql (.-dialect db) t-map entity)]
    (execute-one-conn! conn command)
    (fetch-entity db conn t-map (:kind entity) (:id entity))))

(defmulti upsert-by-id-strategy (fn [_db _conn t-map _entity] (:id-strategy t-map)))

(defmethod upsert-by-id-strategy :default [_db _conn t-map {:keys [kind] :as _entity}]
  (throw (ex-info "Unhandled id strategy" {:kind kind :id-strategy (:id-strategy t-map)})))

(defmethod upsert-by-id-strategy :db-generated [db conn t-map entity]
  (if (:id entity)
    (update-entity db conn t-map entity)
    (insert-entity db conn t-map entity)))

(defmethod upsert-by-id-strategy :pre-populated [db conn t-map entity]
  (upsert-entity db conn t-map entity))

(defn- do-tx [db conn entity]
  (let [t-map (key-map db (:kind entity))]
    (if (api/delete? entity)
      (delete-entity (.-dialect db) conn t-map entity)
      (upsert-by-id-strategy db conn t-map entity))))

(defn tx [db entity] (do-tx db (.-ds db) entity))

(defn tx* [db entities]
  (jdbc/with-transaction [tx (.-ds db)]
                         (doall (map #(do-tx db tx %) entities))))

(defn clause-with-operator [dialect {:keys [key->type] :as t-map} k v operator]
  (let [type (get key->type k)]
    [(str (->field-name t-map k) " " operator " " (->sql-param dialect type))
     (->sql-value type v)]))

(defn- -equality-clause [dialect t-map k v]
  (if (nil? v)
    [(str (->field-name t-map k) " IS NULL")]
    (clause-with-operator dialect t-map k v "=")))

(defn- -build-parity-or-clause [not? dialect {:keys [key->type] :as t-map} k v]
  (let [type (get key->type k)
        is-null? (some nil? v)
        v (->> v set (remove nil?))
        in? (not (empty? v))
        field-name (->field-name t-map k)
        num-vals (core-count v)]
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
  (let [clause-pairs (map #(->sql [% v]) ks)
        clauses (map first clause-pairs)
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

(defn -seq->sql [& sql-bits]
  (->> (flatten sql-bits)
       (remove nil?)
       (str/join " ")))

(defn -build-where [dialect t-map kv-pairs]
  (if-let [sql-conditions (seq (->sql-clauses dialect t-map kv-pairs))]
    (cons (str/join " " (cons "WHERE" (interpose "AND" (map first sql-conditions))))
          (mapcat rest sql-conditions))
    [""]))

(defmulti -build-find-query (fn [dialect _t-map _options] dialect))
(defmethod -build-find-query :default [dialect t-map {:keys [where take drop]}]
  (let [[where-sql & args] (-build-where dialect t-map where)
        sql (-seq->sql "SELECT * FROM" (:table t-map)
                       where-sql
                       (when take (str "LIMIT " take))
                       (when drop (str "OFFSET " drop)))]
    (cons sql args)))

(defn- do-find [db kind options]
  (let [t-map (key-map db kind)
        query (-build-find-query (.-dialect db) t-map options)]
    (->> (execute-conn! (.-ds db) query {:builder-fn (:builder-fn t-map)})
         (map #(ccc/remove-nils (assoc % :kind kind))))))

(defn- do-count [db kind {:keys [where] :as _options}]
  (let [{:keys [table] :as t-map} (key-map db kind)
        [where-sql & args] (-build-where (.-dialect db) t-map where)
        sql (str/join " " ["SELECT COUNT(*) FROM" table where-sql])]
    (first (vals (execute-one-conn! (.-ds db) (cons sql args))))))

(defn reduce [db kind f init options]
  (let [t-map (key-map db kind)
        query (-build-find-query (.-ds db) t-map options)
        connection (jdbc/get-connection (.-ds db))]
    (.setHoldability connection ResultSet/CLOSE_CURSORS_AT_COMMIT)
    (core-reduce
      (fn [a b] (f a (ccc/remove-nils (assoc b :kind kind))))
      init
      (jdbc/plan connection query {:builder-fn  (:builder-fn t-map)
                                   :fetch-size  1000
                                   :concurrency :read-only
                                   :cursors     :close
                                   :result-type :forward-only}))))

(defn clear [db]
  (api/-assert-safety-off! "clear")
  (doseq [[_ schema] (.-legend db)]
    (drop-table-from-schema db schema)
    (create-table-from-schema db schema)))

(defn delete-all [db kind]
  (api/-assert-safety-off! "delete")
  (let [{:keys [table]} (key-map db kind)
        sql (str "DELETE FROM " table " WHERE 1 = 1")]
    (execute-one-conn! (.-ds db) [sql])))

(deftype JDBCDB [legend dialect ds mappings]
  api/DB
  (-clear [this] (clear this))
  (-delete-all [this kind] (delete-all this kind))
  (-count [this kind options] (do-count this kind options))
  (-entity [this kind id] (entity this kind id))
  (-find [this kind options] (do-find this kind options))
  (-reduce [this kind f init options] (reduce this kind f init options))
  (-tx [this entity] (tx this entity))
  (-tx* [this entities] (tx* this entities))
  )

(defmethod api/-create-impl :jdbc [config schemas]
  (let [dialect (:dialect config)
        ds (jdbc/get-datasource config)
        legend (legend/build schemas)]
    (JDBCDB. legend dialect ds (atom {}))))

(defn find-sql-
  "Perform a custom SQL query for a specific kind on db instance."
  [db kind sql]
  (let [t-map (key-map db kind)
        command (maybe-str->command sql)]
    (->> (execute-conn! (.-ds db) command {:builder-fn (:builder-fn t-map)})
         (map #(ccc/remove-nils (assoc % :kind kind))))))

(defn find-sql
  "Perform a custom SQL query for a specific kind on the default db instance (api/impl)."
  [kind sql]
  (find-sql- @api/impl kind sql))
