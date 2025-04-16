(ns c3kit.bucket.jdbc
  (:refer-clojure :rename {find core-file count core-count reduce core-reduce})
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.apron.log :as log]
            [c3kit.apron.schema :as schema]
            [c3kit.apron.time :as time]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.migrator :as migrator]
            [clojure.set :as set]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.connection :as connection]
            [next.jdbc.result-set :as rs])
  (:import (com.mchange.v2.c3p0 ComboPooledDataSource PooledDataSource)
           (java.sql ResultSet)))

(def ^:dynamic in-transaction? false)

(defn execute-one-conn!
  ([conn command] (execute-one-conn! conn command {}))
  ([conn command options]
   (try
     (log/debug "executing SQL:" command)
     (jdbc/execute-one! conn command options)
     (catch Exception e
       (log/error "Choked on SQL: " command)
       (throw e)))))

(defn- execute-conn!
  ([conn command] (execute-conn! conn command {}))
  ([conn command options]
   (try
     (log/debug "executing SQL:" command)
     (jdbc/execute! conn command options)
     (catch Exception e
       (log/error "Choked on SQL: " command)
       (throw e)))))

(defn- maybe-str->command [command] (if (string? command) [command] command))

(defn execute-one!
  "Execute SQL returning 1 raw result"
  ([command] (execute-one! @api/impl command))
  ([db command] (execute-one-conn! (.-ds db) (maybe-str->command command) {}))
  ([db command options] (execute-one-conn! (.-ds db) (maybe-str->command command) options)))

(defn execute!
  "Execute SQL returning all results."
  ([command] (execute! @api/impl command))
  ([db command] (execute-conn! (.-ds db) (maybe-str->command command) {}))
  ([db command options] (execute-conn! (.-ds db) (maybe-str->command command) options)))

(defn table-name [schema]
  (or (-> schema :kind :db :table)
      (-> schema :kind :db :name)
      (-> schema :kind :value name)))

(defn dialect [db] (.-dialect db))

(defmulti ->safe-name (fn [dialect _name] dialect))
(defmethod ->safe-name :default [_ name] (str \" name \"))

(defn drop-table [db table-name]
  (execute-conn! (.-ds db) [(str "DROP TABLE IF EXISTS " (->safe-name (dialect db) table-name))]))

(defn- drop-table-from-schema [db schema]
  (drop-table db (table-name schema)))

(defmulti schema->db-type-map identity)

(defmethod schema->db-type-map :default [_]
  {:int     "int4"
   :long    "int4"
   :boolean "bool"
   :bigdec  "float"
   :ref     "int4"
   :keyword "varchar"
   :kw-ref  "varchar"
   :instant "timestamp without time zone"
   :date    "date"
   :string  "varchar"
   })

(defn schema-type->db-type [dialect type]
  (get (schema->db-type-map dialect) type))

(defn ->sql-type [dialect type]
  (or (schema-type->db-type dialect type)
      (name type)))

(defn column-name
  ([[key spec]] (column-name key spec))
  ([key spec] (or (-> spec :db :name)
                  (-> spec :db :column)
                  (name key))))

(defn sql-col-type [dialect spec]
  (let [type (:type spec)]
    (or (-> spec :db :type)
        (->sql-type dialect type))))

(defn sql-table-col [dialect key spec]
  (let [column-name (->safe-name dialect (column-name key spec))
        column-type (sql-col-type dialect spec)]
    (str column-name " " column-type)))

(defn sql-add-column
  ([dialect table col spec]
   (let [table  (->safe-name dialect (name table))
         column (->safe-name dialect (name col))
         type   (sql-col-type dialect spec)]
     (str "ALTER TABLE " table " ADD COLUMN " column " " type)))
  ([dialect schema attr]
   (let [table  (table-name schema)
         column (sql-table-col dialect attr (get schema attr))]
     (str "ALTER TABLE " table " ADD COLUMN " column))))

(defn sql-create-table [dialect schema]
  (let [table     (->safe-name dialect (table-name schema))
        col-specs (map (fn [[key spec]] (sql-table-col dialect key spec)) (dissoc schema :kind))]
    (str "CREATE TABLE " table " ("
         (str/join "," col-specs)
         ")")))

(defn create-table-from-schema [db schema]
  (let [dialect (.-dialect db)
        ds      (.-ds db)
        sql     (sql-create-table dialect schema)]
    (execute-conn! ds [sql])))

(defn- ->field-name [dialect {:keys [table key->col]} k]
  (cond->> (->safe-name dialect (get key->col k))
           (not (namespace k))
           (str (->safe-name dialect table) \.)))

(defn- json? [type db-type]
  (and (= :string type)
       db-type
       (str/starts-with? db-type "json")))

(defn spec->db-type [spec]
  (let [type      (:type spec)
        db-type   (-> spec :db :type)
        cast-type (-> spec :db :cast)]
    (or (when (json? type db-type) :json)
        cast-type
        type)))

(defn time? [type] (#{:instant :date :timestamp} type))

(defn <-sql-value [type value]
  (cond
    (= :json type) (.getValue value)
    (= :uuid type) (schema/->uuid value)
    (= :bigdec type) (schema/->bigdec value)
    (#{:keyword :kw-ref} type) (schema/->keyword value)
    (and (time? type) (integer? value)) (time/from-epoch value)
    (and (= :boolean type) (integer? value)) (not= 0 value)
    :else value))

(defmulti ->sql-value (fn [dialect _type _value] dialect))

(defmethod ->sql-value :default [_ type value]
  (case type
    :date (schema/->sql-date value)
    :timestamp (schema/->timestamp value)
    :instant (schema/->timestamp value)
    :boolean (cond-> value (integer? value) (not= 0))
    :keyword (str value)
    :kw-ref (str value)
    :ref (schema/->int value)
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
  (let [rs-meta   (.getMetaData rs)
        col-range (range 1 (inc (if rs-meta (.getColumnCount rs-meta) 0)))
        cols      (mapv (fn [^Integer i]
                          (let [col-name (.getColumnLabel rs-meta i)]
                            (get col->key col-name))) col-range)]
    (->MapResultSetOptionalBuilder rs rs-meta cols key->type)))

(defn compile-mapping [schema]
  (let [k->c (core-reduce (fn [m [k s]] (assoc m k (or (-> s :db :column)
                                                       (-> s :db :name)
                                                       (get-full-key-name k)))) {} (dissoc schema :kind))
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

(defn key-map
  ([db kind] (key-map @(.-legend db) (.-mappings db) kind))
  ([legend mappings kind]
   (if-let [m (get @mappings kind)]
     m
     (let [schema (legend/for-kind legend kind)
           m      (compile-mapping schema)]
       (swap! mappings assoc kind m)
       m))))

(defn ->sql-args [dialect key->col key->type entity k]
  (let [v    (get entity k)
        type (get key->type k)]
    {:column (get key->col k)
     :param  (->sql-param dialect type)
     :value  (->sql-value dialect type v)}))

(defn build-fetch-sql [dialect t-map id]
  (let [{:keys [table key->col key->type]} t-map
        column (:id key->col)
        type   (:id key->type)
        param  (->sql-param dialect type)
        value  (->sql-value dialect type id)]
    [(str "SELECT * FROM " (->safe-name dialect table) " WHERE " column " = " param) value]))

(defn clause-with-operator [dialect {:keys [key->type] :as t-map} k v operator]
  (let [type (get key->type k)]
    [(str (->field-name dialect t-map k) " " operator " " (->sql-param dialect type))
     (->sql-value dialect type v)]))

(defn- -equality-clause [dialect t-map k v]
  (if (nil? v)
    [(str (->field-name dialect t-map k) " IS NULL")]
    (clause-with-operator dialect t-map k v "=")))

(defn- -build-seq-or-clause [not? dialect {:keys [key->type] :as t-map} k v]
  (let [type       (get key->type k)
        is-null?   (some nil? v)
        v          (->> v set (remove nil?))
        in?        (seq v)
        field-name (->field-name dialect t-map k)
        num-vals   (core-count v)]
    (cons (str "("
               (when in? (str field-name (when not? " NOT") " IN (" (str/join "," (repeat num-vals (->sql-param dialect type))) ")"))
               (when (and in? is-null?) (if not? " AND " " OR "))
               (when is-null? (str field-name " IS" (when not? " NOT") " NULL"))
               ")")
          (map (partial ->sql-value dialect type) v))))

(defn- -build-parity-or-clause [not? dialect t-map k v]
  (cond
    (seq v) (-build-seq-or-clause not? dialect t-map k v)
    not? ["1 = 1"]
    :else ["1 != 1"]))

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

(defn -build-where [dialect t-map kv-pairs]
  (if-let [sql-conditions (seq (->sql-clauses dialect t-map kv-pairs))]
    (cons (str/join " " (cons "WHERE" (interpose "AND" (map first sql-conditions))))
          (mapcat rest sql-conditions))
    [""]))

(defn- fetch-entity [db conn t-map kind id]
  (when-let [id (if (map? id) (:id id) id)]
    (let [{:keys [id-type builder-fn]} t-map
          id      (api/-coerced-id id-type id)
          command (build-fetch-sql (.-dialect db) t-map id)
          result  (execute-one-conn! conn command {:builder-fn builder-fn})]
      (when result
        (assoc result :kind kind)))))

(defn entity [db kind id]
  (if (nil? kind)
    (throw (UnsupportedOperationException. "JDBC/entity requires a kind"))
    (fetch-entity db (.-ds db) (key-map db kind) kind id)))

(defn arg->set-sql [{:keys [column param]}] (str \" column "\" = " param))

(defn build-update-sql
  ([dialect t-map entity] (build-update-sql dialect t-map entity {}))
  ([dialect t-map entity cas]
   (let [{:keys [table key->col key->type]} t-map
         ->sql-args (partial ->sql-args dialect key->col key->type entity)
         used-keys  (disj (set/intersection (set (keys entity)) (set (keys key->col))) :id)
         sql-args   (map ->sql-args used-keys)
         set-sql    (str/join ", " (map arg->set-sql sql-args))
         where      (-> cas (assoc :id (:id entity)) (dissoc :kind))
         [where-sql & args] (-build-where dialect t-map where)]
     (cons (str "UPDATE " (->safe-name dialect table) " SET " set-sql " " where-sql)
           (concat (map :value sql-args) args)))))

(defmulti build-upsert-sql (fn [dialect _t-map _entity] dialect))
(defmulti build-insert-sql (fn [dialect _t-map _entity] dialect))

(defmethod build-insert-sql :default [dialect t-map entity]
  (let [{:keys [table key->col key->type]} t-map
        used-key->col (select-keys key->col (keys entity))
        ->sql-args    (partial ->sql-args dialect used-key->col key->type entity)
        sql-args      (->> used-key->col keys (map ->sql-args))
        cols          (->> (map :column sql-args) (map #(str \" % \")))
        table         (->safe-name dialect table)]
    (cons (str "INSERT INTO " table " (" (str/join ", " cols) ") "
               "VALUES (" (str/join ", " (map :param sql-args)) ")")
          (map :value sql-args))))

(defn- build-delete-sql [dialect t-map entity]
  (let [{:keys [table key->col key->type]} t-map
        {:keys [column param value]} (->sql-args dialect key->col key->type entity :id)
        table (->safe-name dialect table)]
    [(str "DELETE FROM " table " WHERE " column " = " param) value]))

(defn- delete-entity [dialect conn t-map entity]
  (let [sql    (build-delete-sql dialect t-map entity)
        result (execute-one-conn! conn sql)]
    (when-not (= 1 (:next.jdbc/update-count result)) (throw (ex-info "delete failed" {:entity entity :result result})))
    (api/soft-delete entity)))

(defn execute-update! [db conn t-map entity cas]
  (let [command (build-update-sql (.-dialect db) t-map entity cas)
        result  (execute-one-conn! conn command)]
    (when-not (or (empty? cas) (= 1 (:next.jdbc/update-count result)))
      (throw (ex-info "cas failure" {:cas cas :entity entity :result result})))
    result))

(defn- update-entity
  ([db conn t-map entity] (update-entity db conn t-map entity {}))
  ([db conn t-map entity cas]
   (execute-update! db conn t-map entity cas)
   (fetch-entity db conn t-map (:kind entity) (:id entity))))

(defn- insert-entity [db conn t-map entity]
  (let [command (build-insert-sql (.-dialect db) t-map entity)
        result  (execute-one-conn! conn command {:return-keys [:id]})
        id      (first (vals result))]
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
  (let [kind  (:kind entity)
        t-map (key-map db kind)]
    (if (api/delete? entity)
      (delete-entity (.-dialect db) conn t-map entity)
      (if-let [cas (api/-get-cas entity)]
        (update-entity db conn t-map entity cas)
        (upsert-by-id-strategy db conn t-map entity)))))

(defn tx [db entity] (do-tx db (.-ds db) entity))

(defn tx* [db entities]
  (binding [in-transaction? true]
    (jdbc/with-transaction [tx (.-ds db)]
                           (doall (map #(do-tx db tx %) entities)))))

(defn -seq->sql [& sql-bits]
  (->> (flatten sql-bits)
       (remove nil?)
       (str/join " ")))

(defmulti -build-find-query (fn [dialect _t-map _options] dialect))
(defmethod -build-find-query :default [dialect t-map {:keys [where take drop]}]
  (let [[where-sql & args] (-build-where dialect t-map where)
        table (->safe-name dialect (:table t-map))
        sql   (-seq->sql "SELECT * FROM" table
                         where-sql
                         (when take (str "LIMIT " take))
                         (when drop (str "OFFSET " drop)))]
    (cons sql args)))

(defn- do-find [db kind options]
  (let [t-map (key-map db kind)
        query (-build-find-query (dialect db) t-map options)]
    (->> (execute-conn! (.-ds db) query {:builder-fn (:builder-fn t-map)})
         (map #(ccc/remove-nils (assoc % :kind kind))))))

(defn- do-count [db kind {:keys [where] :as _options}]
  (let [{:keys [table] :as t-map} (key-map db kind)
        dialect (dialect db)
        table   (->safe-name dialect table)
        [where-sql & args] (-build-where dialect t-map where)
        sql     (str/join " " ["SELECT COUNT(*) FROM" table where-sql])]
    (first (vals (execute-one-conn! (.-ds db) (cons sql args))))))

(defn reduce-sql- [db kind f init sql]
  (let [t-map      (key-map db kind)
        connection (jdbc/get-connection (.-ds db))
        command    (maybe-str->command sql)]
    (.setHoldability connection ResultSet/CLOSE_CURSORS_AT_COMMIT)
    (core-reduce
      (fn [a b] (f a (ccc/remove-nils (assoc b :kind kind))))
      init
      (jdbc/plan connection command {:builder-fn  (:builder-fn t-map)
                                     :fetch-size  1000
                                     :concurrency :read-only
                                     :cursors     :close
                                     :result-type :forward-only}))))

(defn reduce [db kind f init options]
  (let [t-map (key-map db kind)
        query (-build-find-query (.-ds db) t-map options)]
    (reduce-sql- db kind f init query)))

(defmulti existing-tables (fn [db] (.-dialect db)))
(defmulti table-column-specs (fn [db _table] (.-dialect db)))
(defmulti sql-rename-column (fn [db _table _col-old _col-new] (.-dialect db)))
(defmulti table-exists? (fn [db _table] (.-dialect db)))
(defmulti column-exists? (fn [db _table _column] (.-dialect db)))

(defn clear [db]
  (api/-assert-safety-off! "clear")
  (doseq [[_ schema] @(.-legend db)]
    (drop-table-from-schema db schema)
    (create-table-from-schema db schema)))

(defn delete-all [db kind]
  (api/-assert-safety-off! "delete")
  (let [{:keys [table]} (key-map db kind)
        table (->safe-name (dialect db) table)
        sql   (str "DELETE FROM " table " WHERE 1 = 1")]
    (execute-one-conn! (.-ds db) [sql])))

(defn build-table-schema [db name->key result table]
  (let [columns (table-column-specs db table)
        schema  (core-reduce (fn [result [column spec]]
                               (let [key (get name->key [table column] (keyword column))]
                                 (assoc result key spec)))
                             {} columns)]
    (assoc result (get name->key table) schema)))

(defn schema-exists? [db schema]
  (let [table-name (table-name schema)]
    (table-exists? db table-name)))

;; TODO - MDM: don't do name translation here, let migration handle it.
(defn build-installed-schema-legend [db legend]
  (let [db-names->schema-keys (core-reduce (fn [result [kind schema]]
                                             (let [table  (table-name schema)
                                                   result (assoc result table kind)]
                                               (core-reduce (fn [result [attr spec]]
                                                              (let [column (column-name attr spec)]
                                                                (assoc result [table column] attr)))
                                                            result (dissoc schema :kind)))) {} legend)
        tables                (existing-tables db)]
    (core-reduce (partial build-table-schema db db-names->schema-keys) {} tables)))

(defn- do-install-schema [db schema]
  (log/info (str "  installing schema " (-> schema :kind :value) " (" (table-name schema) ")"))
  (execute! db (sql-create-table (dialect db) schema)))

(defn do-add-attribute!
  ([db schema attr]
   (execute! db [(sql-add-column (dialect db) schema attr)]))
  ([db kind attr spec]
   (if (column-exists? db (name kind) (name attr))
     (log/warn "  add attribute ALREADY EXISTS " (keyword (name kind) (name attr)))
     (execute! db [(sql-add-column (dialect db) kind attr spec)]))))

(defmulti drop-column-sql (fn [dialect _table _column] dialect))
(defmethod drop-column-sql :default [_ table column]
  (str "ALTER TABLE " table " DROP COLUMN IF EXISTS " column))

(defn do-remove-attribute! [db kind attr]
  (if (column-exists? db (name kind) (name attr))
    (let [dialect (dialect db)
          table   (->safe-name dialect (name kind))
          column  (->safe-name dialect (name attr))
          sql     (drop-column-sql dialect table column)]
      (log/info "  removing " (keyword (name kind) (name attr)))
      (execute! db [sql]))
    (log/warn "  remove MISSING " (keyword (name kind) (name attr)))))

(defn do-rename-attribute! [db kind attr new-kind new-attr]
  (when-not (= kind new-kind)
    (throw (ex-info "cannot rename attribute kind" {:kind kind :attr attr :new-kind new-kind :new-attr new-attr})))
  (if (column-exists? db (name kind) (name attr))
    (let [dialect (dialect db)
          table   (->safe-name dialect (name kind))
          old-col (->safe-name dialect (name attr))
          new-col (->safe-name dialect (name new-attr))
          sql     (sql-rename-column db table old-col new-col)]
      (log/info (str "  renaming " (keyword (name kind) (name attr)) " to " (keyword (name new-kind) (name new-attr))))
      (execute! db [sql]))
    (log/warn "  rename MISSING " (keyword (name kind) (name attr)))))

(defn do-close [db]
  (let [ds (.-ds db)]
    (when (instance? PooledDataSource ds)
      (.close ds))))

(deftype JDBCDB [legend dialect ds mappings]
  api/DB
  (close [this] (do-close this))
  (-clear [this] (clear this))
  (-delete-all [this kind] (delete-all this kind))
  (-count [this kind options] (do-count this kind options))
  (-entity [this kind id] (entity this kind id))
  (-find [this kind options] (do-find this kind options))
  (-reduce [this kind f init options] (reduce this kind f init options))
  (-tx [this entity] (tx this entity))
  (-tx* [this entities] (tx* this entities))
  migrator/Migrator
  (-schema-exists? [this schema] (schema-exists? this schema))
  (-installed-schema-legend [this existing-legend] (build-installed-schema-legend this existing-legend))
  (-install-schema! [this schema] (do-install-schema this schema))
  (-add-attribute! [this schema attr] (do-add-attribute! this schema attr))
  (-add-attribute! [this kind attr spec] (do-add-attribute! this kind attr spec))
  (-remove-attribute! [this kind attr] (do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (do-rename-attribute! this kind attr new-kind new-attr)))

(defn connect [config]
  (if (:connection-pool? config)
    (let [config                    (set/rename-keys config {:min-pool-size :minPoolSize :max-pool-size :maxPoolSize})
          ^ComboPooledDataSource ds (connection/->pool ComboPooledDataSource config)]
      (log/info "\tConnection Pooling: " {:min (.getMinPoolSize ds) :max (.getMaxPoolSize ds)})
      (.close (jdbc/get-connection ds))                     ;; initialize and validate pool says the docs
      ds)
    (jdbc/get-datasource config)))

(defmethod api/-create-impl :jdbc [config schemas]
  (let [dialect (:dialect config)
        ds      (connect config)
        legend  (atom (legend/build schemas))]
    (require [(symbol (str "c3kit.bucket." (name dialect)))])
    (JDBCDB. legend dialect ds (atom {}))))

(defn find-sql-
  "Perform a custom SQL query for a specific kind on db instance."
  [db kind sql]
  (let [t-map   (key-map db kind)
        command (maybe-str->command sql)]
    (->> (execute-conn! (.-ds db) command {:builder-fn (:builder-fn t-map)})
         (map #(ccc/remove-nils (assoc % :kind kind))))))

(defn find-sql
  "Perform a custom SQL query for a specific kind on the default db instance (api/impl)."
  [kind sql]
  (find-sql- @api/impl kind sql))

(defmulti auto-int-primary-key identity)
(defmethod auto-int-primary-key :default [_] "serial PRIMARY KEY")

(defmethod migrator/migration-schema :jdbc [{:keys [dialect]}]
  (merge-with
    merge
    migrator/default-migration-schema
    {:id   {:db {:type (auto-int-primary-key dialect)}}
     :name {:db {:type "varchar(255) UNIQUE"}}
     :at   {:db {:type (->sql-type dialect :timestamp)}}}))
