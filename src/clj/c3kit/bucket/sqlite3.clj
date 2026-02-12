(ns c3kit.bucket.sqlite3
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.log :as log]
            [c3kit.apron.time :as time]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.jdbc :as jdbc]
            [clojure.set :as set]
            [clojure.string :as str]
            [next.jdbc])
  (:import (java.nio ByteBuffer ByteOrder)
           (java.sql Connection)
           (javax.sql DataSource)))

(defmethod jdbc/schema->db-type-map :sqlite3 [_]
  {
   :bigdec    "REAL"
   :boolean   "INTEGER"
   :date      "INTEGER"
   :double    "REAL"
   :float     "REAL"
   :instant   "INTEGER"
   :int       "INTEGER"
   :keyword   "TEXT"
   :kw-ref    "TEXT"
   :long      "INTEGER"
   :ref       "INTEGER"
   :string    "TEXT"
   :timestamp "INTEGER"
   :uuid      "TEXT"
   })

(defmethod jdbc/auto-int-primary-key :sqlite3 [_] "INTEGER PRIMARY KEY AUTOINCREMENT")

(defmethod jdbc/build-insert-sql :sqlite3 [dialect t-map entity]
  (let [[sql & args] (jdbc/build-insert-sql-default dialect t-map entity)]
    (cons (str sql " RETURNING id") args)))

(defn- vec->float32-blob
  "Serializes a vector of numbers to a packed little-endian float32 byte array."
  ^bytes [v]
  (let [bb (doto (ByteBuffer/allocate (* (count v) Float/BYTES))
             (.order ByteOrder/LITTLE_ENDIAN))]
    (doseq [f v] (.putFloat bb (float f)))
    (.array bb)))

(defmethod jdbc/->sql-value :sqlite3 [_ type value]
  (cond
    (= :sqlite-vec type) (when value (vec->float32-blob value))
    (and (jdbc/time? type) value) (time/millis-since-epoch value)
    (= :boolean type) (when (some? value) (if value 1 0))
    :else value))

(defn- parse-float32-blob [^bytes blob]
  (let [bb (doto (ByteBuffer/wrap blob) (.order ByteOrder/LITTLE_ENDIAN))
        n  (/ (alength blob) Float/BYTES)]
    (loop [i 0 result (transient [])]
      (if (< i n)
        (recur (inc i) (conj! result (double (.getFloat bb))))
        (persistent! result)))))

(defmethod jdbc/<-sql-value-for-dialect :sqlite3 [_ type value]
  (if (= :sqlite-vec type)
    (when value (parse-float32-blob value))
    value))

(defmethod jdbc/build-upsert-sql :sqlite3 [dialect t-map {:keys [id] :as entity}]
  (let [{:keys [table key->col key->type key->cast]} t-map
        id-col   (:id key->col)
        key->col (select-keys key->col (keys entity))
        sql-args (->> (if id key->col (dissoc key->col :id))
                      keys
                      (map (partial jdbc/->sql-args dialect key->col key->type key->cast entity)))
        cols     (map #(str \" (:column %) \") sql-args)]
    (cons (str "INSERT INTO \"" table "\" (" (str/join ", " cols) ") "
               "VALUES (" (str/join ", " (map :param sql-args)) ") "
               "ON CONFLICT (" id-col ") DO UPDATE SET "
               (str/join ", " (map #(str % " = excluded." %) cols)) " "
               "RETURNING " id-col)
          (map :value sql-args))))

(defn- -build-find-query-default [dialect t-map {:keys [where order-by take drop]}]
  (let [[where-sql & where-args] (jdbc/-build-where dialect t-map where)
        [order-sql & order-args] (jdbc/-build-order-by dialect t-map order-by)
        sql (jdbc/-seq->sql
              "SELECT * FROM " (jdbc/->safe-name dialect (:table t-map))
              where-sql
              order-sql
              (cond take (str "LIMIT " take)
                    drop "LIMIT -1")
              (when drop (str "OFFSET " drop)))]
    (cons sql (concat where-args order-args))))

(defn- vec0-shadow-table? [table-name]
  (str/starts-with? table-name "vec_"))

(defmethod jdbc/existing-tables :sqlite3 [db]
  (->> (jdbc/execute! db ["SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'"])
       (map :sqlite_master/name)
       (remove vec0-shadow-table?)
       sort))

(defn type-spec [type]
  (let [lower-type (str/lower-case type)]
    (cond
      (or (str/starts-with? lower-type "varchar(")
          (str/starts-with? lower-type "text")) [:string type]
      (str/starts-with? (str/lower-case type) "int") [:long type]
      (str/starts-with? type "numeric(") [:bigdec type]
      (= lower-type "real") [:float type]
      (= lower-type "blob") [:bytes type])))

(defn column->spec [indices {:keys [name type notnull dflt_value pk]}]
  (let [[schema-type db-type] (type-spec type)
        db-type (->> [db-type
                      (when (= 1 pk) "PRIMARY KEY")
                      (when (= 1 (:unique (get indices name))) "UNIQUE")
                      (when (= 1 notnull) "NOT NULL")
                      (when dflt_value (str "DEFAULT " dflt_value))]
                     (remove nil?)
                     (str/join " "))]
    (cond-> {:type schema-type}
            (ccc/not-blank? db-type)
            (assoc :db {:type db-type}))))

(defn include-index-info [db index]
  (let [index-info (jdbc/execute-one! db [(str "PRAGMA index_info(\"" (:name index) "\")")])]
    (merge index (set/rename-keys index-info {:name :column}))))

(defn table-indices [db table]
  (let [indices (jdbc/execute! db [(str "PRAGMA index_list(\"" table "\")")])]
    (->> (map (partial include-index-info db) indices)
         (reduce #(assoc %1 (:column %2) %2) {}))))

(defn- assoc-column-spec [indices specs column]
  (assoc specs (keyword (:name column)) (column->spec indices column)))

(defn- keywordize-keys [m]
  (update-keys m (comp keyword name)))

(defmethod jdbc/table-column-specs :sqlite3 [db table]
  (let [indices (table-indices db table)
        columns (->> (jdbc/execute! db ["SELECT * FROM pragma_table_xinfo(?)" table])
                     (map keywordize-keys))]
    (reduce (partial assoc-column-spec indices) {} columns)))

(defmethod jdbc/sql-rename-column :sqlite3 [_db table col-old col-new]
  (str "ALTER TABLE " table " RENAME COLUMN " col-old " TO " col-new))

(defmethod jdbc/drop-column-sql :sqlite3 [_ table column]
  (str "ALTER TABLE " table " DROP COLUMN " column))

(defmethod jdbc/table-exists? :sqlite3 [db table]
  (-> (jdbc/execute-one! db ["SELECT * FROM sqlite_master WHERE type='table' AND name = ?" table])
      seq
      boolean))

(defmethod jdbc/column-exists? :sqlite3 [db table column]
  (-> (jdbc/execute! db ["SELECT * FROM pragma_table_info(?) WHERE name = ?" table column])
      seq
      boolean))

(def ^:private vector-distance-fns
  {'<-> "vec_distance_L2"
   '<=> "vec_distance_cosine"})

(def ^:private op->required-distance
  {'<-> :l2
   '<=> :cosine})

(defn- validate-vector-op! [op field configured-distance]
  (when-let [required (op->required-distance op)]
    (when (not= required configured-distance)
      (throw (ex-info (str "Operator " op " requires :distance " required
                           ", but field " field " is configured as " configured-distance)
                      {:operator op :field field :required required :configured configured-distance})))))

(defmethod jdbc/build-vector-order-clause :sqlite3 [dialect {:keys [key->type key->cast key->distance] :as t-map} field op query-vec]
  (let [distance (get key->distance field :l2)]
    (validate-vector-op! op field distance)
    (if-let [distance-fn (vector-distance-fns op)]
      (let [field-name (jdbc/->field-name dialect t-map field)
            type       (get key->type field)
            cast-type  (get key->cast field)
            param      (jdbc/->sql-param dialect type cast-type)]
        [(str "CASE WHEN " field-name " IS NOT NULL THEN " distance-fn "(" field-name ", " param ") END NULLS LAST")
         (jdbc/->sql-value dialect type query-vec)])
      (throw (ex-info (str "Unsupported vector operator on sqlite3: " op) {:operator op})))))

(defn- sqlite-vec? [type db-type]
  (and (or (= :seq type) (sequential? type))
       db-type
       (str/includes? db-type "vec_f32")))

(defmethod jdbc/->sql-param :sqlite3 [dialect type cast-type]
  (if (= :sqlite-vec type)
    "?"
    (jdbc/default-sql-param dialect type cast-type)))

(defmethod jdbc/sql-col-type :sqlite3 [dialect spec]
  (if (sqlite-vec? (:type spec) (-> spec :db :type))
    "BLOB"
    (jdbc/default-sql-col-type dialect spec)))

(defmethod jdbc/spec->db-type :sqlite3 [_ spec]
  (let [type    (:type spec)
        db-type (-> spec :db :type)]
    (if (sqlite-vec? type db-type)
      :sqlite-vec
      type)))

(defmethod jdbc/spec->db-cast :sqlite3 [_ spec]
  (let [type    (:type spec)
        db-type (-> spec :db :type)]
    (when (sqlite-vec? type db-type)
      db-type)))

(defn- vec0-table-name
  "Returns the vec0 shadow table name for a given table and column."
  [table col]
  (str "vec_" (str/replace table "-" "_") "_" (str/replace col "-" "_")))

(defn- parse-vec-dimension
  "Extracts dimension from db type like 'vec_f32(3)' -> 3"
  [db-type]
  (when-let [[_ dim] (re-find #"vec_f32\((\d+)\)" db-type)]
    (parse-long dim)))

(defn- vector-columns
  "Returns seq of [col-name dimension distance] for vector columns in schema."
  [schema]
  (for [[k spec] (dissoc schema :kind)
        :let [db-type (-> spec :db :type)
              type    (:type spec)]
        :when (sqlite-vec? type db-type)
        :let [dim      (parse-vec-dimension db-type)
              distance (-> spec :db :distance)
              col-name (jdbc/column-name k spec)]]
    [col-name dim distance]))

(defn- safe-col-name
  "Replaces hyphens with underscores for use in vec0 definitions."
  [col]
  (str/replace col "-" "_"))

(defn- drop-vec0-table! [db vec0-name]
  (try
    (jdbc/execute! db [(str "DROP TABLE IF EXISTS " vec0-name)])
    (catch Exception e
      (log/warn "Could not drop vec0 table" vec0-name ":" (.getMessage e)))))

(defn- create-vec0-table! [db table col dim distance]
  (let [vec0-name    (vec0-table-name table col)
        safe-col     (safe-col-name col)
        distance-sql (when (= :cosine distance) " distance_metric=cosine")
        sql          (str "CREATE VIRTUAL TABLE " vec0-name
                          " USING vec0(" safe-col " float[" dim "]"
                          distance-sql ")")]
    (drop-vec0-table! db vec0-name)
    (jdbc/execute! db [sql])))

(defmethod jdbc/after-create-table! :sqlite3 [db schema]
  (let [table (jdbc/table-name schema)]
    (doseq [[col dim distance] (vector-columns schema)]
      (create-vec0-table! db table col dim distance))))

(defn- vec0-table-names-for
  "Returns vec0 virtual table names associated with a given table.
   Queries sqlite_master for virtual tables matching the vec0 naming convention."
  [db table-name]
  (let [prefix (str "vec_" (str/replace table-name "-" "_") "_")]
    (->> (jdbc/execute! db ["SELECT name FROM sqlite_master WHERE sql LIKE 'CREATE VIRTUAL TABLE%'"])
         (map :sqlite_master/name)
         (filter #(str/starts-with? % prefix)))))

(defmethod jdbc/before-drop-table! :sqlite3 [db table-name]
  (doseq [vec0-name (vec0-table-names-for db table-name)]
    (drop-vec0-table! db vec0-name)))

(defn- vec-fields
  "Returns map of {key col-name} for vector fields in a t-map."
  [t-map]
  (into {} (for [[k type] (:key->type t-map)
                 :when (= :sqlite-vec type)]
             [k (get (:key->col t-map) k)])))

(defn- sync-vec0! [conn table col id embedding]
  (let [vec0-name (vec0-table-name table col)]
    (jdbc/execute-one-conn! conn [(str "DELETE FROM " vec0-name " WHERE rowid = ?") id])
    (when embedding
      (jdbc/execute-one-conn! conn [(str "INSERT INTO " vec0-name "(rowid, " (safe-col-name col) ") VALUES (?, ?)")
                                    id (vec->float32-blob embedding)]))))

(defmethod jdbc/after-tx! :sqlite3 [db conn t-map original result]
  (let [vec-fields (vec-fields t-map)]
    (when (seq vec-fields)
      (let [table (:table t-map)
            id    (or (:id result) (:id original))]
        (if (api/delete? original)
          ;; Delete: remove from all vec0 tables
          (doseq [[_ col] vec-fields]
            (jdbc/execute-one-conn! conn [(str "DELETE FROM " (vec0-table-name table col) " WHERE rowid = ?") id]))
          ;; Insert/Update: sync each vector field
          (doseq [[k col] vec-fields]
            (let [embedding (get result k)]
              (sync-vec0! conn table col id embedding))))))))

(defmethod jdbc/after-delete-all! :sqlite3 [db kind]
  (let [t-map      (jdbc/key-map db kind)
        vec-fields (vec-fields t-map)
        table      (:table t-map)]
    (when (seq vec-fields)
      (doseq [[_ col] vec-fields]
        (jdbc/execute-one-conn! (.-ds db) [(str "DELETE FROM " (vec0-table-name table col) " WHERE 1 = 1")])))))

;; --- vec0 MATCH query builder ---

(defn- vec0-order-field
  "If order-by has exactly one vector distance operator, returns [field col op query-vec].
   Otherwise nil."
  [order-by t-map]
  (let [vec-orders (for [[field dir-or-op] order-by
                         :when (jdbc/vector-op? dir-or-op)]
                     [field (first dir-or-op) (second dir-or-op)])]
    (when (= 1 (count vec-orders))
      (let [[field op query-vec] (first vec-orders)
            col (get (:key->col t-map) field)]
        (when col [field col op query-vec])))))

(defn- alias-t-map
  "Returns t-map with table name replaced by alias for field name generation."
  [t-map alias]
  (assoc t-map :table alias))

(defn- build-vec0-find-query
  "Build a vec0 MATCH-based KNN query with JOIN back to entity table."
  [dialect t-map {:keys [where order-by take drop]} field col _op query-vec]
  (let [table         (:table t-map)
        vec0-name     (vec0-table-name table col)
        safe-col      (safe-col-name col)
        k             (min 4096 (or (when (and take drop) (+ take drop))
                                    take
                                    4096))
        aliased-t-map (alias-t-map t-map "i")
        non-vec-order (dissoc order-by field)
        [where-sql & where-args] (when (seq where)
                                   (jdbc/-build-where dialect aliased-t-map where))
        [extra-order-sql & extra-order-args] (when (seq non-vec-order)
                                               (jdbc/-build-order-by dialect aliased-t-map non-vec-order))
        embed-value   (jdbc/->sql-value dialect :sqlite-vec query-vec)
        where-filter  (when where-sql
                        (let [trimmed (str/trim where-sql)]
                          (when (str/starts-with? trimmed "WHERE")
                            (str "AND " (subs trimmed (count "WHERE "))))))
        order-suffix  (when extra-order-sql
                        (let [trimmed (str/trim extra-order-sql)]
                          (when (str/starts-with? trimmed "ORDER BY")
                            (str ", " (subs trimmed (count "ORDER BY "))))))
        sql           (jdbc/-seq->sql
                        "SELECT i.* FROM" vec0-name "v"
                        (str "JOIN " (jdbc/->safe-name dialect table) " i ON i.id = v.rowid")
                        (str "WHERE v." safe-col " MATCH ? AND k = " k)
                        where-filter
                        (str "ORDER BY v.distance" order-suffix)
                        (when take (str "LIMIT " take))
                        (when drop (str "OFFSET " drop)))]
    (cons sql (concat [embed-value] where-args extra-order-args))))

(defmethod jdbc/-build-find-query :sqlite3 [dialect t-map {:keys [order-by take] :as options}]
  (if (and take (vec0-order-field order-by t-map))
    (let [[field col op query-vec] (vec0-order-field order-by t-map)]
      (build-vec0-find-query dialect t-map options field col op query-vec))
    (-build-find-query-default dialect t-map options)))

(defn- load-extension! [^Connection conn ^String path]
  (let [stmt (.prepareStatement conn "SELECT load_extension(?)")]
    (try
      (.setString stmt 1 path)
      (.execute stmt)
      (finally
        (.close stmt)))))

(defn- load-extensions-on-conn! [conn extensions]
  (doseq [path extensions]
    (load-extension! conn path)))

(defn- extension-loading-datasource
  "Wraps a DataSource so that every connection returned has the given
   SQLite extensions loaded."
  [^DataSource ds extensions]
  (reify DataSource
    (getConnection [_]
      (let [conn (.getConnection ds)]
        (load-extensions-on-conn! conn extensions)
        conn))
    (getConnection [_ user pass]
      (let [conn (.getConnection ds user pass)]
        (load-extensions-on-conn! conn extensions)
        conn))
    (getLogWriter [_] (.getLogWriter ds))
    (setLogWriter [_ w] (.setLogWriter ds w))
    (getLoginTimeout [_] (.getLoginTimeout ds))
    (setLoginTimeout [_ t] (.setLoginTimeout ds t))
    (getParentLogger [_] (.getParentLogger ds))
    (^boolean isWrapperFor [_ ^Class c] (.isWrapperFor ds c))
    (unwrap [_ c] (.unwrap ds c))))

(defmethod jdbc/prepare-config :sqlite3 [_ config]
  (cond-> config
    (seq (:extensions config)) (assoc :enable_load_extension true)))

(defmethod jdbc/load-extensions :sqlite3 [_ ds config]
  (if-let [extensions (seq (:extensions config))]
    (do
      (log/info "Configuring SQLite extensions:" (vec extensions))
      (with-open [conn (next.jdbc/get-connection ds)]
        (load-extensions-on-conn! conn extensions))
      (extension-loading-datasource ds (vec extensions)))
    ds))
