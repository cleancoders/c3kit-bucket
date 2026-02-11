(ns c3kit.bucket.sqlite3
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.log :as log]
            [c3kit.apron.time :as time]
            [c3kit.bucket.jdbc :as jdbc]
            [clojure.set :as set]
            [clojure.string :as str]
            [next.jdbc])
  (:import (java.nio ByteBuffer ByteOrder)
           (org.sqlite SQLiteConnection)))

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

(defmethod jdbc/build-insert-sql :sqlite3 [_ t-map entity]
  (let [[sql & args] (jdbc/build-insert-sql nil t-map entity)]
    (cons (str sql " RETURNING id") args)))

(defmethod jdbc/->sql-value :sqlite3 [_ type value]
  (cond
    (= :sqlite-vec type) (when value (str "[" (str/join "," value) "]"))
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

(defmethod jdbc/-build-find-query :sqlite3 [dialect t-map {:keys [where order-by take drop]}]
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

(defmethod jdbc/existing-tables :sqlite3 [db]
  (->> (jdbc/execute! db ["SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'"])
       (map :sqlite_master/name)
       sort))

(defn type-spec [type]
  (let [lower-type (str/lower-case type)]
    (cond
      (or (str/starts-with? lower-type "varchar(")
          (str/starts-with? lower-type "text")) [:string type]
      (str/starts-with? (str/lower-case type) "int") [:long type]
      (str/starts-with? type "numeric(") [:bigdec type]
      (= lower-type "real") [:float type])))

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

(defmethod jdbc/build-vector-order-clause :sqlite3 [dialect {:keys [key->type key->cast] :as t-map} field op query-vec]
  (if-let [distance-fn (vector-distance-fns op)]
    (let [field-name (jdbc/->field-name dialect t-map field)
          type       (get key->type field)
          cast-type  (get key->cast field)
          param      (jdbc/->sql-param dialect type cast-type)]
      [(str distance-fn "(" field-name ", " param ")") (jdbc/->sql-value dialect type query-vec)])
    (throw (ex-info (str "Unsupported vector operator on sqlite3: " op) {:operator op}))))

(defn- sqlite-vec? [type db-type]
  (and (= :seq type) db-type (str/includes? db-type "vec_f32")))

(defmethod jdbc/->sql-param :sqlite3 [dialect type cast-type]
  (if (= :sqlite-vec type)
    "vec_f32(?)"
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

(defn- load-extension! [conn path]
  (log/info "Loading SQLite extension:" path)
  (try
    (let [stmt (.prepareStatement conn "SELECT load_extension(?)")]
      (try
        (.setString stmt 1 path)
        (.execute stmt)
        (finally
          (.close stmt))))
    (catch Exception e
      (throw (ex-info (str "Failed to load SQLite extension: " path) {:path path} e)))))

(defn- enable-and-load-extensions! [ds extensions]
  (let [conn (.unwrap (next.jdbc/get-connection ds) SQLiteConnection)]
    (.enableLoadExtension conn true)
    (doseq [path extensions]
      (load-extension! conn path))))

(defmethod jdbc/load-extensions :sqlite3 [_ ds config]
  (when-let [extensions (seq (:extensions config))]
    (enable-and-load-extensions! ds extensions)))
