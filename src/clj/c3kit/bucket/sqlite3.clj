(ns c3kit.bucket.sqlite3
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.time :as time]
            [c3kit.bucket.jdbc :as jdbc]
            [clojure.set :as set]
            [clojure.string :as str]))

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
    (and (jdbc/time? type) value) (time/millis-since-epoch value)
    (= :boolean type) (when (some? value) (if value 1 0))
    :else value))

(defmethod jdbc/build-upsert-sql :sqlite3 [dialect t-map {:keys [id] :as entity}]
  (let [{:keys [table key->col key->type]} t-map
        id-col   (:id key->col)
        key->col (select-keys key->col (keys entity))
        sql-args (->> (if id key->col (dissoc key->col :id))
                      keys
                      (map (partial jdbc/->sql-args dialect key->col key->type entity)))
        cols     (map #(str \" (:column %) \") sql-args)]
    (cons (str "INSERT INTO \"" table "\" (" (str/join ", " cols) ") "
               "VALUES (" (str/join ", " (map :param sql-args)) ") "
               "ON CONFLICT (" id-col ") DO UPDATE SET "
               (str/join ", " (map #(str % " = excluded." %) cols)) " "
               "RETURNING " id-col)
          (map :value sql-args))))

(defmethod jdbc/-build-find-query :sqlite3 [dialect t-map {:keys [where take drop]}]
  (let [[where-sql & args] (jdbc/-build-where dialect t-map where)
        sql (jdbc/-seq->sql
              "SELECT * FROM " (jdbc/->safe-name dialect (:table t-map))
              where-sql
              (cond take (str "LIMIT " take)
                    drop "LIMIT -1")
              (when drop (str "OFFSET " drop)))]
    (cons sql args)))

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
