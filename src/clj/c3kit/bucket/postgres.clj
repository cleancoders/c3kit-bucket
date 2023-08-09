(ns c3kit.bucket.postgres
  (:require [c3kit.bucket.jdbc :as jdbc]
            [clojure.string :as str]))

(defmethod jdbc/schema->db-type-map :postgres [_]
  {:int     "int4"
   :long    "int4"
   :boolean "bool"
   :string  "text"
   :instant "timestamp without time zone"})

(defmethod jdbc/build-upsert-sql :postgres [dialect t-map {:keys [id] :as entity}]
  (let [{:keys [table key->col key->type]} t-map
        id-col   (:id key->col)
        key->col (select-keys key->col (keys entity))
        sql-args (->> (if id key->col (dissoc key->col :id))
                      keys
                      (map (partial jdbc/->sql-args dialect key->col key->type entity)))
        cols     (map :column sql-args)
        cols     (map #(str "\"" % "\"") cols)]
    (cons (str "INSERT INTO " table " (" (str/join ", " cols) ") "
               "VALUES (" (str/join ", " (map :param sql-args)) ") "
               "ON CONFLICT (" id-col ") DO UPDATE SET "
               (str/join ", " (map #(str % " = excluded." %) cols)) " "
               "RETURNING " id-col)
          (map :value sql-args))))

(defmethod jdbc/existing-tables :postgres [db]
  (->> (jdbc/execute! db ["SELECT * FROM pg_catalog.pg_tables"])
       (map :pg_tables/tablename)
       (remove #(str/starts-with? % "pg_"))
       (remove #(str/starts-with? % "sql_"))
       sort))

(defn default-serial? [column]
  (when-let [default (:columns/column_default column)]
    (str/starts-with? default "nextval(")))

(defn default-value [column]
  (when-not (default-serial? column)
    (if-let [default (:columns/column_default column)]
      (first (str/split default #"\:\:"))
      nil)))

(defmulti -type-spec :columns/udt_name)
(defmethod -type-spec :default [column] (let [type (:columns/udt_name column)] [(keyword type) type]))
(defmethod -type-spec "varchar" [column] [:string (str "varchar(" (:columns/character_maximum_length column) ")")])
(defmethod -type-spec "numeric" [column] [:bigdec (str "numeric(" (:columns/numeric_precision column) "," (:columns/numeric_scale column) ")")])
(defmethod -type-spec "int4" [column] [:long (if (default-serial? column) "serial" "int4")])

(defn column->spec [constraints column]
  (let [default (default-value column)
        [schema-type db-type] (-type-spec column)
        name    (:columns/column_name column)
        db-type (->> [db-type (get constraints name) (when default (str "DEFAULT " default))]
                     (remove nil?)
                     (str/join " "))]
    (if (str/blank? db-type)
      {:type schema-type}
      {:type schema-type :db {:type db-type}})))

(defn result-set->table-constraints [result-set]
  (let [name-frequencies      (frequencies (map :table_constraints/constraint_name result-set))
        multi-col-constraints (reduce (fn [r [c n]] (if (> n 1) (conj r c) r)) #{} name-frequencies)]
    (->> result-set
         (remove #(contains? multi-col-constraints (:table_constraints/constraint_name %)))
         (remove #(= "FOREIGN KEY" (:table_constraints/constraint_type %)))
         (reduce #(assoc %1 (:constraint_column_usage/column_name %2) (:table_constraints/constraint_type %2)) {}))))

(defn table-constraints [db table]
  (let [sql    (str "SELECT * FROM information_schema.table_constraints tc "
                    "inner join information_schema.constraint_column_usage cu "
                    "on cu.constraint_name = tc.constraint_name "
                    "WHERE tc.table_name=?")
        result (jdbc/execute! db [sql table])]
    (result-set->table-constraints result)))

(defmethod jdbc/table-column-specs :postgres [db table]
  (let [constraints (table-constraints db table)
        columns     (jdbc/execute! db ["SELECT * FROM information_schema.columns WHERE table_name = ?" table])]
    (reduce #(assoc %1 (:columns/column_name %2) (column->spec constraints %2)) {} columns)))

(defmethod jdbc/sql-rename-column :postgres [_db table col-old col-new]
  (str "ALTER TABLE " (name table) " RENAME COLUMN \"" (name col-old) "\" TO \"" (name col-new) "\""))

(defmethod jdbc/table-exists? :postgres [db table]
  (:exists (jdbc/execute-one! db ["SELECT EXISTS (SELECT FROM pg_tables WHERE tablename=?)" table])))
