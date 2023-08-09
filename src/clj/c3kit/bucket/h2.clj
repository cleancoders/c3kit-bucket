(ns c3kit.bucket.h2
  (:require [c3kit.bucket.jdbc :as jdbc]
            [clojure.string :as str]))


(defmethod jdbc/build-upsert-sql :h2 [dialect t-map {:keys [id] :as entity}]
  (let [{:keys [table key->col key->type]} t-map
        used-key->col (select-keys key->col (keys entity))
        ->sql-args    (partial jdbc/->sql-args dialect used-key->col key->type entity)
        sql-args      (->> used-key->col keys (map ->sql-args))
        cols          (->> (map :column sql-args) (map #(str "\"" % "\"")))]
    (cons (str "MERGE INTO " table " (" (str/join ", " cols) ") "
               "VALUES (" (str/join ", " (map :param sql-args)) ")")
          (map :value sql-args))))

(defmethod jdbc/existing-tables :h2 [db]
  (->> (jdbc/execute! db ["SHOW TABLES"])
       (map :tables/table_name)
       sort))

(defn default-serial? [column]
  (some? (:columns/identity_generation column)))

(defn default-value [column]
  (when-not (default-serial? column)
    (if-let [default (:columns/column_default column)]
      (first (str/split default #"\:\:"))
      nil)))

(defmulti -type-spec :columns/data_type)
(defmethod -type-spec :default [column] (let [type (:columns/data_type column)] [(keyword type) type]))
(defmethod -type-spec "character varying" [column] [:string (str "varchar(" (:columns/character_maximum_length column) ")")])
(defmethod -type-spec "numeric" [column] [:bigdec (str "numeric(" (:columns/numeric_precision column) "," (:columns/numeric_scale  column) ")")])
(defmethod -type-spec "integer" [column] [:long (if (default-serial? column) "serial" "integer")])

(defn column->spec [constraints column]
  (let [default (default-value column)
        [schema-type db-type]    (-type-spec column)
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
         (reduce #(assoc %1 (:key_column_usage/column_name %2) (:table_constraints/constraint_type %2)) {}))))

(defn table-constraints [db table]
  (let [sql    (str "SELECT * FROM information_schema.table_constraints tc "
                    "inner join information_schema.key_column_usage cu "
                    "on cu.constraint_name = tc.constraint_name "
                    "WHERE tc.table_name=?")
        result (jdbc/execute! db [sql table])]
    (result-set->table-constraints result)))

(defmethod jdbc/table-column-specs :h2 [db table]
  (let [constraints (table-constraints db table)
        columns (jdbc/execute! db ["SELECT * FROM information_schema.columns WHERE table_name = ?" table])]
    (reduce #(assoc %1 (:columns/column_name %2) (column->spec constraints %2)) {} columns)))

(defmethod jdbc/sql-rename-column :h2 [_db table col-old col-new]
  (str "ALTER TABLE " (name table) " ALTER COLUMN IF EXISTS \"" (name col-old) "\" RENAME TO \"" (name col-new) "\""))
