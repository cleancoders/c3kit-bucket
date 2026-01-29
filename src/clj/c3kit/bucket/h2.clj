(ns c3kit.bucket.h2
  (:require [c3kit.bucket.jdbc :as jdbc]
            [clojure.string :as str]))

(defn- json? [type db-type]
  (and (= :string type)
       db-type
       (str/includes? db-type "json")))

(defmethod jdbc/spec->db-type :h2 [_ spec]
  (let [type    (:type spec)
        db-type (-> spec :db :type)]
    (if (json? type db-type)
      :json
      type)))

(defn- unwrap-json-string
  "H2 wraps JSON string values with extra quotes - unwrap if needed"
  [s]
  (if (and (string? s)
           (> (count s) 1)
           (= \" (first s))
           (= \" (last s)))
    (-> s
        (subs 1 (dec (count s)))
        (str/replace "\\\"" "\"")
        (str/replace "\\\\" "\\"))
    s))

(defmethod jdbc/<-sql-value-for-dialect :h2 [_ type value]
  (if (= :json type)
    (when value
      (let [s (if (bytes? value)
                (String. ^bytes value "UTF-8")
                value)]
        (unwrap-json-string s)))
    value))

;; Note: H2 doesn't need a cast for JSON - it accepts the string directly
;; Using CAST(? AS json) causes H2 to double-encode the value


(defmethod jdbc/build-upsert-sql :h2 [dialect t-map {:keys [id] :as entity}]
  (let [{:keys [table key->col key->type key->cast]} t-map
        used-key->col (select-keys key->col (keys entity))
        ->sql-args    (partial jdbc/->sql-args dialect used-key->col key->type key->cast entity)
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
  (str "ALTER TABLE " table " ALTER COLUMN IF EXISTS " col-old " RENAME TO " col-new))

(defmethod jdbc/table-exists? :h2 [db table]
  (some? (jdbc/execute-one! db ["SELECT * FROM information_schema.tables WHERE table_name=?" table])))

(defmethod jdbc/column-exists? :h2 [db table column]
  (->> (jdbc/execute! db ["SELECT column_name FROM information_schema.columns WHERE table_name = ?" table])
       (map :columns/column_name)
       (some #(= column %))
       boolean))
