(ns c3kit.bucket.mssql
  (:require [c3kit.bucket.jdbc :as jdbc]))

(defmethod jdbc/schema->db-type-map :mssql [_]
  {:bigdec    "decimal(32, 16)"
   :boolean   "bit"
   :double    "float"
   :instant   "datetime2"
   :int       "int"
   :keyword   "varchar(255)"
   :kw-ref    "varchar(255)"
   :long      "bigint"
   :ref       "int"
   :string    "varchar(255)"
   :timestamp "datetime2"
   :uuid      "uniqueidentifier"})

(defmethod jdbc/->safe-name :mssql [_ name] (str \[ name \]))

(defmethod jdbc/-build-find-query :mssql [dialect t-map {:keys [where order-by take drop]}]
  (let [id-col (get-in t-map [:key->col :id])
        [where-sql & where-args] (jdbc/-build-where dialect t-map where)
        [order-sql & order-args] (jdbc/-build-order-by dialect t-map order-by)
        ;; MSSQL requires ORDER BY for OFFSET/FETCH, default to id if using pagination
        order-sql (or order-sql (when drop (str "ORDER BY " id-col)))
        sql    (jdbc/-seq->sql "SELECT"
                 (when (and take (not drop)) (str "TOP " take))
                 "* FROM" (jdbc/->safe-name dialect (:table t-map))
                 where-sql
                 order-sql
                 (when drop ["OFFSET" drop "ROWS"
                             (when take ["FETCH NEXT" take " ROWS ONLY"])]))]
    (cons sql (concat where-args order-args))))

(defmethod jdbc/build-upsert-sql :mssql [dialect t-map {:keys [id] :as entity}]
  (let [[fetch-sql & fetch-params] (jdbc/build-fetch-sql dialect t-map id)
        [insert-sql & insert-params] (jdbc/build-insert-sql dialect t-map entity)
        [update-sql & update-params] (jdbc/build-update-sql dialect t-map entity)]
    (cons (str "IF NOT EXISTS (" fetch-sql ") " insert-sql " ELSE " update-sql)
          (concat fetch-params insert-params update-params))))

(defmethod jdbc/->sql-value :mssql [_ type value]
  (if (= :uuid type)
    (str value)
    (jdbc/->sql-value nil type value)))

(defmethod jdbc/auto-int-primary-key :mssql [_] "bigint IDENTITY PRIMARY KEY")

(defmethod jdbc/sql-rename-column :mssql [_db table col-old col-new]
  (str "EXEC sp_rename '" table "." col-old "', " col-new ", 'COLUMN'"))

(defmethod jdbc/column-exists? :mssql [db table column]
  (->> (jdbc/execute! db ["SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = ? AND COLUMN_NAME = ?" table column])
       seq
       boolean))

(defmethod jdbc/existing-tables :mssql [db]
  (->> (jdbc/execute! db ["SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES"])
       (map :TABLE_NAME)
       sort))
