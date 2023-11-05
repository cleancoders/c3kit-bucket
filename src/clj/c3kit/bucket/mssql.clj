(ns c3kit.bucket.mssql
  (:require [c3kit.bucket.jdbc :as jdbc]))

(defmethod jdbc/schema->db-type-map :mssql [_]
  {:long      "bigint"
   :int       "int"
   :uuid      "uniqueidentifier"
   :instant   "datetime2"
   :timestamp "datetime2"
   :boolean   "bit"})

(defmethod jdbc/->safe-name :mssql [_ name] (str \[ name \]))

(defmethod jdbc/-build-find-query :mssql [dialect t-map {:keys [where take drop]}]
  (let [id-col (get-in t-map [:key->col :id])
        [where-sql & args] (jdbc/-build-where dialect t-map where)
        sql    (jdbc/-seq->sql "SELECT"
                 (when (and take (not drop)) (str "TOP " take))
                 "* FROM" (jdbc/->safe-name dialect (:table t-map))
                 where-sql
                 (when drop ["ORDER BY" id-col
                             "OFFSET" drop "ROWS"
                             (when take ["FETCH NEXT" take " ROWS ONLY"])]))]
    (cons sql args)))

(defmethod jdbc/build-upsert-sql :mssql [dialect t-map {:keys [id] :as entity}]
  (let [[fetch-sql & fetch-params] (jdbc/build-fetch-sql dialect t-map id)
        [insert-sql & insert-params] (jdbc/build-insert-sql dialect t-map entity)
        [update-sql & update-params] (jdbc/build-update-sql dialect t-map entity)]
    (cons (str "IF NOT EXISTS (" fetch-sql ") " insert-sql " ELSE " update-sql)
          (concat fetch-params insert-params update-params))))


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
