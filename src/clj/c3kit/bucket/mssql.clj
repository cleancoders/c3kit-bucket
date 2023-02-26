(ns c3kit.bucket.mssql
  (:require [c3kit.bucket.jdbc :as jdbc]))

(defmethod jdbc/schema->db-type-map :mssql [_]
  {:long      "bigint"
   :int       "int"
   :uuid      "uniqueidentifier"
   :instant   "datetime2"
   :timestamp "datetime2"
   :boolean   "bit"})

(defmethod jdbc/-build-find-by-query :mssql [dialect t-map {[limit] :keys :as params} kvs]
  (let [kv-pairs (partition 2 kvs)
        select   (str "SELECT " (when limit (str "TOP " limit)) " * FROM " (:table t-map) " ")
        [sql & args] (jdbc/-build-where dialect select t-map kv-pairs)]
    (assert (every? keyword? (map first kv-pairs)) "Attributes must be keywords")
    (cons sql args)))

(defmethod jdbc/build-upsert-sql :mssql [dialect t-map {:keys [id] :as entity}]
  (let [[fetch-sql & fetch-params] (jdbc/build-fetch-sql dialect t-map id)
        [insert-sql & insert-params] (jdbc/build-insert-sql dialect t-map entity)
        [update-sql & update-params] (jdbc/build-update-sql dialect t-map entity)]
    (cons (str "IF NOT EXISTS (" fetch-sql ") " insert-sql " ELSE " update-sql)
          (concat fetch-params insert-params update-params))))

(defn create-db [config]
  (jdbc/create-db (assoc config :dialect :mssql)))