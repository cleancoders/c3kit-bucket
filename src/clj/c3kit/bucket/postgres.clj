(ns c3kit.bucket.postgres
  (:require [c3kit.bucket.jdbc :as jdbc]
            [clojure.string :as str]))

(defmethod jdbc/schema->db-type-map :default [_]
  {:int     "int4"
   :long    "int4"
   :boolean "bool"
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
