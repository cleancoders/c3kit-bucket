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
