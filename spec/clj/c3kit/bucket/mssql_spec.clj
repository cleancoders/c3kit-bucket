(ns c3kit.bucket.mssql-spec
  (:require [c3kit.apron.schema :as schema]
            [c3kit.apron.schema]
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.mssql :as sut]
            [c3kit.bucket.jdbc :as jdbc]
            [c3kit.bucket.jdbc-spec :as jdbc-spec]
            [speclj.core :refer :all]))

(defn new-db []
  (sut/create-db {:host     "localhost"
                  :port     1433
                  :dbtype   "sqlserver"
                  :dbname   "bucketTest"
                  :user     "sa"
                  :password "Pala2023"}))

(def bibelot
  (schema/merge-schemas
    jdbc-spec/bibelot
    {:id    {:db {:type "bigint IDENTITY PRIMARY KEY"}}}))

(def thingy
  (schema/merge-schemas
    jdbc-spec/thingy
    {:id   {:db {:type "int primary key"}}}))

(with-redefs [spec/bibelot bibelot
              spec/thingy  thingy]

  (describe "MS SQL Server"

    (before-all (reset! jdbc/development? true))
    (with-stubs)

    (context "slow"

      (tags :slow)

      (spec/crud-specs (new-db))
      (spec/nil-value-specs (new-db))
      (spec/find-all (new-db))
      (spec/find-by (new-db))
      (spec/reduce-by (new-db))
      (spec/count-all (new-db))
      (spec/count-by (new-db))
      (spec/broken-in-datomic (new-db))

      )
    ))
