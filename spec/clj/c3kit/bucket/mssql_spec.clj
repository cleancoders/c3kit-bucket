(ns c3kit.bucket.mssql-spec
  (:require [c3kit.apron.schema :as schema]
            [c3kit.apron.schema]
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.jdbc :as sut]
            [c3kit.bucket.jdbc-spec :as jdbc-spec]
            [speclj.core :refer :all]))

(def test-db-config {:dialect  :mssql
                     :host     "localhost"
                     :port     1433
                     :dbtype   "sqlserver"
                     :dbname   "bucketTest"
                     :user     "sa"
                     :password "Pala2023"})

(def db (sut/create-db test-db-config))
(def ds (.ds db))

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

    (before-all (reset! sut/development? true))
    (with-stubs)

    (context "slow"

      (tags :slow)

      (spec/crud-specs db)
      (spec/nil-value-specs db)
      (spec/find-all db)
      (spec/find-by db)
      (spec/reduce-by db)
      (spec/count-all db)
      (spec/count-by db)

      )
    ))
