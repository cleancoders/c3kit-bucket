(ns c3kit.bucket.mssql-spec
  (:require [c3kit.apron.schema :as schema]
            [c3kit.apron.schema]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.jdbc-spec :as jdbc-spec]
            [c3kit.bucket.mssql]
            [speclj.core :refer :all]))

(def config {:impl     :jdbc
             :dialect  :mssql
             :host     "localhost"
             :port     1433
             :dbtype   "sqlserver"
             :dbname   "bucketTest"
             :user     "sa"
             :password "Pala2023"})

(def bibelot
  (schema/merge-schemas
    jdbc-spec/bibelot
    {:id {:db {:type "bigint IDENTITY PRIMARY KEY"}}}))

(def disorganized
  (schema/merge-schemas
    jdbc-spec/disorganized
    {:id {:db {:type "int IDENTITY PRIMARY KEY"}}}))

(def thingy
  (schema/merge-schemas
    jdbc-spec/thingy
    {:id {:db {:type "int primary key"}}}))

(with-redefs [spec/bibelot      bibelot
              spec/disorganized disorganized
              spec/thingy       thingy]

  (describe "MS SQL Server"

    (around [it] (api/with-safety-off (it)))
    (with-stubs)

    (context "slow"

      (tags :slow)

      (spec/crud-specs config)
      (spec/nil-value-specs config)
      (spec/find-specs config)
      (spec/filter-specs config)
      (spec/query-specs config)
      (spec/reduce-specs config)
      (spec/count-specs config)
      (spec/broken-in-datomic config)
      (spec/cas config)

      )
    )
  )
