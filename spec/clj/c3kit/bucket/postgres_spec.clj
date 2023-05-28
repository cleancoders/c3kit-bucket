(ns c3kit.bucket.postgres-spec
  (:require [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.jdbc-spec :as jdbc-spec]
            [c3kit.bucket.spec-helperc :as helper]
            [speclj.core :refer :all]))

(def reserved-word-entity
  {:kind  (assoc (s/kind :reserved-word-entity) :db {:table "reserved_word_entity"})
   :id    {:type :int :db {:type "SERIAL PRIMARY KEY"}}
   :limit {:type :int}})                                    ;; limit is a reserved word in psql

(def config {:impl    :jdbc
             :dialect :postgres
             :host    "localhost"
             :port    5432
             :dbtype  "postgresql"
             :dbname  "test"})

(with-redefs [spec/bibelot jdbc-spec/bibelot
              spec/thingy  jdbc-spec/thingy]

  (describe "PostgresSQL"

    (around [it] (api/with-safety-off (it)))
    (with-stubs)

    (context "slow"

      (tags :slow)

      (spec/crud-specs config)
      (spec/nil-value-specs config)
      (spec/find-specs config)
      (spec/filter-specs config)
      (spec/reduce-specs config)
      (spec/count-specs config)
      (spec/broken-in-datomic config)

      (context "column with reserved word as name"

        (helper/with-schemas config [reserved-word-entity])

        (it "crud"
          (let [e (api/tx {:kind :reserved-word-entity :limit 123})]
            (should= 123 (:limit e))
            (should= 123 (:limit (api/reload e)))
            (api/delete e)
            (should= nil (api/reload e))))

        (it "search"
          (let [e (api/tx {:kind :reserved-word-entity :limit 123})]
            (should= e (api/ffind-by :reserved-word-entity :limit 123))))
        )
      )

    )
  )
