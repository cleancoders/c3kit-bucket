(ns c3kit.bucket.postgres-spec
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.jdbc :as jdbc]
            [c3kit.bucket.jdbc-spec :as jdbc-spec]
            [c3kit.bucket.migrator :as migrator]
            [c3kit.bucket.postgres]
            [speclj.core :refer :all]))

(def config {:impl    :jdbc
             :dialect :postgres
             :host    "localhost"
             :port    5432
             :dbtype  "postgresql"
             :dbname  "test"})

(declare db)

(with-redefs [spec/bibelot      jdbc-spec/bibelot
              spec/thingy       jdbc-spec/thingy
              spec/disorganized jdbc-spec/disorganized]

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
      (spec/cas config)
      (jdbc-spec/type-specs config)
      (jdbc-spec/reserved-word-specs config)

      (context "migrator"

        (with db (api/create-db config []))
        (before (doseq [table (jdbc/existing-tables @db)] (jdbc/drop-table @db table)))

        (it "schema"
          (let [schema (migrator/migration-schema config)]
            (should= :migration (-> schema :kind :value))
            (should= :int (-> schema :id :type))
            (should= {:type "serial PRIMARY KEY"} (-> schema :id :db))
            (should= {:type "varchar(255) UNIQUE"} (-> schema :name :db))
            (should= {:type "timestamp"} (-> schema :at :db))))

        (it "installed-schema-legend"
          (let [_      (jdbc/create-table-from-schema @db jdbc-spec/bibelot)
                result (migrator/-installed-schema-legend @db {:bibelot jdbc-spec/bibelot})]
            (should= {:type :long :db {:type "serial PRIMARY KEY"}} (-> result :bibelot :id))
            (should= {:type :string :db {:type "varchar(42)"}} (-> result :bibelot :name))
            (should= {:type :long :db {:type "int4"}} (-> result :bibelot :size))
            (should= {:type :string :db {:type "varchar(55)"}} (-> result :bibelot :color))))

        (it "install-schema!"
          (let [schema (assoc-in jdbc-spec/bibelot [:kind :value] :bubble)
                _      (migrator/-install-schema! @db schema)
                result (migrator/-installed-schema-legend @db {:bubble schema})]
            (should= {:type :string :db {:type "varchar(42)"}} (-> result :bubble :name))
            (should= {:type :long :db {:type "int4"}} (-> result :bubble :size))
            (should= {:type :string :db {:type "varchar(55)"}} (-> result :bubble :color))))

        (it "schema-exists?"
          (should= false (migrator/-schema-exists? @db jdbc-spec/bibelot))
          (migrator/-install-schema! @db jdbc-spec/bibelot)
          (should= true (migrator/-schema-exists? @db jdbc-spec/bibelot)))

        (it "column-exists?"
          (should= false (jdbc/column-exists? @db "bibelot" "name"))
          (migrator/-install-schema! @db jdbc-spec/bibelot)
          (should= true (jdbc/column-exists? @db "bibelot" "name")))

        (it "add-attribute!"
          (let [_      (migrator/-install-schema! @db jdbc-spec/bibelot)
                _      (migrator/-add-attribute! @db :bibelot :fizz {:type :string :db {:type "varchar(123)"}})
                result (migrator/-installed-schema-legend @db {:bibelot jdbc-spec/bibelot})]
            (should= {:type :string :db {:type "varchar(123)"}} (-> result :bibelot :fizz))))

        (it "remove-attribute!"
          (let [_          (migrator/-install-schema! @db jdbc-spec/bibelot)
                db2        (api/create-db config [jdbc-spec/bibelot])
                entity     (api/tx- db2 {:kind :bibelot :name "red" :size 2 :color "red"})
                _          (migrator/-remove-attribute! @db :bibelot :color)
                reloaded   (api/reload- db2 entity)
                new-legend (migrator/-installed-schema-legend @db {:bibelot jdbc-spec/bibelot})]
            (should= nil (:color reloaded))
            (should-not-contain :color (:bibelot new-legend))))

        (it "rename-attribute!"
          (let [_          (migrator/-install-schema! @db jdbc-spec/bibelot)
                db2        (api/create-db config [jdbc-spec/bibelot])
                entity     (api/tx- db2 {:kind :bibelot :name "red" :size 2 :color "red"})
                _          (migrator/-rename-attribute! @db :bibelot :color :bibelot :hue)
                new-legend (migrator/-installed-schema-legend @db {:bibelot jdbc-spec/bibelot})
                reloaded   (api/reload- db2 entity)]
            (should= nil (:color reloaded))
            (should-not-contain :color (:bibelot new-legend))
            (should= :string (get-in new-legend [:bibelot :hue :type]))))

        ;(it "rename-attribute! - can't change kind"
        ;  (migrator/install-schema! @db bibelot)
        ;  (should-throw (migrator/rename-attribute! @db :bibelot :color :widget :hue)))
        ;
        ;(it "rename-attribute! - new attribute exists"
        ;  (migrator/install-schema! @db bibelot)
        ;  (log/capture-logs
        ;    (should-throw (migrator/rename-attribute! @db :bibelot :color :bibelot :size))))

        (context "schema translation"

          (it "integer"
            (jdbc-spec/should-regurgitate-spec @db {:type :long :db {:type "int4"}})
            (jdbc-spec/should-regurgitate-spec @db {:type :long :db {:type "int4 UNIQUE"}}))

          (it "string"
            (jdbc-spec/should-regurgitate-spec @db {:type :string :db {:type "varchar(123)"}}))

          (it "numeric"
            (jdbc-spec/should-regurgitate-spec @db {:type :bigdec :db {:type "numeric(4,3)"}}))

          )
        )
      )
    )
  )
