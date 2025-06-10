(ns c3kit.bucket.sqlite3-spec
  (:require [c3kit.apron.schema :as schema]
            [c3kit.apron.schema]
            [c3kit.apron.time :as time]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.jdbc :as jdbc]
            [c3kit.bucket.jdbc-spec :as jdbc-spec]
            [c3kit.bucket.migrator :as migrator]
            [c3kit.bucket.spec-helperc :as helper]
            [c3kit.bucket.sqlite3]
            [speclj.core :refer :all]))

(def config {:impl    :jdbc
             :dialect :sqlite3
             :dbtype  "sqlite"
             :dbname  "sqlite_test.db"})

(def auto-id {:id {:db {:type "INTEGER PRIMARY KEY AUTOINCREMENT"}}})

(def reserved-word-entity (schema/merge-schemas jdbc-spec/reserved-word-entity auto-id))
(def bibelot (schema/merge-schemas jdbc-spec/bibelot auto-id))
(def disorganized (schema/merge-schemas jdbc-spec/disorganized auto-id))
(def variform (schema/merge-schemas jdbc-spec/variform auto-id))

(def thingy
  (schema/merge-schemas
    jdbc-spec/thingy
    {:id      {:db {:type "INTEGER PRIMARY KEY" :strategy :pre-populated}}
     :truthy? {:db {:column "truthy"}}}))

(defmacro test-type-conversion [from to]
  `(it ~from
     (should= ~to (jdbc/->sql-type :sqlite3 ~from))))

(declare db)

(with-redefs [spec/bibelot                   bibelot
              spec/thingy                    thingy
              spec/disorganized              disorganized
              jdbc-spec/reserved-word-entity reserved-word-entity
              jdbc-spec/variform             variform]

  (describe "SQLite3"

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
      (jdbc-spec/reserved-word-specs config)
      (jdbc-spec/type-specs config)

      (context "db specific"
        (helper/with-schemas config [thingy])

        (it "reads and writes instants"
          (let [now    (time/now)
                thingy (api/tx {:kind :thingy :id 1 :bang now})]
            (should= now (:bang thingy))))

        (it "reads and writes booleans"
          (let [now    (time/now)
                thingy (api/tx {:kind :thingy :id 1 :bang now})]
            (should= now (:bang thingy))))

        )

      (context "->sql-type"
        (test-type-conversion :bigdec "REAL")
        (test-type-conversion :boolean "INTEGER")
        (test-type-conversion :date "INTEGER")
        (test-type-conversion :double "REAL")
        (test-type-conversion :float "REAL")
        (test-type-conversion :instant "INTEGER")
        (test-type-conversion :int "INTEGER")
        (test-type-conversion :keyword "TEXT")
        (test-type-conversion :kw-ref "TEXT")
        (test-type-conversion :long "INTEGER")
        (test-type-conversion :ref "INTEGER")
        (test-type-conversion :string "TEXT")
        (test-type-conversion :timestamp "INTEGER")
        (test-type-conversion :uuid "TEXT")
        )

      (context "migrator"

        (with db (api/create-db config []))
        (before (doseq [table (jdbc/existing-tables @db)] (jdbc/drop-table @db table)))

        (it "schema"
          (let [schema (migrator/migration-schema config)]
            (should= :migration (-> schema :kind :value))
            (should= :int (-> schema :id :type))
            (should= {:type "INTEGER PRIMARY KEY AUTOINCREMENT"} (-> schema :id :db))
            (should= {:type "varchar(255) UNIQUE"} (-> schema :name :db))
            (should= {:type "INTEGER"} (-> schema :at :db))))

        (it "installed-schema-legend"
          (let [_      (jdbc/create-table-from-schema @db bibelot)
                result (migrator/-installed-schema-legend @db {:bibelot bibelot})]
            (should= {:type :long :db {:type "INTEGER PRIMARY KEY"}} (-> result :bibelot :id))
            (should= {:type :string :db {:type "varchar(42)"}} (-> result :bibelot :name))
            (should= {:type :long :db {:type "INTEGER"}} (-> result :bibelot :size))
            (should= {:type :string :db {:type "varchar(55)"}} (-> result :bibelot :color))))

        (it "install-schema!"
          (let [schema (assoc-in jdbc-spec/bibelot [:kind :value] :bubble)
                _      (migrator/-install-schema! @db schema)
                result (migrator/-installed-schema-legend @db {:bubble schema})]
            (should= {:type :string :db {:type "varchar(42)"}} (-> result :bubble :name))
            (should= {:type :long :db {:type "INTEGER"}} (-> result :bubble :size))
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
          (let [_          (migrator/-install-schema! @db bibelot)
                db2        (api/create-db config [bibelot])
                entity     (api/tx- db2 {:kind :bibelot :name "red" :size 2 :color "red"})
                _          (migrator/-remove-attribute! @db :bibelot :color)
                reloaded   (api/reload- db2 entity)
                new-legend (migrator/-installed-schema-legend @db {:bibelot bibelot})]
            (should= nil (:color reloaded))
            (should-not-contain :color (:bibelot new-legend))))

        (it "rename-attribute!"
          (let [_          (migrator/-install-schema! @db bibelot)
                db2        (api/create-db config [bibelot])
                entity     (api/tx- db2 {:kind :bibelot :name "red" :size 2 :color "red"})
                _          (migrator/-rename-attribute! @db :bibelot :color :bibelot :hue)
                new-legend (migrator/-installed-schema-legend @db {:bibelot bibelot})
                reloaded   (api/reload- db2 entity)]
            (should= nil (:color reloaded))
            (should-not-contain :color (:bibelot new-legend))
            (should= :string (get-in new-legend [:bibelot :hue :type]))))

        (context "schema translation"

          (it "integer"
            (jdbc-spec/should-regurgitate-spec @db {:type :long :db {:type "int4"}}))

          (it "float"
            (jdbc-spec/should-regurgitate-spec @db {:type :float :db {:type "REAL"}}))

          (it "string"
            (jdbc-spec/should-regurgitate-spec @db {:type :string :db {:type "varchar(123)"}}))

          (it "numeric"
            (jdbc-spec/should-regurgitate-spec @db {:type :bigdec :db {:type "numeric(4,3)"}}))

          (it "primary key"
            (jdbc-spec/should-regurgitate-spec @db {:type :long :db {:type "INTEGER PRIMARY KEY"}}))

          (it "unique"
            (jdbc-spec/should-regurgitate-spec @db {:type :long :db {:type "INTEGER UNIQUE"}}))

          (it "not null"
            (jdbc-spec/should-regurgitate-spec @db {:type :long :db {:type "INTEGER NOT NULL"}}))

          (it "default value"
            (jdbc-spec/should-regurgitate-spec @db {:type :long :db {:type "INTEGER DEFAULT 4"}}))

          (it "hidden"
            (jdbc-spec/should-regurgitate-spec @db {:type :string :db {:type "TEXT HIDDEN"}}))
          )
        )
      )
    )
  )
