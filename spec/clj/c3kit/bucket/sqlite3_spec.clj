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
            [speclj.core :refer :all])
  (:import (java.nio ByteBuffer ByteOrder)))

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

    (context "vector type detection"

      (it "spec->db-type returns :sqlite-vec for vec_f32 spec"
        (should= :sqlite-vec (jdbc/spec->db-type :sqlite3 {:type :seq :db {:type "vec_f32(3)"}})))

      (it "spec->db-type returns :sqlite-vec for vec_f32 without dimension"
        (should= :sqlite-vec (jdbc/spec->db-type :sqlite3 {:type :seq :db {:type "vec_f32"}})))

      (it "spec->db-type returns default type for non-vector spec"
        (should= :string (jdbc/spec->db-type :sqlite3 {:type :string :db {:type "TEXT"}}))
        (should= :long (jdbc/spec->db-type :sqlite3 {:type :long :db {:type "INTEGER"}})))

      (it "spec->db-cast returns db type for vector fields"
        (should= "vec_f32(3)" (jdbc/spec->db-cast :sqlite3 {:type :seq :db {:type "vec_f32(3)"}})))

      (it "spec->db-cast returns nil for non-vector fields"
        (should-be-nil (jdbc/spec->db-cast :sqlite3 {:type :string :db {:type "TEXT"}}))
        (should-be-nil (jdbc/spec->db-cast :sqlite3 {:type :long})))

      )

    (context "vector serialization"

      (it "->sql-value converts vector to JSON string"
        (should= "[1.0,0.0,0.0]" (jdbc/->sql-value :sqlite3 :sqlite-vec [1.0 0.0 0.0])))

      (it "->sql-value returns nil for nil vector"
        (should-be-nil (jdbc/->sql-value :sqlite3 :sqlite-vec nil)))

      (it "->sql-param wraps with vec_f32 for sqlite-vec type"
        (should= "vec_f32(?)" (jdbc/->sql-param :sqlite3 :sqlite-vec "vec_f32(3)")))

      (it "->sql-param uses default CAST for non-vector types"
        (should= "CAST(? AS INTEGER)" (jdbc/->sql-param :sqlite3 :long "INTEGER")))

      (it "->sql-param returns plain ? when no dialect type or cast"
        (should= "?" (jdbc/->sql-param :sqlite3 :unknown-type nil)))

      (it "DDL uses BLOB for vector column type"
        (should= "BLOB" (jdbc/sql-col-type :sqlite3 {:type :seq :db {:type "vec_f32(3)"}})))

      (it "DDL uses normal type for non-vector columns"
        (should= "TEXT" (jdbc/sql-col-type :sqlite3 {:type :string}))
        (should= "varchar(42)" (jdbc/sql-col-type :sqlite3 {:type :string :db {:type "varchar(42)"}})))

      )

    (context "vector deserialization"

      (it "decodes float32 BLOB to vector of doubles"
        (let [bb    (doto (ByteBuffer/allocate 12)
                      (.order ByteOrder/LITTLE_ENDIAN)
                      (.putFloat (float 1.0))
                      (.putFloat (float 0.0))
                      (.putFloat (float 0.0)))
              blob  (.array bb)]
          (should= [1.0 0.0 0.0] (jdbc/<-sql-value-for-dialect :sqlite3 :sqlite-vec blob))))

      (it "returns nil for nil value"
        (should-be-nil (jdbc/<-sql-value-for-dialect :sqlite3 :sqlite-vec nil)))

      (it "passes through non-vector types unchanged"
        (should= "hello" (jdbc/<-sql-value-for-dialect :sqlite3 :string "hello"))
        (should= 42 (jdbc/<-sql-value-for-dialect :sqlite3 :long 42)))

      )

    (context "vector ORDER BY SQL generation"

      (it "generates vec_distance_L2 for <-> operator"
        (let [t-map {:table   "vectorable"
                     :key->col  {:embedding "embedding"}
                     :key->type {:embedding :sqlite-vec}
                     :key->cast {:embedding "vec_f32(3)"}}
              [sql & args] (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<-> [1.0 0.0 0.0]]})]
          (should-contain "vec_distance_L2" sql)
          (should-contain "vec_f32(?)" sql)
          (should= "[1.0,0.0,0.0]" (first args))))

      (it "generates vec_distance_cosine for <=> operator"
        (let [t-map {:table   "vectorable"
                     :key->col  {:embedding "embedding"}
                     :key->type {:embedding :sqlite-vec}
                     :key->cast {:embedding "vec_f32(3)"}}
              [sql & args] (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<=> [1.0 0.0 0.0]]})]
          (should-contain "vec_distance_cosine" sql)
          (should-contain "vec_f32(?)" sql)))

      (it "throws for unsupported <#> operator"
        (let [t-map {:table   "vectorable"
                     :key->col  {:embedding "embedding"}
                     :key->type {:embedding :sqlite-vec}
                     :key->cast {:embedding "vec_f32(3)"}}]
          (should-throw Exception "Unsupported vector operator on sqlite3: <#>"
            (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<#> [1.0 0.0 0.0]]}))))

      )

    (context "extensions"

      (it "load-extensions is called during create-db"
        (let [calls (atom [])]
          (with-redefs [jdbc/load-extensions (fn [dialect ds config] (swap! calls conj {:dialect dialect :ds ds :config config}))]
            (let [db (api/create-db config [])]
              (try
                (should= 1 (count @calls))
                (should= :sqlite3 (:dialect (first @calls)))
                (should= config (:config (first @calls)))
                (finally (api/close db)))))))

      (it "throws meaningful error for invalid extension path"
        (let [bad-config (assoc config :extensions ["/nonexistent/path/to/vec0"])]
          (should-throw Exception
            (api/create-db bad-config []))))

      )

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
