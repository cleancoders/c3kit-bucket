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

(def vec-extension-path "/Users/micahmartin/Library/Python/3.9/lib/python/site-packages/sqlite_vec/vec0")

(def vec-config (assoc config :extensions [vec-extension-path]))

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

(def vectorable
  (schema/merge-schemas
    spec/vectorable
    {:id        {:db {:type "INTEGER PRIMARY KEY AUTOINCREMENT"}}
     :embedding {:type [:float] :db {:type "vec_f32(3)"}}}))

(def multi-vectorable
  {:kind            (schema/kind :multi-vectorable)
   :id              {:type :long :db {:type "INTEGER PRIMARY KEY AUTOINCREMENT"}}
   :name            {:type :string}
   :title-embedding {:type [:float] :db {:type "vec_f32(3)"}}
   :body-embedding  {:type [:float] :db {:type "vec_f32(2)" :distance :cosine}}})

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

      (it "->sql-value converts vector to float32 byte array"
        (let [blob (jdbc/->sql-value :sqlite3 :sqlite-vec [1.0 0.0 0.0])]
          (should (bytes? blob))
          (should= 12 (alength ^bytes blob))))

      (it "->sql-value returns nil for nil vector"
        (should-be-nil (jdbc/->sql-value :sqlite3 :sqlite-vec nil)))

      (it "->sql-param uses plain ? for sqlite-vec type"
        (should= "?" (jdbc/->sql-param :sqlite3 :sqlite-vec "vec_f32(3)")))

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
          (should (bytes? (first args)))))

      (it "generates vec_distance_cosine for <=> operator"
        (let [t-map {:table        "vectorable"
                     :key->col     {:embedding "embedding"}
                     :key->type    {:embedding :sqlite-vec}
                     :key->cast    {:embedding "vec_f32(3)"}
                     :key->distance {:embedding :cosine}}
              [sql & args] (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<=> [1.0 0.0 0.0]]})]
          (should-contain "vec_distance_cosine" sql)
          (should (bytes? (first args)))))

      (it "throws for unsupported <#> operator"
        (let [t-map {:table   "vectorable"
                     :key->col  {:embedding "embedding"}
                     :key->type {:embedding :sqlite-vec}
                     :key->cast {:embedding "vec_f32(3)"}}]
          (should-throw Exception "Unsupported vector operator on sqlite3: <#>"
            (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<#> [1.0 0.0 0.0]]}))))

      (it "generates mixed vector and scalar ORDER BY"
        (let [t-map {:table      "vectorable"
                     :key->col   {:embedding "embedding" :name "name"}
                     :key->type  {:embedding :sqlite-vec :name :string}
                     :key->cast  {:embedding "vec_f32(3)"}}
              [sql & args] (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<-> [1.0 0.0 0.0]] :name :asc})]
          (should-contain "vec_distance_L2" sql)
          (should-contain "name" sql)
          (should-contain "ASC" sql)
          (should (bytes? (first args)))))

      )

    (context "operator-distance metric validation"

      (it "<=> on :distance :l2 schema throws"
        (let [t-map {:table        "vectorable"
                     :key->col     {:embedding "embedding"}
                     :key->type    {:embedding :sqlite-vec}
                     :key->cast    {:embedding "vec_f32(3)"}
                     :key->distance {:embedding :l2}}]
          (should-throw Exception "Operator <=> requires :distance :cosine, but field :embedding is configured as :l2"
            (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<=> [1.0 0.0 0.0]]}))))

      (it "<-> on :distance :cosine schema throws"
        (let [t-map {:table        "vectorable"
                     :key->col     {:embedding "embedding"}
                     :key->type    {:embedding :sqlite-vec}
                     :key->cast    {:embedding "vec_f32(3)"}
                     :key->distance {:embedding :cosine}}]
          (should-throw Exception "Operator <-> requires :distance :l2, but field :embedding is configured as :cosine"
            (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<-> [1.0 0.0 0.0]]}))))

      (it "<#> always throws on sqlite3 regardless of distance"
        (let [t-map {:table        "vectorable"
                     :key->col     {:embedding "embedding"}
                     :key->type    {:embedding :sqlite-vec}
                     :key->cast    {:embedding "vec_f32(3)"}
                     :key->distance {:embedding :l2}}]
          (should-throw Exception "Unsupported vector operator on sqlite3: <#>"
            (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<#> [1.0 0.0 0.0]]}))))

      (it "correct operator succeeds on matching :l2 schema"
        (let [t-map {:table        "vectorable"
                     :key->col     {:embedding "embedding"}
                     :key->type    {:embedding :sqlite-vec}
                     :key->cast    {:embedding "vec_f32(3)"}
                     :key->distance {:embedding :l2}}
              [sql] (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<-> [1.0 0.0 0.0]]})]
          (should-contain "vec_distance_L2" sql)))

      (it "correct operator succeeds on matching :cosine schema"
        (let [t-map {:table        "vectorable"
                     :key->col     {:embedding "embedding"}
                     :key->type    {:embedding :sqlite-vec}
                     :key->cast    {:embedding "vec_f32(3)"}
                     :key->distance {:embedding :cosine}}
              [sql] (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<=> [1.0 0.0 0.0]]})]
          (should-contain "vec_distance_cosine" sql)))

      (it "defaults to :l2 when no :distance configured"
        (let [t-map {:table     "vectorable"
                     :key->col  {:embedding "embedding"}
                     :key->type {:embedding :sqlite-vec}
                     :key->cast {:embedding "vec_f32(3)"}}
              [sql] (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<-> [1.0 0.0 0.0]]})]
          (should-contain "vec_distance_L2" sql)))

      (it "throws <=> when no :distance configured (defaults to :l2)"
        (let [t-map {:table     "vectorable"
                     :key->col  {:embedding "embedding"}
                     :key->type {:embedding :sqlite-vec}
                     :key->cast {:embedding "vec_f32(3)"}}]
          (should-throw Exception "Operator <=> requires :distance :cosine, but field :embedding is configured as :l2"
            (jdbc/-build-order-by :sqlite3 t-map {:embedding ['<=> [1.0 0.0 0.0]]}))))

      (it "compile-mapping populates key->distance from schema :db :distance"
        (let [schema   {:kind      (schema/kind :vectorable)
                        :id        {:type :long :db {:type "INTEGER PRIMARY KEY AUTOINCREMENT"}}
                        :name      {:type :string}
                        :embedding {:type [:float] :db {:type "vec_f32(3)" :distance :cosine}}}
              compiled (jdbc/compile-mapping :sqlite3 schema)]
          (should= {:embedding :cosine} (:key->distance compiled))))

      (it "compile-mapping omits key->distance entries for fields without :distance"
        (let [schema   {:kind      (schema/kind :vectorable)
                        :id        {:type :long :db {:type "INTEGER PRIMARY KEY AUTOINCREMENT"}}
                        :name      {:type :string}
                        :embedding {:type [:float] :db {:type "vec_f32(3)"}}}
              compiled (jdbc/compile-mapping :sqlite3 schema)]
          (should= {} (:key->distance compiled))))

      )

    (context "extensions"

      (it "load-extensions is called during create-db"
        (let [calls (atom [])]
          (with-redefs [jdbc/load-extensions (fn [dialect ds config] (swap! calls conj {:dialect dialect :ds ds :config config}) ds)]
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

      (context "vector CRUD"
        (helper/with-schemas vec-config [vectorable])

        (it "roundtrip: stores and retrieves vector entity"
          (let [saved (api/tx {:kind :vectorable :name "a" :embedding [1.0 0.0 0.0]})]
            (should= "a" (:name saved))
            (should= [1.0 0.0 0.0] (:embedding saved))
            (let [reloaded (api/entity :vectorable (:id saved))]
              (should= [1.0 0.0 0.0] (:embedding reloaded)))))

        (it "deletes a single vectorable entity"
          (let [saved    (api/tx {:kind :vectorable :name "a" :embedding [1.0 0.0 0.0]})
                deleted  (api/delete saved)]
            (should= true (:db/delete? deleted))
            (should-be-nil (api/entity :vectorable (:id saved)))))

        (it "delete-all removes all vectorable entities"
          (api/tx {:kind :vectorable :name "a" :embedding [1.0 0.0 0.0]})
          (api/tx {:kind :vectorable :name "b" :embedding [0.0 1.0 0.0]})
          (should= 2 (count (api/find :vectorable)))
          (api/delete-all :vectorable)
          (should= [] (api/find :vectorable)))

        (it "clear drops and recreates vectorable table"
          (api/tx {:kind :vectorable :name "a" :embedding [1.0 0.0 0.0]})
          (should= 1 (count (api/find :vectorable)))
          (api/clear)
          (should= [] (api/find :vectorable))
          (let [saved (api/tx {:kind :vectorable :name "b" :embedding [0.0 1.0 0.0]})]
            (should= [0.0 1.0 0.0] (:embedding saved))))

        (it "stores and retrieves nil embedding"
          (let [saved (api/tx {:kind :vectorable :name "empty" :embedding nil})]
            (should-be-nil (:embedding saved))
            (let [reloaded (api/entity :vectorable (:id saved))]
              (should-be-nil (:embedding reloaded)))))

        (it "nil embeddings sort last in vector distance ordering"
          (api/tx {:kind :vectorable :name "a" :embedding [1.0 0.0 0.0]})
          (api/tx {:kind :vectorable :name "b" :embedding [0.0 1.0 0.0]})
          (api/tx {:kind :vectorable :name "c" :embedding [0.9 0.1 0.0]})
          (api/tx {:kind :vectorable :name "d" :embedding nil})
          (let [results (api/find :vectorable :order-by {:embedding ['<-> [1.0 0.0 0.0]]})]
            (should= ["a" "c" "b" "d"] (map :name results))))

        (it "mixed vector and scalar ORDER BY"
          (api/tx {:kind :vectorable :name "a" :embedding [1.0 0.0 0.0]})
          (api/tx {:kind :vectorable :name "b" :embedding [0.0 1.0 0.0]})
          (api/tx {:kind :vectorable :name "c" :embedding [0.9 0.1 0.0]})
          (let [results (api/find :vectorable :order-by {:embedding ['<-> [1.0 0.0 0.0]] :name :desc})]
            (should= 3 (count results))
            ;; primary sort: vector distance; secondary: name desc
            (should= "a" (:name (first results)))))

        (it "vector distance with take"
          (api/tx {:kind :vectorable :name "a" :embedding [1.0 0.0 0.0]})
          (api/tx {:kind :vectorable :name "b" :embedding [0.0 1.0 0.0]})
          (api/tx {:kind :vectorable :name "c" :embedding [0.9 0.1 0.0]})
          (let [results (api/find :vectorable :order-by {:embedding ['<-> [1.0 0.0 0.0]]} :take 2)]
            (should= ["a" "c"] (map :name results))))

        )

      (context "multiple vector columns"
        (helper/with-schemas vec-config [multi-vectorable])

        (it "stores and retrieves entity with two vector columns"
          (let [saved (api/tx {:kind :multi-vectorable :name "doc"
                               :title-embedding [1.0 0.0 0.0]
                               :body-embedding  [0.5 0.5]})]
            (should= "doc" (:name saved))
            (should= [1.0 0.0 0.0] (:title-embedding saved))
            (should= [0.5 0.5] (:body-embedding saved))
            (let [reloaded (api/entity :multi-vectorable (:id saved))]
              (should= [1.0 0.0 0.0] (:title-embedding reloaded))
              (should= [0.5 0.5] (:body-embedding reloaded)))))

        (it "orders by title-embedding distance"
          (api/tx {:kind :multi-vectorable :name "a" :title-embedding [1.0 0.0 0.0] :body-embedding [1.0 0.0]})
          (api/tx {:kind :multi-vectorable :name "b" :title-embedding [0.0 1.0 0.0] :body-embedding [0.0 1.0]})
          (api/tx {:kind :multi-vectorable :name "c" :title-embedding [0.9 0.1 0.0] :body-embedding [0.5 0.5]})
          (let [results (api/find :multi-vectorable :order-by {:title-embedding ['<-> [1.0 0.0 0.0]]})]
            (should= ["a" "c" "b"] (map :name results))))

        (it "orders by body-embedding distance (cosine)"
          (api/tx {:kind :multi-vectorable :name "a" :title-embedding [1.0 0.0 0.0] :body-embedding [1.0 0.0]})
          (api/tx {:kind :multi-vectorable :name "b" :title-embedding [0.0 1.0 0.0] :body-embedding [0.0 1.0]})
          (api/tx {:kind :multi-vectorable :name "c" :title-embedding [0.9 0.1 0.0] :body-embedding [0.9 0.1]})
          (let [results (api/find :multi-vectorable :order-by {:body-embedding ['<=> [1.0 0.0]]})]
            (should= ["a" "c" "b"] (map :name results))))

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

        (it "add-attribute! for vector column creates BLOB"
          (let [_      (migrator/-install-schema! @db jdbc-spec/bibelot)
                _      (migrator/-add-attribute! @db :bibelot :embedding {:type [:float] :db {:type "vec_f32(3)"}})
                result (migrator/-installed-schema-legend @db {:bibelot jdbc-spec/bibelot})]
            (should-not-be-nil (-> result :bibelot :embedding))
            (should= "BLOB" (-> result :bibelot :embedding :db :type))))

        (it "install-schema! with vector column"
          (let [_      (migrator/-install-schema! @db vectorable)
                result (migrator/-installed-schema-legend @db {:vectorable vectorable})]
            (should= "BLOB" (-> result :vectorable :embedding :db :type))))

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
