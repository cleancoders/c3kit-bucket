(ns c3kit.bucket.postgres-spec
  (:require [c3kit.apron.log :as log]
            [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.jdbc :as jdbc]
            [c3kit.bucket.jdbc-spec :as jdbc-spec]
            [c3kit.bucket.migrator :as migrator]
            [c3kit.bucket.postgres]
            [c3kit.bucket.spec-helperc :as helper]
            [speclj.core :refer :all]))

(def config {:impl    :jdbc
             :dialect :postgres
             :host    "localhost"
             :port    5432
             :dbtype  "postgresql"
             :dbname  "test"})

(def vectorable (assoc spec/vectorable :embedding {:type [:float] :db {:type "vector(3)"}}))

(declare db)

(with-redefs [spec/bibelot      jdbc-spec/bibelot
              spec/thingy       jdbc-spec/thingy
              spec/disorganized jdbc-spec/disorganized
              spec/vectorable   vectorable]

  (describe "PostgresSQL"

    (around [it] (api/with-safety-off (it)))
    (with-stubs)

    (context "slow"

      ;(tags :slow)
      ;(before (log/debug!))

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
      (spec/order-by-specs config)
      ;; For vectors to work, the pgvector plugin needs to be installed in the test database.
      ;; psql -d test -c "CREATE EXTENSION IF NOT EXISTS vector;"
      (spec/order-by-vector-specs config)



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

      #_(context "vector type"

          (def vectored
            {:kind      (s/kind :vectored)
             :id        {:type :long :db {:type "serial PRIMARY KEY"}}
             :name      {:type :string :db {:type "varchar(255)"}}
             :embedding {:type [:float] :db {:type "vector(3)"}}}) ;; [:float] with pgvector db type

          (helper/with-schemas config [vectored])

          (it "stores and retrieves vectors"
            (let [saved (api/tx {:kind :vectored :name "test" :embedding [0.1 0.2 0.3]})]
              (should= [0.1 0.2 0.3] (:embedding (api/reload saved)))))

          (it "handles nil vectors"
            (let [saved (api/tx {:kind :vectored :name "test" :embedding nil})]
              (should-be-nil (:embedding (api/reload saved)))))

          (it "updates vector value"
            (let [saved   (api/tx {:kind :vectored :name "test" :embedding [0.1 0.2 0.3]})
                  updated (api/tx saved :embedding [0.4 0.5 0.6])]
              (should= [0.4 0.5 0.6] (:embedding (api/reload updated)))))

          (it "retracts vector value"
            (let [saved   (api/tx {:kind :vectored :name "test" :embedding [0.1 0.2 0.3]})
                  updated (api/tx saved :embedding nil)]
              (should-be-nil (:embedding (api/reload updated))))))

      #_(context "order-by"

          (def orderable
            {:kind      (s/kind :orderable)
             :id        {:type :long :db {:type "serial PRIMARY KEY"}}
             :name      {:type :string :db {:type "varchar(255)"}}
             :embedding {:type [:float] :db {:type "vector(3)"}}}) ;; [:float] with pgvector db type

          (helper/with-schemas config [orderable])

          (before
            (api/clear)
            (api/tx {:kind :orderable :name "a" :embedding [1.0 0.0 0.0]})
            (api/tx {:kind :orderable :name "b" :embedding [0.0 1.0 0.0]})
            (api/tx {:kind :orderable :name "c" :embedding [0.9 0.1 0.0]}))

          (it "orders by field ascending"
            (should= ["a" "b" "c"] (map :name (api/find :orderable {:order-by {:name :asc}}))))

          (it "orders by field descending"
            (should= ["c" "b" "a"] (map :name (api/find :orderable {:order-by {:name :desc}}))))

          (it "orders by cosine distance <=>"
            (let [query   [1.0 0.0 0.0]
                  results (api/find :orderable {:order-by {:embedding ['<=> query]}})]
              (should= ["a" "c" "b"] (map :name results))))

          (it "orders by L2 distance <->"
            (let [query   [1.0 0.0 0.0]
                  results (api/find :orderable {:order-by {:embedding ['<-> query]}})]
              (should= ["a" "c" "b"] (map :name results))))

          (it "orders by inner product <#>"
            (let [query   [1.0 0.0 0.0]
                  results (api/find :orderable {:order-by {:embedding ['<#> query]}})]
              (should= ["a" "c" "b"] (map :name results))))

          (it "combines order-by with where clause"
            (let [query   [1.0 0.0 0.0]
                  results (api/find-by :orderable :name ['not= "b"]
                                       {:order-by {:embedding ['<=> query]}})]
              (should= ["a" "c"] (map :name results))))

          (it "combines order-by with take/drop (pagination)"
            (let [query [1.0 0.0 0.0]
                  page1 (api/find :orderable {:order-by {:embedding ['<=> query]} :take 2})
                  page2 (api/find :orderable {:order-by {:embedding ['<=> query]} :take 2 :drop 2})]
              (should= ["a" "c"] (map :name page1))
              (should= ["b"] (map :name page2))))

          (it "handles nil embeddings in ordering"
            (api/tx {:kind :orderable :name "d" :embedding nil})
            (let [query   [1.0 0.0 0.0]
                  results (api/find :orderable {:order-by {:embedding ['<=> query]}})]
              (should= 4 (count results)))))
      )
    )
  )
