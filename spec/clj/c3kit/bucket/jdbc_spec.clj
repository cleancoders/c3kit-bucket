(ns c3kit.bucket.jdbc-spec
  (:require [c3kit.apron.log :as log]
            [c3kit.apron.schema :as schema]
            [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.h2 :as h2]
            [c3kit.bucket.jdbc :as jdbc]
            [c3kit.bucket.jdbc :as sut]
            [c3kit.bucket.migrator :as migrator]
            [c3kit.bucket.spec-helperc :as helper]
            [speclj.core :refer :all])
  (:import (com.mchange.v2.c3p0 PooledDataSource)))

(def json-entity
  {:kind  (assoc (s/kind :json-entity) :db {:name "json_entity"})
   :id    {:type :int :db {:type "serial PRIMARY KEY"}}
   :stuff {:type :string :db {:type "jsonb"}}})

(def str-id-entity
  {:kind  (assoc (s/kind :str-id-entity) :db {:name "str_id_entity"})
   :id    {:type :string :db {:type "varchar(255) PRIMARY KEY"} :strategy :pre-populated}
   :value {:type :int}})

(def config {:impl    :jdbc
             :dialect :h2
             :jdbcUrl "jdbc:h2:mem:test-db;DB_CLOSE_DELAY=-1;DATABASE_TO_LOWER=TRUE;"})

(def bibelot
  (schema/merge-schemas
    spec/bibelot
    {:id    {:db {:type "serial PRIMARY KEY"}}
     :name  {:db {:type "varchar(42)"}}
     :color {:db {:type "varchar(55)"}}}))

(def disorganized
  (schema/merge-schemas
    spec/disorganized
    {:type {:db {:type "varchar(255)"}}
     :id   {:db {:type "serial PRIMARY KEY"}}
     :name {:db {:type "varchar(255)"}}}))

(def thingy
  (schema/merge-schemas
    spec/thingy
    {:id   {:db {:type "int primary key"} :strategy :pre-populated}
     :foo  {:db {:type "varchar(255)"}}
     :name {:db {:type "varchar(255)"}}}))

(declare db)

(defn regurgitate-spec [db spec]
  (jdbc/drop-table db "foo")
  (let [schema     {:kind (schema/kind :foo) :bar spec}
        legend     {:foo schema}
        _          (jdbc/create-table-from-schema db schema)
        new-legend (migrator/-installed-schema-legend db legend)]
    (get-in new-legend [:foo :bar])))

(defmacro should-regurgitate-spec [db spec]
  `(should= ~spec (regurgitate-spec ~db ~spec)))

(with-redefs [spec/bibelot      bibelot
              spec/thingy       thingy
              spec/disorganized disorganized]
  (describe "JDBC DB (H2)"

    (around [it] (api/with-safety-off (it)))
    (with-stubs)

    (context "sql generation"

      (context "create table"

        (it "name"
          (should= "CREATE TABLE foo ()" (sut/sql-create-table :foo {:kind {:value :foo}}))
          (should= "CREATE TABLE bar ()" (sut/sql-create-table :foo {:kind {:value :foo :db {:name "bar"}}})))

        (it "id"
          (should= "\"id\" int auto_increment PRIMARY KEY" (sut/sql-table-col :foo :id {:type :int :db {:type "int auto_increment PRIMARY KEY"}}))
          (should= "\"eyeD\" uuid" (sut/sql-table-col :foo :id {:type :uuid :db {:name "eyeD"}}))
          (should= "\"eyeD\" varchar" (sut/sql-table-col :foo :id {:type :uuid :db {:name "eyeD" :type "varchar"}})))

        (it "bibelot"
          (should= (str "CREATE TABLE bibelot ("
                        "\"id\" serial PRIMARY KEY,"
                        "\"name\" varchar(42),"
                        "\"size\" int4,"
                        "\"color\" varchar(55)"
                        ")")
                   (sut/sql-create-table :foo bibelot)))

        (it "schema-type to db-type"
          (should= "int4" (sut/schema-type->db-type :foo :int))
          (should= "int4" (sut/schema-type->db-type :foo :long))
          (should= "bool" (sut/schema-type->db-type :foo :boolean)))
        )
      )

    (context "compile schema"

      (it "bibelot"
        (let [compiled (sut/compile-mapping spec/bibelot)]
          (should= "bibelot" (:table compiled))
          (should= {:id "id" :name "name" :size "size" :color "color"} (:key->col compiled))
          (should= {"id" :id "name" :name "size" :size "color" :color} (:col->key compiled))
          (should= {:id :long :name :string :size :long :color :string} (:key->type compiled))
          ))

      (it "json reader"
        (let [compiled (sut/compile-mapping json-entity)]
          (should= "json_entity" (:table compiled))
          (should= {:id "id" :stuff "stuff"} (:key->col compiled))
          (should= {"id" :id "stuff" :stuff} (:col->key compiled))))
      )

    (context "api"

      (spec/crud-specs config)
      (spec/nil-value-specs config)
      (spec/find-specs config)
      (spec/filter-specs config)
      (spec/reduce-specs config)
      (spec/count-specs config)
      (spec/broken-in-datomic config)
      (spec/kind-is-required config)
      (spec/cas config)

      )

    (context "safety"
      (around [it] (with-redefs [api/*safety* true] (it)))

      (it "clear" (should-throw AssertionError (sut/clear (config nil))))
      (it "delete-all" (should-throw AssertionError (sut/delete-all (config nil) :foo)))
      )

    (context "SQL Injection"

      (helper/with-schemas config [spec/bibelot str-id-entity])

      (it "finds by always true"
        (api/tx {:kind :bibelot :name "John" :size 5 :color "Red"})
        (should= [] (api/find-by :bibelot :name "' OR 1 = 1;--")))

      (it "creates an entity with a malicious name"
        (api/tx {:kind :bibelot :name "'" :size 5 :color "Red"}))

      (it "sneaky delete doesn't work"
        (api/tx {:kind :str-id-entity :id "123" :value 1})
        (should-throw (api/delete {:kind :str-id-entity :id "' OR 1 = 1;--"}))
        (should= 1 (api/count :str-id-entity)))

      (it "sneaky deletes with actual id works"
        (api/tx {:kind :str-id-entity :id "' OR 1 = 1;--" :value 1})
        (api/delete {:kind :str-id-entity :id "' OR 1 = 1;--"})
        (should= 0 (api/count :str-id-entity)))
      )

    (context "connection pool"

      (it "doesn't use a pool by default"
        (with-open [db (api/create-db config [])]
          (should= false (instance? PooledDataSource (.-ds db)))))

      (it ":connection-pool? option activates pooling"
        (with-open [db (api/create-db (assoc config :connection-pool? true) [])]
          (should= true (instance? PooledDataSource (.-ds db)))))
      )

    (context "local api"

      (helper/with-schemas config [bibelot thingy])

      (it "execute!"
        (should= {:1 1} (sut/execute-one! @api/impl "SELECT 1"))
        (should= {:?1 1} (sut/execute-one! @api/impl ["SELECT ?" 1]))
        (should= [{:1 1}] (sut/execute! @api/impl "SELECT 1"))
        (should= [{:?1 1}] (sut/execute! @api/impl ["SELECT ?" 1])))

      (it "find-sql"
        (let [red   (api/tx- @api/impl {:kind :bibelot :name "one" :color "red"})
              green (api/tx- @api/impl {:kind :bibelot :name "two" :color "green"})]
          (should= red (first (sut/find-sql- @api/impl :bibelot "SELECT * from bibelot WHERE color='red'")))
          (should= red (first (sut/find-sql- @api/impl :bibelot ["SELECT * from bibelot WHERE color=?" "red"])))
          (should= green (first (sut/find-sql :bibelot "SELECT * from bibelot WHERE color='green'")))))

      (it "tries to update a deleted entity with db-generated ids"
        (let [saved (api/tx {:kind :bibelot :name "thingy"})]
          (api/delete saved)
          (should-be-nil (api/tx saved))))

      )

    (context "schema"

      (context "result-set->table-constraints"

        (it "unique"
          (should= {"id" "UNIQUE"} (h2/result-set->table-constraints [{:key_column_usage/column_name      "id"
                                                                       :table_constraints/constraint_name "migration_pkey"
                                                                       :table_constraints/constraint_type "UNIQUE"}])))

        (it "ignore foreign keys"
          ;; MDM - we don't want foreign key constraints.  The code will maintain integrity.
          (should= {} (h2/result-set->table-constraints [{:key_column_usage/column_name      "id"
                                                          :table_constraints/constraint_name "migration_pkey"
                                                          :table_constraints/constraint_type "FOREIGN KEY"}])))

        (it "ignore unique that applies to multiple columns"
          (should= {} (h2/result-set->table-constraints [{:key_column_usage/column_name      "id"
                                                          :table_constraints/constraint_name "migration_pkey"
                                                          :table_constraints/constraint_type "UNIQUE"}
                                                         {:key_column_usage/column_name      "id"
                                                          :table_constraints/constraint_name "migration_pkey"
                                                          :table_constraints/constraint_type "UNIQUE"}])))
        )

      )

    (context "migrator"

      (with db (api/create-db config []))
      (before (doseq [table (jdbc/existing-tables @db)] (jdbc/drop-table @db table)))

      (it "schema"
        (let [schema (migrator/migration-schema {:impl :jdbc})]
          (should= :migration (-> schema :kind :value))
          (should= :int (-> schema :id :type))
          (should= {:type "serial PRIMARY KEY"} (-> schema :id :db))
          (should= {:type "varchar(255) UNIQUE"} (-> schema :name :db))
          (should= {:type "timestamp"} (-> schema :at :db))))

      (it "installed-schema-legend"
        (let [_      (jdbc/create-table-from-schema @db bibelot)
              result (migrator/-installed-schema-legend @db {:bibelot bibelot})]
          (should= {:type :long :db {:type "serial PRIMARY KEY"}} (-> result :bibelot :id))
          (should= {:type :string :db {:type "varchar(42)"}} (-> result :bibelot :name))
          (should= {:type :long :db {:type "integer"}} (-> result :bibelot :size))
          (should= {:type :string :db {:type "varchar(55)"}} (-> result :bibelot :color))))

      ;(it "installed-schema-legend with renamed column"
      ;  (let [schema (assoc-in bibelot [:color :db :name] "hue")
      ;        _      (jdbc/create-table-from-schema @db schema)
      ;        result (migrator/-installed-schema-legend @db {:bibelot bibelot})]
      ;    (should= {:type :long :db {:type "serial PRIMARY KEY"}} (-> result :bibelot :id))
      ;    (should= {:type :string :db {:type "varchar(42)"}} (-> result :bibelot :name))
      ;    (should= {:type :long :db {:type "integer"}} (-> result :bibelot :size))
      ;    (should= {:type :string :db {:type "varchar(55)"}} (-> result :bibelot :color))))

      (it "install-schema!"
        (let [schema (assoc-in bibelot [:kind :value] :bubble)
              _      (migrator/-install-schema! @db schema)
              result (migrator/-installed-schema-legend @db {:bubble schema})]
          (should= {:type :string :db {:type "varchar(42)"}} (-> result :bubble :name))
          (should= {:type :long :db {:type "integer"}} (-> result :bubble :size))
          (should= {:type :string :db {:type "varchar(55)"}} (-> result :bubble :color))))

      (it "schema-exists?"
        (should= false (migrator/-schema-exists? @db bibelot))
        (migrator/-install-schema! @db bibelot)
        (should= true (migrator/-schema-exists? @db bibelot)))

      (it "column-exists?"
        (should= false (jdbc/column-exists? @db "bibelot" "name"))
        (migrator/-install-schema! @db bibelot)
        (should= true (jdbc/column-exists? @db "bibelot" "name")))

      (it "add-attribute!"
        (let [_      (migrator/-install-schema! @db bibelot)
              _      (migrator/-add-attribute! @db :bibelot :fizz {:type :string :db {:type "varchar(123)"}})
              result (migrator/-installed-schema-legend @db {:bibelot bibelot})]
          (should= {:type :string :db {:type "varchar(123)"}} (-> result :bibelot :fizz))))

      (it "add-attribute! - schema attr"
        (let [_      (migrator/-install-schema! @db bibelot)
              schema (assoc bibelot :fizz {:type :string :db {:type "varchar(123)"}})
              _      (migrator/-add-attribute! @db schema :fizz)
              result (migrator/-installed-schema-legend @db {:bibelot bibelot})]
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

      (it "remove-attribute! - that doesn't exist"
        (migrator/-install-schema! @db bibelot)
        (log/capture-logs
          (should-not-throw (migrator/-remove-attribute! @db :bibelot :fizz))
          (should-not-throw (migrator/-remove-attribute! @db :fizz :bang))))

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

      (it "rename-attribute! - can't change kind"
        (migrator/-install-schema! @db bibelot)
        (should-throw (migrator/-rename-attribute! @db :bibelot :color :widget :hue)))

      (it "rename-attribute! - new attribute exists"
        (migrator/-install-schema! @db bibelot)
        (log/capture-logs
          (should-throw (migrator/-rename-attribute! @db :bibelot :color :bibelot :size))))

      (it "rename-attribute! - existing doesnt exist"
        (migrator/-install-schema! @db bibelot)
        (log/capture-logs
          (should-not-throw (migrator/-rename-attribute! @db :bibelot :blah :bibelot :size))
          (should-not-throw (migrator/-rename-attribute! @db :fizz :blah :fizz :size))))

      (context "schema translation"

        (it "integer"
          (should-regurgitate-spec @db {:type :long :db {:type "integer"}})
          (should-regurgitate-spec @db {:type :long :db {:type "integer UNIQUE"}}))

        (it "string"
          (should-regurgitate-spec @db {:type :string :db {:type "varchar(123)"}}))

        (it "numeric"
          (should-regurgitate-spec @db {:type :bigdec :db {:type "numeric(4,5)"}}))

        )
      )
    )
  )
