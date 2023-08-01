(ns c3kit.bucket.jdbc-spec
  (:require [c3kit.apron.schema :as schema]
            [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.h2]
            [c3kit.bucket.jdbc :as sut]
            [c3kit.bucket.spec-helperc :as helper]
            [speclj.core :refer :all]))

(def json-entity
  {:kind  (assoc (s/kind :json-entity) :db {:table "json_entity"})
   :id    {:type :int :db {:type "serial PRIMARY KEY"}}
   :stuff {:type :string :db {:type "jsonb"}}})

(def str-id-entity
  {:kind  (assoc (s/kind :str-id-entity) :db {:table "str_id_entity"})
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

(def thingy
  (schema/merge-schemas
    spec/thingy
    {:id   {:db {:type "int primary key"} :strategy :pre-populated}
     :foo  {:db {:type "varchar(255)"}}
     :name {:db {:type "varchar(255)"}}}))

(with-redefs [spec/bibelot bibelot
              spec/thingy  thingy]
  (describe "JDBC DB"

    (around [it] (api/with-safety-off (it)))
    (with-stubs)

    (context "sql generation"

      (context "create table"

        (it "name"
          (should= "CREATE TABLE foo ()" (sut/sql-create-table :foo {:kind {:value :foo}}))
          (should= "CREATE TABLE bar ()" (sut/sql-create-table :foo {:kind {:value :foo :db {:table "bar"}}})))

        (it "id"
          (should= "\"id\" int auto_increment PRIMARY KEY" (sut/sql-table-col :foo :id {:type :int :db {:type "int auto_increment PRIMARY KEY"}}))
          (should= "\"eyeD\" uuid" (sut/sql-table-col :foo :id {:type :uuid :db {:column "eyeD"}}))
          (should= "\"eyeD\" varchar" (sut/sql-table-col :foo :id {:type :uuid :db {:column "eyeD" :type "varchar"}})))

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
      (it "delete-all" (should-throw AssertionError (sut/delete-all (config nil) :foo))))

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

      )
    )
  )
