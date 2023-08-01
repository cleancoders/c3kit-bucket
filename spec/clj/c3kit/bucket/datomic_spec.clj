(ns c3kit.bucket.datomic-spec
  (:require [c3kit.apron.log :as log]
            [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.datomic :as sut]
            [speclj.core :refer :all]))

(def config {:impl :datomic :uri "datomic:mem://test"})
(declare db)

(describe "Datomic"

  (with-stubs)
  (around [it] (api/with-safety-off (it)))

  (context "api"
    (spec/crud-specs config)
    (spec/nil-value-specs config)
    (spec/find-specs config)
    (spec/filter-specs config)
    (spec/reduce-specs config)
    (spec/count-specs config)
    (spec/kind-in-entity-is-optional config)
    (spec/multi-value-fields config)
    (spec/cas config)
    )

  (context "safety"
    (around [it] (with-redefs [api/*safety* true] (it)))

    (it "clear" (should-throw AssertionError (sut/clear config)))
    (it "delete-all" (should-throw AssertionError (sut/delete-all config :foo))))

  (context "unique behavior"

    (with db (api/create-db config [spec/bibelot]))

    (it "one kv with nil value"
      (log/capture-logs
        (should= [] (api/find- @db :bibelot :where {:name nil})))
      (should-contain "search for nil value (:bibelot :name), returning no results." (log/captured-logs-str)))

    )

  (context "schema"

    (context "attributes"

      (it "simple string"
        (let [attribute (sut/build-attribute :foo [:name :string])]
          ;(should-contain :db/id attribute)
          (should= :foo/name (:db/ident attribute))
          (should= :db.type/string (:db/valueType attribute))
          (should= :db.cardinality/one (:db/cardinality attribute))
          ;(should= :db.part/db (:db.install/_attribute attribute))
          ))

      (it "long"
        (let [attribute (sut/build-attribute :foo [:names :long :many])]
          (should= :db.type/long (:db/valueType attribute))))

      (it "keyword-ref"
        (let [attribute (sut/build-attribute :foo [:temper :kw-ref :many])]
          (should= :db.type/ref (:db/valueType attribute))))

      (it "many strings"
        (let [attribute (sut/build-attribute :foo [:names :string :many])]
          (should= :db.cardinality/many (:db/cardinality attribute))))

      (it "indexed"
        (let [attribute (sut/build-attribute :foo [:names :string])]
          (should= false (:db/index attribute))
          (let [attribute (sut/build-attribute :foo [:names :string :index])]
            (should= true (:db/index attribute)))))

      (it "uniqueness"
        (let [attribute (sut/build-attribute :foo [:name :string])]
          (should= nil (:db/unique attribute)))
        (let [attribute (sut/build-attribute :foo [:name :string :unique-value])]
          (should= :db.unique/value (:db/unique attribute)))
        (let [attribute (sut/build-attribute :foo [:name :string :unique-identity])]
          (should= :db.unique/identity (:db/unique attribute))))

      (it "component"
        (let [attribute (sut/build-attribute :foo [:name :ref])]
          (should= false (:db/isComponent attribute)))
        (let [attribute (sut/build-attribute :foo [:name :ref :component])]
          (should= true (:db/isComponent attribute))))

      (it "history"
        (let [attribute (sut/build-attribute :foo [:name :ref])]
          (should= false (:db/noHistory attribute)))
        (let [attribute (sut/build-attribute :foo [:name :ref :no-history])]
          (should= true (:db/noHistory attribute))))

      (it "fulltext"
        (let [attribute (sut/build-attribute :foo [:name :string])]
          (should= false (:db/fulltext attribute)))
        (let [attribute (sut/build-attribute :foo [:name :string :fulltext])]
          (should= true (:db/fulltext attribute))))
      )

    (it "entity"
      (let [schema (sut/->entity-schema {:kind (s/kind :bar)
                                         :id s/id
                                         :fizz {:type :string}
                                         :bang {:type :long}})]
        (should= 2 (count schema))
        (should= :bar/fizz (:db/ident (first schema)))
        (should= :db.type/string (:db/valueType (first schema)))
        (should= :bar/bang (:db/ident (second schema)))
        (should= :db.type/long (:db/valueType (second schema)))))

    (it "enum schema"
      (let [schema (sut/->enum-schema {:enum :thing :values [:foo :bar]})]
        (should= 2 (count schema))
        (should-contain {:db/ident :thing/foo} schema)
        (should-contain {:db/ident :thing/bar} schema)))
    )

  (context "partition"

    (it "default"
      (let [db (api/create-db (dissoc config :partition) nil)]
        (should= :db.part/user (sut/partition-name db))))

    (it "in config"
      (let [db (api/create-db (assoc config :partition :test) nil)]
        (should= :test (sut/partition-name db))))

    (it "schema"
      (should= [{:db/id "test", :db/ident :test} [:db/add :db.part/db :db.install/partition "test"]] (sut/partition-schema :test))
      (should= [{:db/id "newbie", :db/ident :newbie} [:db/add :db.part/db :db.install/partition "newbie"]] (sut/partition-schema :newbie)))

    )
  )
