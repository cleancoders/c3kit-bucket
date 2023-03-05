(ns c3kit.bucket.datomic-spec
  (:require [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.datomic :as sut]
            [c3kit.bucket.spec-helperc :as helper]
            [speclj.core :refer :all]))

(def config {:uri "datomic:mem://test"})
(defn new-db [] (sut/create-db config))

(describe "Datomic"

  (with-stubs)
  (around [it] (api/with-safety-off (it)))

  (context "api"
    (spec/crud-specs (new-db))
    (spec/nil-value-specs (new-db))
    (spec/find-by (new-db))
    (spec/reduce-by (new-db))
    (spec/count-all (new-db))
    (spec/count-by (new-db))
    (spec/kind-is-optional (new-db))
    )

  (context "safety"
    (around [it] (with-redefs [api/*safety* true] (it)))

    (it "clear" (should-throw AssertionError (sut/clear (new-db))))
    (it "delete-all" (should-throw AssertionError (sut/delete-all (new-db) :foo))))

  (context "kind is optional"

    (helper/with-schemas (new-db) [spec/bibelot spec/thingy])

    (it "entity"
      (let [foo (api/tx {:kind :bibelot :name "foo"})]
        (should= foo (api/entity (:id foo)))
        (should= nil (api/entity :thingy (:id foo)))))

    (it "entity!"
      (let [foo (api/tx {:kind :bibelot :name "foo"})]
        (should= foo (api/entity! (:id foo)))
        (should-throw (api/entity! :thingy (:id foo)))))
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
      (let [db (sut/create-db (dissoc config :partition))]
        (should= :db.part/user (sut/partition-name db))))

    (it "in config"
      (let [db (sut/create-db (assoc config :partition :test))]
        (should= :test (sut/partition-name db))))

    (it "schema"
      (should= [{:db/id "test", :db/ident :test} [:db/add :db.part/db :db.install/partition "test"]] (sut/partition-schema :test))
      (should= [{:db/id "newbie", :db/ident :newbie} [:db/add :db.part/db :db.install/partition "newbie"]] (sut/partition-schema :newbie)))

    )
  )
