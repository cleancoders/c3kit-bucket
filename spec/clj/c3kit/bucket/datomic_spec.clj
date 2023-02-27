(ns c3kit.bucket.datomic-spec
  (:require [c3kit.apron.schema :as s]
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.datomic :as sut]
            [speclj.core :refer :all]))

(def uri "datomic:mem://test")

(describe "Datomic"

  (with-stubs)
  (before-all (reset! sut/development? true))

  (context "api"

    ;(spec/crud-specs (sut/create-db uri))
    ;(spec/nil-value-specs db)
    ;(spec/find-all db)
    ;(spec/find-by db)
    ;(spec/reduce-by db)
    ;(spec/count-all db)
    ;(spec/count-by db)

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
  )
