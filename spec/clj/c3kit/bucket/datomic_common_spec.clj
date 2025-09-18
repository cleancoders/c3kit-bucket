(ns c3kit.bucket.datomic-common-spec
  (:require [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.datomic-common :as sut]
            [speclj.core :refer :all]))

(def config {:impl :datomic :uri "datomic:mem://test"})
(declare db)

(describe "Datomic Common"
  (with-stubs)
  (around [it] (api/with-safety-off (it)))

  (context "schema"

    (context "spec->attribute"

      (it "simple string"
        (let [attribute (sut/spec->attribute :foo :name {:type :string} true)]
          (should= :foo/name (:db/ident attribute))
          (should= :db.type/string (:db/valueType attribute))
          (should= :db.cardinality/one (:db/cardinality attribute))))

      (it "long"
        (let [attribute (sut/spec->attribute :foo :names {:type :long} true)]
          (should= :db.type/long (:db/valueType attribute))))

      (it "keyword-ref"
        (let [attribute (sut/spec->attribute :foo :temper {:type :kw-ref} true)]
          (should= :db.type/ref (:db/valueType attribute))))

      (it "many strings"
        (let [attribute (sut/spec->attribute :foo :names {:type [:string]} true)]
          (should= :db.type/string (:db/valueType attribute))
          (should= :db.cardinality/many (:db/cardinality attribute))))

      (it "indexed"
        (let [attribute (sut/spec->attribute :foo :names {:type :string} true)]
          (should= false (:db/index attribute)))
        (let [attribute (sut/spec->attribute :foo :names {:type :string :db [:index]} true)]
          (should= true (:db/index attribute))))

      (it "uniqueness"
        (let [attribute (sut/spec->attribute :foo :name {:type :string :db []} true)]
          (should= nil (:db/unique attribute)))
        (let [attribute (sut/spec->attribute :foo :name {:type :string :db [:unique-value]} true)]
          (should= :db.unique/value (:db/unique attribute)))
        (let [attribute (sut/spec->attribute :foo :name {:type :string :db [:unique-identity]} true)]
          (should= :db.unique/identity (:db/unique attribute))))

      (it "component"
        (let [attribute (sut/spec->attribute :foo :name {:type :ref :db []} true)]
          (should= false (:db/isComponent attribute)))
        (let [attribute (sut/spec->attribute :foo :name {:type :ref :db [:component]} true)]
          (should= true (:db/isComponent attribute))))

      (it "history"
        (let [attribute (sut/spec->attribute :foo :name {:type :ref :db []} true)]
          (should= false (:db/noHistory attribute)))
        (let [attribute (sut/spec->attribute :foo :name {:type :ref :db [:no-history]} true)]
          (should= true (:db/noHistory attribute))))

      (it "fulltext"
        (let [attribute (sut/spec->attribute :foo :name {:type :string :db []} true)]
          (should= false (:db/fulltext attribute)))
        (let [attribute (sut/spec->attribute :foo :name {:type :string :db [:fulltext]} true)]
          (should= true (:db/fulltext attribute))))
      )

    (context "attribute->spec"

      (it "ignores any without a valueType, which could be just the db"
        (should-be-nil (sut/attribute->spec {:db/ident :doodle})))

      (it "simple string"
        (let [spec (sut/attribute->spec {:db/ident       :foo/name
                                         :db/valueType   :db.type/string
                                         :db/cardinality :db.cardinality/one})]
          (should= [:foo :name {:type :string}] spec)))

      (it "long"
        (let [spec (sut/attribute->spec {:db/ident     :foo/size
                                         :db/valueType :db.type/long})]
          (should= [:foo :size {:type :long}] spec)))

      (it "ref"
        (let [spec (sut/attribute->spec {:db/ident     :foo/bar
                                         :db/valueType :db.type/ref})]
          (should= [:foo :bar {:type :ref}] spec)))

      (it "many strings"
        (let [spec (sut/attribute->spec {:db/ident       :foo/bar
                                         :db/valueType   :db.type/string
                                         :db/cardinality :db.cardinality/many})]
          (should= [:foo :bar {:type [:string]}] spec)))

      (it "index"
        (let [spec (sut/attribute->spec {:db/ident     :foo/bar
                                         :db/valueType :db.type/string
                                         :db/index     true})]
          (should= [:foo :bar {:type :string :db [:index]}] spec)))

      (it "unique"
        (let [spec (sut/attribute->spec {:db/ident     :foo/bar
                                         :db/valueType :db.type/string
                                         :db/unique    :db.unique/identity})]
          (should= [:foo :bar {:type :string :db [:unique-identity]}] spec))
        (let [spec (sut/attribute->spec {:db/ident     :foo/bar
                                         :db/valueType :db.type/string
                                         :db/unique    :db.unique/value})]
          (should= [:foo :bar {:type :string :db [:unique-value]}] spec)))

      (it "component"
        (let [spec (sut/attribute->spec {:db/ident       :foo/bar
                                         :db/valueType   :db.type/string
                                         :db/isComponent true})]
          (should= [:foo :bar {:type :string :db [:component]}] spec)))

      (it "history"
        (let [spec (sut/attribute->spec {:db/ident     :foo/bar
                                         :db/valueType :db.type/string
                                         :db/noHistory true})]
          (should= [:foo :bar {:type :string :db [:no-history]}] spec)))

      (it "fulltext"
        (let [spec (sut/attribute->spec {:db/ident     :foo/bar
                                         :db/valueType :db.type/string
                                         :db/fulltext  true})]
          (should= [:foo :bar {:type :string :db [:fulltext]}] spec)))

      (it "indexed"
        (let [attribute (sut/spec->attribute :foo :names {:type :string} false)]
          (should-not-contain :db/index attribute))
        (let [attribute (sut/spec->attribute :foo :names {:type :string :db [:index]} true)]
          (should-contain :db/index attribute)))

      )

    (it "entity"
      (let [schema (sut/->entity-schema {:kind (s/kind :bar)
                                         :id   s/id
                                         :fizz {:type :string}
                                         :bang {:type :long}}
                                        true)]
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
    ))