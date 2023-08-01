(ns c3kit.bucket.datomic-spec
  (:require [c3kit.apron.log :as log]
            [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.datomic :as sut]
            [c3kit.bucket.migrator :as migrator]
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

    (context "spec->attribute"

      (it "simple string"
        (let [attribute (sut/spec->attribute :foo :name {:type :string})]
          (should= :foo/name (:db/ident attribute))
          (should= :db.type/string (:db/valueType attribute))
          (should= :db.cardinality/one (:db/cardinality attribute))))

      (it "long"
        (let [attribute (sut/spec->attribute :foo :names {:type :long})]
          (should= :db.type/long (:db/valueType attribute))))

      (it "keyword-ref"
        (let [attribute (sut/spec->attribute :foo :temper {:type :kw-ref})]
          (should= :db.type/ref (:db/valueType attribute))))

      (it "many strings"
        (let [attribute (sut/spec->attribute :foo :names {:type [:string]})]
          (should= :db.type/string (:db/valueType attribute))
          (should= :db.cardinality/many (:db/cardinality attribute))))

      (it "indexed"
        (let [attribute (sut/spec->attribute :foo :names {:type :string})]
          (should= false (:db/index attribute)))
        (let [attribute (sut/spec->attribute :foo :names {:type :string :db [:index]})]
          (should= true (:db/index attribute))))

      (it "uniqueness"
        (let [attribute (sut/spec->attribute :foo :name {:type :string :db []})]
          (should= nil (:db/unique attribute)))
        (let [attribute (sut/spec->attribute :foo :name {:type :string :db [:unique-value]})]
          (should= :db.unique/value (:db/unique attribute)))
        (let [attribute (sut/spec->attribute :foo :name {:type :string :db [:unique-identity]})]
          (should= :db.unique/identity (:db/unique attribute))))

      (it "component"
        (let [attribute (sut/spec->attribute :foo :name {:type :ref :db []})]
          (should= false (:db/isComponent attribute)))
        (let [attribute (sut/spec->attribute :foo :name {:type :ref :db [:component]})]
          (should= true (:db/isComponent attribute))))

      (it "history"
        (let [attribute (sut/spec->attribute :foo :name {:type :ref :db []})]
          (should= false (:db/noHistory attribute)))
        (let [attribute (sut/spec->attribute :foo :name {:type :ref :db [:no-history]})]
          (should= true (:db/noHistory attribute))))

      (it "fulltext"
        (let [attribute (sut/spec->attribute :foo :name {:type :string :db []})]
          (should= false (:db/fulltext attribute)))
        (let [attribute (sut/spec->attribute :foo :name {:type :string :db [:fulltext]})]
          (should= true (:db/fulltext attribute))))
      )

    (context "attribute->spec"

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
                                         :db/fulltext true})]
          (should= [:foo :bar {:type :string :db [:fulltext]}] spec)))

      )

    (it "entity"
      (let [schema (sut/->entity-schema {:kind (s/kind :bar)
                                         :id   s/id
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

  (context "migrator"

      (it "installed-schema-legend"
        (let [db    (api/create-db config [])
              _     (sut/transact! db (sut/->db-schema spec/bibelot))
              result (migrator/installed-schema-legend db {:bibelot spec/bibelot})]
          (should= {:type :string} (-> result :bibelot :name))
          (should= {:type :long} (-> result :bibelot :size))
          (should= {:type :string} (-> result :bibelot :color))))

      (it "install-schema!"
        (let [db     (api/create-db config [])
              schema  (assoc-in spec/bibelot [:kind :value] :bubble)
              _      (migrator/install-schema! db schema)
              result (migrator/installed-schema-legend db {:bibelot spec/bibelot})]
          (should= {:type :string} (-> result :bubble :name))
          (should= {:type :long} (-> result :bubble :size))
          (should= {:type :string} (-> result :bubble :color))))

      (it "install-attribute!"
        (let [db     (api/create-db config [])
              schema  (assoc-in spec/bibelot [:kind :value] :gum)
              _      (migrator/install-attribute! db schema :name)
              result (migrator/installed-schema-legend db {:bibelot spec/bibelot})]
          (should= {:type :string} (-> result :gum :name))))

      )
  )
