(ns c3kit.bucket.datomic-cloud-spec
  (:require [c3kit.apron.log :as log]
            [c3kit.apron.schema :as s]
            [c3kit.apron.time :as time]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.datomic-cloud :as sut]
            [c3kit.bucket.migrator :as migrator]
            [c3kit.bucket.spec-helperc :as helper]
            [speclj.core :refer :all])
  (:import (java.util Date)))

(def config {:impl :datomic-cloud :server-type :datomic-cloud :system "test" :db-name "cloud"})
(declare db)
(declare biby)
(defn sleep [entity] (Thread/sleep 10) entity)

(focus-describe "Datomic Cloud"

  (with-stubs)
  (around [it] (api/with-safety-off (it)))

  (context "api"
    (spec/crud-specs config)
    (spec/nil-value-specs config)
    #_(spec/find-specs config)
    #_(spec/filter-specs config)
    #_(spec/reduce-specs config)
    #_(spec/count-specs config)
    #_(spec/kind-in-entity-is-optional config)
    #_(spec/multi-value-fields config)
    #_(spec/cas config)
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

      (it "ignores any without a valueType, which could be just the db"
        (let [spec (sut/attribute->spec {:db/ident :doodle})]
          (should= nil spec)))

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

      )

    (it "all-attributes->specs"
      (let [specs (sut/all-attributes->specs [{:db/ident :doodle}
                                              {:db/ident       :foo/name
                                               :db/valueType   :db.type/string
                                               :db/cardinality :db.cardinality/one}])]
        (should= [[:foo :name {:type :string}]] specs)))

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

  #_(context "min & max"
    (with db (api/create-db config [spec/bibelot spec/thingy]))

    (it "find-max-of-all"
      (sut/delete-all @db :bibelot)
      (let [_ (sut/tx @db {:kind :bibelot :size 1})
            _ (sut/tx @db {:kind :bibelot :size 2})
            b3 (sut/tx @db {:kind :bibelot :size 3})]
        (should= b3 (sut/find-max-of-all- @db :bibelot :size))
        (should= 3 (sut/find-max-val-of-all- @db :bibelot :size))))

    (it "find-min-of-all"
      (sut/delete-all @db :bibelot)
      (let [b1 (sut/tx @db {:kind :bibelot :size 1})
            _ (sut/tx @db {:kind :bibelot :size 2})
            _ (sut/tx @db {:kind :bibelot :size 3})]
        (should= b1 (sut/find-min-of-all- @db :bibelot :size))
        (should= 1 (sut/find-min-val-of-all- @db :bibelot :size))))
    )

  (describe "migrator"

    (with db (api/create-db config []))
    (before (api/clear- @db))

    #_(it "schema"
      (let [schema (migrator/migration-schema config)]
        (should= :migration (-> schema :kind :value))
        (should= :int (-> schema :id :type))
        (should= [:unique-value] (-> schema :name :db))))

    #_(it "installed-schema-legend"
      (let [db (api/create-db config [])
            _ (sut/transact! db (sut/->db-schema spec/bibelot))
            result (migrator/-installed-schema-legend db {:bibelot spec/bibelot})]
        (should= {:type :string} (-> result :bibelot :name))
        (should= {:type :long} (-> result :bibelot :size))
        (should= {:type :string} (-> result :bibelot :color))))

    #_(it "install-schema!"
      (let [schema (assoc-in spec/bibelot [:kind :value] :bubble)
            _ (migrator/-install-schema! @db schema)
            result (migrator/-installed-schema-legend @db {:bubble schema})]
        (should= {:type :string} (-> result :bubble :name))
        (should= {:type :long} (-> result :bubble :size))
        (should= {:type :string} (-> result :bubble :color))))

    #_(it "schema-exists?"
      (should= false (migrator/-schema-exists? @db spec/bibelot))
      (migrator/-install-schema! @db spec/bibelot)
      (should= true (migrator/-schema-exists? @db spec/bibelot)))

    #_(it "add-attribute!"
      (let [_ (migrator/-add-attribute! @db :gum :name {:type :string})
            result (migrator/-installed-schema-legend @db {:bibelot spec/bibelot})]
        (should= {:type :string} (-> result :gum :name))))

    #_(it "add-attribute! - schema attr"
      (let [_ (migrator/-add-attribute! @db (assoc-in spec/bibelot [:kind :value] :gum) :name)
            result (migrator/-installed-schema-legend @db {:bibelot spec/bibelot})]
        (should= {:type :string} (-> result :gum :name))))

    #_(it "remove-attribute!"
      (let [_ (migrator/-install-schema! @db spec/bibelot)
            bibelot (api/tx- @db {:kind :bibelot :name "red" :size 2 :color "red"})
            _ (migrator/-remove-attribute! @db :bibelot :color)
            reloaded (api/reload- @db bibelot)
            new-legend (migrator/-installed-schema-legend @db nil)]
        (should= nil (:color reloaded))
        (should-not-contain :color (:bibelot new-legend))))

    #_(it "remove-attribute! that doesn't exist"
      (migrator/-install-schema! @db spec/bibelot)
      (log/capture-logs
        (should-not-throw (migrator/-remove-attribute! @db :bibelot :fizz))
        (should-not-throw (migrator/-remove-attribute! @db :fizz :bang))))

    #_(it "remove-attribute! - multi"
      (let [db (api/create-db config [])
            _ (migrator/-install-schema! db spec/doodad)
            doodad (api/tx- db {:kind :doodad :names ["bill" "bob"] :numbers [123 456]})
            _ (migrator/-remove-attribute! db :doodad :numbers)
            reloaded (api/reload- db doodad)
            new-legend (migrator/-installed-schema-legend db nil)]
        (should= nil (:numbers reloaded))
        (should-not-contain :numbers (:doodad new-legend))))

    #_(it "rename-attribute!"
      (let [_ (migrator/-install-schema! @db spec/bibelot)
            bibelot (api/tx- @db {:kind :bibelot :name "red" :size 2 :color "red"})
            _ (migrator/-rename-attribute! @db :bibelot :color :bibelot :hue)
            new-legend (migrator/-installed-schema-legend @db nil)
            reloaded (api/reload- @db bibelot)]
        (should= nil (:color reloaded))
        (should-not-contain :color (:bibelot new-legend))
        (should= :string (get-in new-legend [:bibelot :hue :type]))))

    #_(it "rename-attribute! - new attribute exists"
      (migrator/-install-schema! @db spec/bibelot)
      (should-throw (migrator/-rename-attribute! @db :bibelot :color :bibelot :size)))

    #_(it "rename-attribute! - existing missing"
      (migrator/-install-schema! @db spec/bibelot)
      (log/capture-logs
        (should-not-throw (migrator/-rename-attribute! @db :blah :color :blah :size)))))


  #_(context "history"
    (helper/with-schemas config [spec/bibelot #_spec/thingy])
    (with biby (-> (api/tx :kind :bibelot :name "Biby" :size 1 :color "blue")
                   sleep
                   (api/tx :size 2)
                   sleep
                   (api/tx :color "green")
                   sleep
                   (api/tx :size 3 :color "red")))

    (it "of entity"
      (let [history (sut/history @biby)]
        (should= 4 (count history))
        (doseq [h history]
          (should (:db/tx h))
          (should (:db/instant h)))
        (should= {:name "Biby" :size 1 :color "blue"} (select-keys (nth history 0) [:name :size :color]))
        (should= {:name "Biby" :size 2 :color "blue"} (select-keys (nth history 1) [:name :size :color]))
        (should= {:name "Biby" :size 2 :color "green"} (select-keys (nth history 2) [:name :size :color]))
        (should= {:name "Biby" :size 3 :color "red"} (select-keys (nth history 3) [:name :size :color]))))

    (it "created-at"
      (let [moment (sut/created-at @biby)
            now (time/now)]
        (should-be-a Date moment)
        (should (time/after? moment (-> 1 time/seconds time/ago)))
        (should (time/before? moment now))))

    (it "updated-at"
      (Thread/sleep 10)
      (let [updated (api/tx @biby :size 4)
            moment (sut/updated-at updated)
            _ (Thread/sleep 10)
            now (time/now)]
        (should-be-a Date moment)
        (should (time/after? moment (-> 1 time/seconds time/ago)))
        (should (time/before? moment now))
        (should (time/after? moment (sut/created-at updated)))))

    (it "with-timestamps"
      (let [updated (api/tx @biby :size 4)
            result (sut/with-timestamps updated)]
        (should= (sut/created-at updated) (:db/created-at result))
        (should= (sut/updated-at updated) (:db/updated-at result))))

    ;(it "excises biby"
    ;  (sut/excise! @biby)
    ;  @(datomic/sync-index @(.-conn @api/impl) (time/millis-since-epoch (time/from-now (time/hours 1))))
    ;  (should-be-nil (api/find-by :bibelot :name "Biby"))
    ;  (should-be-nil (sut/history @biby)))

    )

  )
