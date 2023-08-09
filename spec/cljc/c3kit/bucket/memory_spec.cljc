(ns c3kit.bucket.memory-spec
  (:require
    [c3kit.bucket.api :as db]
    [c3kit.bucket.migrator :as migrator]
    [speclj.core #?(:clj :refer :cljs :refer-macros) [after after-all around around-all before before before-all
                                                      context describe focus-context focus-describe focus-it it
                                                      pending should should-be should-be-a should-be-nil
                                                      should-be-same should-contain should-end-with should-fail
                                                      should-have-invoked should-invoke should-not should-not
                                                      should-not-be should-not-be-a should-not-be-nil
                                                      should-not-be-same should-not-contain should-not-end-with
                                                      should-not-have-invoked should-not-invoke
                                                      should-not-start-with should-not-throw should-not=
                                                      should-not== should-start-with should-throw should<
                                                      should<= should= should== should> should>= stub tags
                                                      with with-all with-stubs xit]]
    [c3kit.bucket.api :as api #?(:clj :refer :cljs :refer-macros) [with-safety-off]]
    [c3kit.bucket.api-spec :as spec]
    [c3kit.bucket.memory :as sut]))

(def config {:impl :memory})
(declare db)

(describe "Memory DB"

  (around [it] (with-safety-off (it)))

  (spec/crud-specs config)
  (spec/nil-value-specs config)
  (spec/count-specs config)
  (spec/find-specs config)
  (spec/filter-specs config)
  (spec/reduce-specs config)
  (spec/kind-in-entity-is-optional config)
  (spec/broken-in-datomic config)
  (spec/multi-value-fields config)
  (spec/cas config)

  (context "safety"
    (around [it] (with-redefs [api/*safety* true] (it)))

    (it "clear" (should-throw #?(:clj AssertionError :cljs js/Error) (sut/clear (api/create-db config nil))))
    (it "delete-all" (should-throw #?(:clj AssertionError :cljs js/Error) (sut/delete-all (api/create-db config nil) :foo))))

  (it "specifying the store"
    (let [store (atom {:foo :bar})
          db    (db/create-db {:impl :memory :store store} [])]
      (should= :bar (:foo @(.-store db)))))

  (describe "migrator"

    (with db (api/create-db config []))
    (before (api/clear- @db))

    (it "installed-schema-legend"
      (let [db (api/create-db config [spec/bibelot])]
        (should= {:bibelot spec/bibelot} (migrator/-installed-schema-legend db nil))))

    (it "install-schema!"
      (let [schema (assoc-in spec/bibelot [:kind :value] :bubble)
            _      (migrator/-install-schema! @db schema)
            result (migrator/-installed-schema-legend @db {:bubble schema})]
        (should= {:type :string} (-> result :bubble :name))
        (should= {:type :long} (-> result :bubble :size))
        (should= {:type :string} (-> result :bubble :color))))

    (it "schema-exists?"
      (should= false (migrator/-schema-exists? @db spec/bibelot))
      (migrator/-install-schema! @db spec/bibelot)
      (should= true (migrator/-schema-exists? @db spec/bibelot)))

    (it "add-attribute!"
      (let [_      (migrator/-add-attribute! @db :gum :name {:type :string})
            result (migrator/-installed-schema-legend @db {:bibelot spec/bibelot})]
        (should= {:type :string} (-> result :gum :name))))

    (it "add-attribute! - schema attr"
      (let [_      (migrator/-add-attribute! @db (assoc-in spec/bibelot [:kind :value] :gum) :name)
            result (migrator/-installed-schema-legend @db {:bibelot spec/bibelot})]
        (should= {:type :string} (-> result :gum :name))))

    (it "remove-attribute!"
      (let [_          (migrator/-install-schema! @db spec/bibelot)
            bibelot    (api/tx- @db {:kind :bibelot :name "red" :size 2 :color "red"})
            _          (migrator/-remove-attribute! @db :bibelot :color)
            reloaded   (api/reload- @db bibelot)
            new-legend (migrator/-installed-schema-legend @db nil)]
        (should= nil (:color reloaded))
        (should-not-contain :color (:bibelot new-legend))))

    (it "rename-attribute!"
      (let [_          (migrator/-install-schema! @db spec/bibelot)
            bibelot    (api/tx- @db {:kind :bibelot :name "red" :size 2 :color "red"})
            _          (migrator/-rename-attribute! @db :bibelot :color :bibelot :hue)
            new-legend (migrator/-installed-schema-legend @db nil)
            reloaded   (api/reload- @db bibelot)]
        (should= nil (:color reloaded))
        (should-not-contain :color (:bibelot new-legend))
        (should= :string (get-in new-legend [:bibelot :hue :type]))))

    (it "rename-attribute! - can't change kind"
      (let [db (api/create-db config [spec/bibelot])]
        (should-throw (migrator/-rename-attribute! db :bibelot :color :blah :hue))))

    (it "rename-attribute! - new attribute exists"
      (migrator/-install-schema! @db spec/bibelot)
      (should-throw (migrator/-rename-attribute! @db :bibelot :color :bibelot :size)))
    )
  )

