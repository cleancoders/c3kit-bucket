(ns c3kit.bucket.memory-spec
  (:require
    [c3kit.bucket.api :as db]
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

  (context "safety"
    (around [it] (with-redefs [api/*safety* true] (it)))

    (it "clear" (should-throw #?(:clj AssertionError :cljs js/Error) (sut/clear (api/create-db config nil))))
    (it "delete-all" (should-throw #?(:clj AssertionError :cljs js/Error) (sut/delete-all (api/create-db config nil) :foo))))

  (it "specifying the story"
    (let [store (atom {:foo :bar})
          db (db/create-db {:impl :memory :store store} [])]
      (should= :bar (:foo @(.-store db)))))
  )

