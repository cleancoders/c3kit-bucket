(ns c3kit.bucket.memory-spec
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [after after-all around around-all before before before-all
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

(describe "Memory DB"

  (around [it] (with-safety-off (it)))

  (spec/crud-specs (sut/create-db))
  (spec/nil-value-specs (sut/create-db))
  (spec/count-specs (sut/create-db))
  (spec/find-specs (sut/create-db))
  (spec/filter-specs (sut/create-db))
  (spec/reduce-specs (sut/create-db))
  (spec/kind-is-optional (sut/create-db))
  (spec/broken-in-datomic (sut/create-db))

  (context "safety"
    (around [it] (with-redefs [api/*safety* true] (it)))

    (it "clear" (should-throw #?(:clj AssertionError :cljs js/Error) (sut/clear (sut/create-db))))
    (it "delete-all" (should-throw #?(:clj AssertionError :cljs js/Error) (sut/delete-all (sut/create-db) :foo))))
  )
