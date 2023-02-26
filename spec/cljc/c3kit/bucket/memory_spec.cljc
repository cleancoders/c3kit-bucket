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
            [c3kit.bucket.api-spec :as spec]
            [c3kit.bucket.memory :as sut]
            ))

(describe "Memory DB"
  (spec/crud-specs (sut/create-db))
  (spec/nil-value-specs (sut/create-db))
  (spec/count-all (sut/create-db))
  (spec/count-by (sut/create-db))
  (spec/find-by (sut/create-db))
  (spec/find-all (sut/create-db))
  (spec/reduce-by (sut/create-db))
  )
