(ns c3kit.bucket.seed-spec
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
            [c3kit.bucket.api :as db]
            [c3kit.bucket.seed :as sut]
            [c3kit.bucket.dbc-spec :as entities]
            [c3kit.bucket.spec-helperc :as helper]))

(describe "Seed"

  (helper/with-schemas [entities/bibelot])

  (it "creating"
    (let [optimus (sut/entity :bibelot {:name "Optimus"} {})
          output (with-out-str @optimus)]
      (should= "CREATING:  :bibelot {:name \"Optimus\"}\n" output)
      (should-not-be-nil (:id @optimus))
      (should= @optimus @optimus)))

  (it "updating"
    (let [_original (db/tx :kind :bibelot :name "Optimus" :happy? false)
          optimus (sut/entity :bibelot {:name "Optimus"} {:happy? true})
          output (with-out-str @optimus)]
      (should= "UPDATING:  :bibelot {:name \"Optimus\"}\n" output)
      (should= true (:happy? @optimus))))

  (it "exists"
    (let [_original (db/tx :kind :bibelot :name "Optimus" :happy? true)
          optimus (sut/entity :bibelot {:name "Optimus"} {:happy? true})
          output (with-out-str @optimus)]
      (should= "EXISTS:    :bibelot {:name \"Optimus\"}\n" output)))
 )
