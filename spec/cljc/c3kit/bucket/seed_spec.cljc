(ns c3kit.bucket.seed-spec
  (:require [c3kit.apron.schema :as schema]
            [speclj.core #?(:clj :refer :cljs :refer-macros) [describe it should= should-not-be-nil]]
            [c3kit.bucket.api :as db]
            [c3kit.bucket.seed :as sut]
            [c3kit.apron.schema :as s]
            [c3kit.bucket.impl-spec :as impl-spec]
            [c3kit.bucket.spec-helperc :as helper]))

(def bibelot
  (schema/merge-schemas
    impl-spec/bibelot
    {:id     s/id
     :happy? {:type :boolean}}))

(describe "Seed"

  (helper/with-schemas [bibelot])

  (it "creating"
    (let [optimus (sut/entity :bibelot {:name "Optimus"} {})
          output  (with-out-str @optimus)]
      (should= "CREATING:  :bibelot {:name \"Optimus\"}\n" output)
      (should-not-be-nil (:id @optimus))
      (should= @optimus @optimus)))

  (it "updating"
    (let [_original (db/tx :kind :bibelot :name "Optimus" :happy? false)
          optimus   (sut/entity :bibelot {:name "Optimus"} {:happy? true})
          output    (with-out-str @optimus)]
      (should= "UPDATING:  :bibelot {:name \"Optimus\"}\n" output)
      (should= true (:happy? @optimus))))

  (it "exists"
    (let [_original (db/tx :kind :bibelot :name "Optimus" :happy? true)
          optimus   (sut/entity :bibelot {:name "Optimus"} {:happy? true})
          output    (with-out-str @optimus)]
      (should= "EXISTS:    :bibelot {:name \"Optimus\"}\n" output)))
  )
