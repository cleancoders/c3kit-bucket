(ns c3kit.bucket.seed-spec
  (:require [c3kit.apron.log :as log]
            [c3kit.apron.schema :as schema]
            [speclj.core #?(:clj :refer :cljs :refer-macros) [describe it should= should-not-be-nil should-contain]]
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
    (let [optimus (sut/entity :bibelot {:name "Optimus"} {})]
      (log/capture-logs
        @optimus)
      (should-contain "CREATING:" (log/captured-logs-str))
      (should-contain ":bibelot" (log/captured-logs-str))
      (should-contain "{:name \"Optimus\"}" (log/captured-logs-str))
      (should-not-be-nil (:id @optimus))
      (should= @optimus @optimus)))

  (it "updating"
    (let [_original (db/tx :kind :bibelot :name "Optimus" :happy? false)
          optimus   (sut/entity :bibelot {:name "Optimus"} {:happy? true})]
      (log/capture-logs
        @optimus)
      (should-contain "UPDATING:" (log/captured-logs-str))
      (should= true (:happy? @optimus))))

  (it "exists"
    (let [_original (db/tx :kind :bibelot :name "Optimus" :happy? true)
          optimus   (sut/entity :bibelot {:name "Optimus"} {:happy? true})]
      (log/capture-logs
        @optimus)
      (should-contain "EXISTS:" (log/captured-logs-str))))
  )
