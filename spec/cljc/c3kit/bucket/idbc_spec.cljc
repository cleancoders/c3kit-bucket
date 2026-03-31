(ns c3kit.bucket.idbc-spec
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [describe context it should= should should-not]]
            [c3kit.bucket.api :as db]
            [c3kit.bucket.idbc :as sut]
            [c3kit.bucket.impl-spec :as impl-spec]
            [c3kit.bucket.spec-helperc :as helper]))

(describe "idbc"

  (context "offline-id?"

    (it "true for negative integers"
      (should (sut/offline-id? -1))
      (should (sut/offline-id? -100)))

    (it "false for positive integers"
      (should-not (sut/offline-id? 1))
      (should-not (sut/offline-id? 12345)))

    (it "false for zero"
      (should-not (sut/offline-id? 0)))

    (it "false for nil"
      (should-not (sut/offline-id? nil)))

    (it "false for non-numbers"
      (should-not (sut/offline-id? "hello"))
      (should-not (sut/offline-id? :keyword))))

  (context "sync-tx"
    (helper/with-schemas [impl-spec/bibelot])

    (it "strips negative id and creates new entity"
      (let [result (sut/sync-tx {:kind :bibelot :id -1 :name "offline"})]
        (should (pos? (:id result)))
        (should= "offline" (:name result))
        (should= result (db/entity (:id result)))))

    (it "passes through positive id as update"
      (let [existing (db/tx :kind :bibelot :name "original")
            result   (sut/sync-tx (assoc existing :name "updated"))]
        (should= (:id existing) (:id result))
        (should= "updated" (:name result))
        (should= "updated" (:name (db/entity (:id existing))))))

    (it "creates entity when id is nil"
      (let [result (sut/sync-tx {:kind :bibelot :name "no-id"})]
        (should (pos? (:id result)))
        (should= "no-id" (:name result)))))
  )
