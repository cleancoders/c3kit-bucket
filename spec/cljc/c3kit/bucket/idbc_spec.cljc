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

  (context "sync-tx*"
    (helper/with-schemas [impl-spec/bibelot])

    (it "processes mixed positive and negative ids"
      (let [existing (db/tx :kind :bibelot :name "existing")
            entities [{:kind :bibelot :id -1 :name "offline-1"}
                      {:kind :bibelot :id -2 :name "offline-2"}
                      (assoc existing :name "updated")]
            {:keys [entities id-map]} (sut/sync-tx* entities)]
        (should= 3 (count entities))
        (should= "offline-1" (:name (first entities)))
        (should= "offline-2" (:name (second entities)))
        (should= "updated" (:name (nth entities 2)))
        (should (pos? (:id (first entities))))
        (should (pos? (:id (second entities))))
        (should= (:id existing) (:id (nth entities 2)))
        (should= (:id (first entities)) (get id-map -1))
        (should= (:id (second entities)) (get id-map -2))
        (should-not (contains? id-map (:id existing)))))

    (it "returns empty id-map when no negative ids"
      (let [existing (db/tx :kind :bibelot :name "existing")
            {:keys [entities id-map]} (sut/sync-tx* [(assoc existing :name "updated")])]
        (should= 1 (count entities))
        (should= {} id-map)))

    (it "handles empty input"
      (let [{:keys [entities id-map]} (sut/sync-tx* [])]
        (should= [] entities)
        (should= {} id-map))))

  (context "claim-sync!"

    (it "returns true for new sync-id"
      (should (sut/claim-sync! "sync-1")))

    (it "returns false for duplicate sync-id"
      (sut/claim-sync! "sync-2")
      (should-not (sut/claim-sync! "sync-2")))

    (it "returns true for nil sync-id (always processes)"
      (should (sut/claim-sync! nil))
      (should (sut/claim-sync! nil)))

    (it "trims processed set when exceeding max"
      (doseq [i (range 105)]
        (sut/claim-sync! (str "trim-" i)))
      (should (sut/claim-sync! "trim-0"))))
  )
