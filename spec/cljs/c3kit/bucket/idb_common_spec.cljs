(ns c3kit.bucket.idb-common-spec
  (:require-macros [speclj.core :refer [before context describe it should= with-stubs]])
  (:require [c3kit.bucket.idb-common :as sut]
            [c3kit.bucket.indexeddb :as indexeddb]
            [c3kit.bucket.memory :as memory]
            [speclj.core]))

(def legend {:bibelot {:id {:type :long} :name {:type :string}}})

(describe "IDB Common"

  (context "sync!"

    (it "invokes callback with empty vector when idb-atom is nil"
      (let [db      (indexeddb/->IndexedDB (atom legend) (atom {}) (atom nil) "test" (constantly true) memory/entity memory/do-find)
            called? (atom false)]
        (sut/sync! db (fn [entities]
                        (reset! called? true)
                        (should= [] entities)))
        (should= true @called?))))

  (context "ensure-offline-id"
    (before (reset! sut/offline-id-counter 0))

    (it "assigns negative decrementing ID to entity without ID"
      (let [result (sut/ensure-offline-id {:kind :bibelot :name "thing"})]
        (should= -1 (:id result))))

    (it "decrements for each new entity"
      (sut/ensure-offline-id {:kind :bibelot :name "first"})
      (let [result (sut/ensure-offline-id {:kind :bibelot :name "second"})]
        (should= -2 (:id result))))

    (it "preserves existing positive ID"
      (let [result (sut/ensure-offline-id {:kind :bibelot :id 42 :name "existing"})]
        (should= 42 (:id result))))

    (it "preserves existing negative ID"
      (let [result (sut/ensure-offline-id {:kind :bibelot :id -5 :name "existing"})]
        (should= -5 (:id result)))))

  (context "offline-ensure-id"
    (with-stubs)
    (before (reset! sut/offline-id-counter 0))

    (it "uses negative ID when offline"
      (let [result (sut/offline-ensure-id (constantly false) {:kind :bibelot :name "offline"})]
        (should= -1 (:id result))))

    (it "uses positive ID when online"
      (let [result (sut/offline-ensure-id (constantly true) {:kind :bibelot :name "online"})]
        (should= true (pos? (:id result)))))))
