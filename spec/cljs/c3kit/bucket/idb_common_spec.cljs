(ns c3kit.bucket.idb-common-spec
  (:require-macros [speclj.core :refer [before context describe it should should= with-stubs]]
                   [c3kit.bucket.api :refer [with-safety-off]])
  (:require [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as sut]
            [c3kit.bucket.indexeddb :as indexeddb]
            [c3kit.bucket.memory :as memory]
            [speclj.core]))

(def legend {:bibelot {:id {:type :long} :name {:type :string}}})

(def bibelot
  {:kind (s/kind :bibelot)
   :id   {:type :long}
   :name {:type :string}})

(describe "IDB Common"

  (context "sync!"

    (it "invokes callback with empty vector when idb-atom is nil"
      (let [db      (indexeddb/->IndexedDB (atom legend) (atom {}) (atom nil) "test" (constantly true) memory/entity memory/do-find)
            called? (atom false)]
        (sut/sync! db (fn [entities]
                        (reset! called? true)
                        (should= [] entities)))
        (should= true @called?))))

  (context "sync-complete!"

    (before (reset! sut/offline-id-counter 0))

    (it "performs memory effects even when idb-atom is nil"
      (with-safety-off
        (let [db (api/create-db {:impl :indexeddb :db-name "test-sync-complete" :online? (constantly false)} [bibelot])]
          (reset! api/impl db)
          (sut/idb-tx db {:kind :bibelot :name "offline-widget"})
          (sut/sync-complete! db #{-1} [{:kind :bibelot :id 9001 :name "offline-widget"}])
          (should= 0 (count (api/find-by- db :bibelot :id -1)))
          (should (some? (api/entity- db :bibelot 9001)))))))

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
