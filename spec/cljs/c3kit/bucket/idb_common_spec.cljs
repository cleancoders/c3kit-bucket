(ns c3kit.bucket.idb-common-spec
  (:require-macros [speclj.core :refer [before context describe it should= with-stubs]])
  (:require [c3kit.bucket.idb-common :as sut]
            [c3kit.bucket.idb-io :as io]
            [c3kit.bucket.indexeddb :as indexeddb]
            [c3kit.bucket.memory :as memory]
            [speclj.core]))

(def legend {:bibelot {:id {:type :long} :name {:type :string}}})

(describe "IDB"

  (before (reset! sut/dirty-chain (js/Promise.resolve nil)))

  (context "dirty set"

    (it "read-dirty-set returns empty map from fresh db"
      (-> (io/open "test-dirty-1" legend)
          (.then (fn [idb]
                   (-> (sut/read-dirty-set idb)
                       (.then (fn [result]
                                (should= {} result)
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-1"))))))))

    (it "add-to-dirty-set! adds entries to the map"
      (-> (io/open "test-dirty-2" legend)
          (.then (fn [idb]
                   (-> (sut/add-to-dirty-set! idb {1 :bibelot 2 :bibelot 3 :bibelot})
                       (.then (fn [_] (sut/read-dirty-set idb)))
                       (.then (fn [result]
                                (should= {1 :bibelot 2 :bibelot 3 :bibelot} result)
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-2"))))))))

    (it "multiple add-to-dirty-set! calls accumulate entries"
      (-> (io/open "test-dirty-3" legend)
          (.then (fn [idb]
                   (-> (sut/add-to-dirty-set! idb {1 :bibelot 2 :bibelot})
                       (.then (fn [_] (sut/add-to-dirty-set! idb {3 :bibelot 4 :bibelot})))
                       (.then (fn [_] (sut/read-dirty-set idb)))
                       (.then (fn [result]
                                (should= {1 :bibelot 2 :bibelot 3 :bibelot 4 :bibelot} result)
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-3"))))))))

    (it "remove-from-dirty-set! removes specific IDs"
      (-> (io/open "test-dirty-4" legend)
          (.then (fn [idb]
                   (-> (sut/add-to-dirty-set! idb {1 :bibelot 2 :bibelot 3 :bibelot 4 :bibelot})
                       (.then (fn [_] (sut/remove-from-dirty-set! idb #{2 4})))
                       (.then (fn [_] (sut/read-dirty-set idb)))
                       (.then (fn [result]
                                (should= {1 :bibelot 3 :bibelot} result)
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-4")))))))))

  (context "sync!"

    (it "returns resolved promise with empty callback when idb-atom is nil"
      (let [db      (indexeddb/->IndexedDB (atom legend) (atom {}) (atom nil) "test" (constantly true))
            called? (atom false)]
        (-> (sut/sync! db (fn [entities]
                            (reset! called? true)
                            (should= [] entities)))
            (.then (fn [_] (should= true @called?)))))))

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
