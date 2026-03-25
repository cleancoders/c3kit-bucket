(ns c3kit.bucket.idb-spec
  (:require-macros [speclj.core :refer [before context describe it should= with-stubs]])
  (:require [c3kit.bucket.idb :as sut]
            [c3kit.bucket.idb-common :as common]
            [c3kit.bucket.memory :as memory]
            [speclj.core]))

(def legend {:bibelot {:id {:type :long} :name {:type :string}}})

(describe "IDB"

  (context "dirty set"

    (it "read-dirty-set returns empty set from fresh db"
      (-> (common/open "test-dirty-1" legend)
          (.then (fn [idb]
                   (-> (sut/read-dirty-set idb)
                       (.then (fn [result]
                                (should= #{} result)
                                (common/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-1"))))))))

    (it "add-to-dirty-set! adds IDs to the set"
      (-> (common/open "test-dirty-2" legend)
          (.then (fn [idb]
                   (-> (sut/add-to-dirty-set! idb #{1 2 3})
                       (.then (fn [_] (sut/read-dirty-set idb)))
                       (.then (fn [result]
                                (should= #{1 2 3} result)
                                (common/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-2"))))))))

    (it "multiple add-to-dirty-set! calls accumulate IDs"
      (-> (common/open "test-dirty-3" legend)
          (.then (fn [idb]
                   (-> (sut/add-to-dirty-set! idb #{1 2})
                       (.then (fn [_] (sut/add-to-dirty-set! idb #{3 4})))
                       (.then (fn [_] (sut/read-dirty-set idb)))
                       (.then (fn [result]
                                (should= #{1 2 3 4} result)
                                (common/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-3"))))))))

    (it "remove-from-dirty-set! removes specific IDs"
      (-> (common/open "test-dirty-4" legend)
          (.then (fn [idb]
                   (-> (sut/add-to-dirty-set! idb #{1 2 3 4})
                       (.then (fn [_] (sut/remove-from-dirty-set! idb #{2 4})))
                       (.then (fn [_] (sut/read-dirty-set idb)))
                       (.then (fn [result]
                                (should= #{1 3} result)
                                (common/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-4")))))))))

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
