(ns c3kit.bucket.idb-spec
  (:require-macros [speclj.core :refer [context describe it should=]])
  (:require [c3kit.bucket.idb :as sut]
            [c3kit.bucket.idb-common :as common]
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
                                (.deleteDatabase js/indexedDB "test-dirty-4"))))))))))
