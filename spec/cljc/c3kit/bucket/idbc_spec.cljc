(ns c3kit.bucket.idbc-spec
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [describe context it should= should should-not]]
            [c3kit.bucket.idbc :as sut]))

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
  )
