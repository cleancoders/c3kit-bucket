(ns c3kit.bucket.migrator-spec
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [describe context it should= should-not-contain should-contain]]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.impl-spec :as impl-spec]
            [c3kit.bucket.migrator :as sut]))

(describe "Migrator public API"
  (context "rename-attribute!- (explicit db)"
    (it "renames the attribute on the given db"
      (let [db (api/create-db {:impl :memory} [impl-spec/bibelot])]
        (sut/rename-attribute!- db :bibelot :color :bibelot :hue)
        (let [legend (sut/-installed-schema-legend db nil)]
          (should-not-contain :color (:bibelot legend))
          (should-contain :hue (:bibelot legend))
          (should= :string (get-in legend [:bibelot :hue :type])))))))
