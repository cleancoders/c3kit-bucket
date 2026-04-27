(ns c3kit.bucket.migrator-spec
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [describe context it should= should-not-contain should-contain]]
            [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.migrator :as sut]))

(def bibelot
  {:kind  (s/kind :bibelot)
   :id    s/id
   :name  {:type :string}
   :color {:type :string}})

(describe "Migrator public API"
  (context "rename-attribute!- (explicit db)"
    (it "renames the attribute on the given db"
      (let [db (api/create-db {:impl :memory} [bibelot])]
        (sut/rename-attribute!- db :bibelot :color :bibelot :hue)
        (let [legend (sut/-installed-schema-legend db nil)]
          (should-not-contain :color (:bibelot legend))
          (should-contain :hue (:bibelot legend)))))))
