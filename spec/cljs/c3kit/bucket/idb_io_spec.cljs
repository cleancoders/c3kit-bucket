(ns c3kit.bucket.idb-io-spec
  (:require-macros [speclj.core :refer [before context describe it should should= should-not=]])
  (:require [c3kit.bucket.idb-io :as sut]
            [speclj.core]))

(describe "IDB Common"

  (context "schema-hash"
    (it "produces a consistent hash from a legend"
      (let [legend {:user {:id {:type :long} :name {:type :string}}}]
        (should= (sut/schema-hash legend) (sut/schema-hash legend))))

    (it "produces different hashes for different legends"
      (let [legend-1 {:user {:id {:type :long} :name {:type :string}}}
            legend-2 {:user {:id {:type :long} :name {:type :string} :email {:type :string}}}]
        (should-not= (sut/schema-hash legend-1) (sut/schema-hash legend-2)))))

  (context "idb-version"
    (before (.removeItem js/localStorage "test-db-schema-hash")
            (.removeItem js/localStorage "test-db-schema-ver"))

    (it "returns 1 for a new database"
      (let [legend {:user {:id {:type :long} :name {:type :string}}}]
        (should= 1 (sut/idb-version "test-db" legend))))

    (it "returns same version when legend unchanged"
      (let [legend {:user {:id {:type :long} :name {:type :string}}}]
        (sut/idb-version "test-db" legend)
        (should= 1 (sut/idb-version "test-db" legend))))

    (it "increments version when legend changes"
      (let [legend-1 {:user {:id {:type :long} :name {:type :string}}}
            legend-2 {:user {:id {:type :long} :name {:type :string} :email {:type :string}}}]
        (sut/idb-version "test-db" legend-1)
        (should= 2 (sut/idb-version "test-db" legend-2)))))

  (context "serialization"
    (it "round-trips keyword values"
      (let [entity {:id 1 :kind :bibelot :name "widget" :color "red"}]
        (should= entity (sut/js->clj-entity (sut/clj->js-entity entity)))))

    (it "round-trips entities with vector values"
      (let [entity {:id 2 :kind :doodad :names ["alice" "bob"]}]
        (should= entity (sut/js->clj-entity (sut/clj->js-entity entity)))))

    (it "round-trips entities with keyword vector values"
      (let [entity {:id 3 :kind :doodad :letters [:a :b :c]}]
        (should= entity (sut/js->clj-entity (sut/clj->js-entity entity)))))

    (it "round-trips nil for nil input"
      (should= nil (sut/js->clj-entity nil)))))
