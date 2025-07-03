(ns c3kit.bucket.re-memory-spec
  (:require-macros [speclj.core :refer [describe it should should= context around redefs-around]]
                   [c3kit.bucket.api :refer [with-safety-off]])
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.re-memory :as sut]
            [c3kit.bucket.memory :refer [MemoryDB]]))


(def config {:impl :re-memory})
(declare db)

(describe "rememory"
  (around [it] (with-safety-off (it)))

  (spec/crud-specs config)
  (spec/nil-value-specs config)
  (spec/count-specs config)
  (spec/find-specs config)
  (spec/filter-specs config)
  (spec/reduce-specs config)
  (spec/kind-in-entity-is-optional config)
  (spec/broken-in-datomic config)
  (spec/multi-value-fields config)
  (spec/cas config)
  )
