(ns c3kit.bucket.re-memory-spec
  (:require-macros [c3kit.bucket.api :refer [with-safety-off]]
                   [speclj.core :refer [around before context describe it redefs-around should should=]])
  (:require [c3kit.bucket.api :as db]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.re-memory]
            [c3kit.bucket.spec-helperc :as helperc]
            [c3kit.wire.spec-helper :as wire]))


(def config {:impl :re-memory})
(declare db)

(def thingy (atom {:kind :thingy :name "thingy"}))
(def doodad (atom {:kind :doodad :names [:doodad]}))

(def thingy-render-count (atom 0))
(defn thingy-component []
  (let [thingy (db/ffind :thingy)]
    (swap! thingy-render-count inc)
    [:div (:name thingy)]))

(def doodad-render-count (atom 0))
(defn doodad-component []
  (let [doodad (db/ffind :doodad)]
    (swap! doodad-render-count inc)
    [:div (first (:names doodad))]))

(defn page []
  [:div [thingy-component] [doodad-component]])

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

  (context "do-find"
    (wire/with-root-dom)
    (helperc/with-schemas config [spec/thingy spec/doodad])
    (before (reset! thingy-render-count 0)
            (reset! doodad-render-count 0)
            (db/tx* [@thingy @doodad])
            (wire/render [page] (wire/select "#root")))

    (it "editing a thingy re-renders a thingy but not a doodad"
      (should= 1 @thingy-render-count)
      (should= 1 @doodad-render-count)
      (db/tx @thingy :name "my-thingy")
      (wire/flush)
      (should= 2 @thingy-render-count)
      (should= 1 @doodad-render-count)))
  )
