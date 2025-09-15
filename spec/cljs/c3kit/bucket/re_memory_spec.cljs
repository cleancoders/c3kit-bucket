(ns c3kit.bucket.re-memory-spec
  (:require-macros [c3kit.bucket.api :refer [with-safety-off]]
                   [speclj.core :refer [around before context describe focus-it it redefs-around should should-throw should=]])
  (:require [c3kit.bucket.api :as db]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.re-memory :as rem]
            [c3kit.bucket.spec-helperc :as helperc]
            [c3kit.wire.spec-helper :as wire]))


(def config {:impl :re-memory})
(declare db)

(def thingy (atom nil))
(def thingy-2 (atom nil))
(def doodad (atom nil))
(def doodad-2 (atom nil))

(def thingy-render-count (atom 0))
(def doodad-render-count (atom 0))
(def thingy-2-count (atom 0))
(def doodad-2-count (atom 0))

(defn reset-render-counts! []
  (reset! thingy-render-count 0)
  (reset! doodad-render-count 0)
  (reset! thingy-2-count 0)
  (reset! doodad-2-count 0))

(defn thingy-component [atom lookup-fn]
  (let [thingy (lookup-fn)]
    (swap! atom inc)
    [:div (:name thingy)]))

(defn doodad-component [atom lookup-fn]
  (let [doodad (lookup-fn)]
    (swap! atom inc)
    [:div (first (:names doodad))]))

(defn page []
  [:div
   [thingy-component thingy-render-count #(db/ffind-by :thingy :id (:id @thingy))]
   [doodad-component doodad-render-count #(db/ffind-by :doodad :id (:id @doodad))]])

(defn init-entities! []
  (reset! thingy (db/tx {:kind :thingy :name "thingy"}))
  (reset! doodad (db/tx {:kind :doodad :names [:doodad]}))
  (reset! thingy-2 (db/tx {:kind :thingy :name "thingy-2" :foo "foo 2"}))
  (reset! doodad-2 (db/tx {:kind :doodad :names ["doodad-2"]})))


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

  (context "find-by"
    (helperc/with-schemas config [spec/thingy spec/doodad])
    (before (init-entities!))

    (it "searching by id"
      (should= [(select-keys @thingy [:kind :id])] (rem/find-by :thingy :id (:id @thingy))))

    (it "searching by name"
      (should= [(select-keys @thingy [:kind :id :name])] (rem/find-by :thingy :name (:name @thingy))))

    (it "searching by multiple attrs"
      (db/tx (swap! thingy assoc :foo "hey foo"))
      (should= [(select-keys @thingy [:kind :id :name :foo])]
               (rem/find-by :thingy :name (:name @thingy) :foo (:foo @thingy))))
    )

  (context "select-find-by"
    (helperc/with-schemas config [spec/thingy spec/doodad])
    (before (reset! thingy (db/tx {:kind :thingy :name "thingy" :foo "hey foo" :bar 12345 :fizz 6789})))

    (it "forget to include keyseq"
      (should-throw js/Error (rem/select-find-by :thingy :id (:id @thingy))))

    (it "search by id, empty selection"
      (should= [(select-keys @thingy [:kind :id])] (rem/select-find-by :thingy [] :id (:id @thingy))))

    (it "search by id, one key in selection"
      (should= [(select-keys @thingy [:kind :id :name])] (rem/select-find-by :thingy [:name] :id (:id @thingy))))

    (it "search by id, multiple keys in selection"
      (should= [(select-keys @thingy [:kind :id :name :foo :bar :fizz])]
               (rem/select-find-by :thingy [:name :foo :bar :fizz] :id (:id @thingy))))

    (it "search by multiple attrs, multiple keys in selection"
      (should= [(select-keys @thingy [:kind :id :name :foo :bar :fizz])]
               (rem/select-find-by :thingy [:bar :fizz] :name (:name @thingy) :foo (:foo @thingy))))
    )

  (context "render control"
    (wire/with-root-dom)
    (helperc/with-schemas config [spec/thingy spec/doodad])
    (before (reset-render-counts!)
            (init-entities!))

    (context "do-find"
      (it "editing a thingy re-renders a thingy but not a doodad"
        (wire/render [page] (wire/select "#root"))
        (should= 1 @thingy-render-count)
        (should= 1 @doodad-render-count)
        (db/tx @thingy :name "my-thingy")
        (wire/flush)
        (should= 2 @thingy-render-count)
        (should= 1 @doodad-render-count))

      (it "editing a thingy re-renders all thingies but no doodads"
        (wire/render [:div
                      [thingy-component thingy-render-count #(db/ffind-by :thingy :id (:id @thingy))]
                      [thingy-component thingy-2-count #(db/ffind-by :thingy :id (:id @thingy-2))]
                      [doodad-component doodad-render-count #(db/ffind-by :doodad :id (:id @doodad))]
                      [doodad-component doodad-2-count #(db/ffind-by :doodad :id (:id @doodad-2))]])
        (should= 1 @thingy-render-count)
        (should= 1 @thingy-2-count)
        (should= 1 @doodad-render-count)
        (should= 1 @doodad-2-count)
        (db/tx @thingy :name "my-thingy")
        (wire/flush)
        (should= 2 @thingy-render-count)
        (should= 2 @thingy-2-count)
        (should= 1 @doodad-2-count)
        (should= 1 @doodad-render-count))
      )

    (context "entity"
      (it "editing a thingy doesn't make another thingy re-render"
        (wire/render [:div
                      [thingy-component thingy-render-count #(db/entity (:id @thingy))]
                      [thingy-component thingy-2-count #(db/entity (:id @thingy-2))]])
        (should= 1 @thingy-render-count)
        (should= 1 @thingy-2-count)
        (db/tx @thingy :name "edited-thingy")
        (wire/flush)
        (should= 2 @thingy-render-count)
        (should= 1 @thingy-2-count))
      )

    (context "find-by"
      (it "editing a field not searched by"
        (wire/render [:div
                      [thingy-component thingy-render-count #(rem/find-by :thingy :name (:name @thingy))]
                      [thingy-component thingy-2-count #(rem/find-by :thingy :name (:name @thingy-2))]])
        (should= 1 @thingy-render-count)
        (should= 1 @thingy-2-count)
        (db/tx (swap! thingy assoc :foo "bye foo"))
        (wire/flush)
        (should= 1 @thingy-render-count)
        (should= 1 @thingy-2-count))

      (it "editing a field searched by"
        (let [thingy-3-count (atom 0)
              thingy-4-count (atom 0)]
          (wire/render [:div
                        [thingy-component thingy-render-count #(rem/find-by :thingy :name (:name @thingy))]
                        [thingy-component thingy-2-count #(rem/find-by :thingy :name (:name @thingy-2))]
                        [thingy-component thingy-3-count #(rem/find-by :thingy :foo (:foo @thingy-2))]
                        [thingy-component thingy-4-count #(rem/find-by :thingy :name (:name @thingy-2)
                                                                       :foo (:foo @thingy-2))]])
          (should= 1 @thingy-render-count)
          (should= 1 @thingy-2-count)
          (db/tx (swap! thingy assoc :name "new name"))
          (wire/flush)
          (should= 2 @thingy-render-count)
          (should= 2 @thingy-2-count)
          (should= 1 @thingy-3-count)
          (should= 2 @thingy-4-count)))

      (it "finding by id scopes re-renders to those entities"
        (let [thingy-3       (db/tx {:kind :thingy :name "thingy-3"})
              thingy-3-count (atom 0)]
          (wire/render [:div
                        [thingy-component thingy-render-count #(rem/find-by :thingy :id [(:id @thingy) (:id @thingy-2)]
                                                                            :name (:name @thingy))]
                        [thingy-component thingy-3-count #(rem/find-by :thingy :name (:name thingy-3))]])
          (should= 1 @thingy-render-count)
          (should= 1 @thingy-3-count)
          (db/tx thingy-3 :name "new-thingy-3")
          (wire/flush)
          (should= 2 @thingy-3-count)
          (should= 1 @thingy-render-count)
          (db/tx @thingy :name (:name "new-thingy-1"))
          (wire/flush)
          (should= 3 @thingy-3-count)
          (should= 2 @thingy-render-count)))

      )

    (context "select-find-by"
      (it "editing a field not searched by or selected"
        (wire/render [:div
                      [thingy-component thingy-render-count #(rem/select-find-by :thingy [:foo] :name (:name @thingy))]
                      [thingy-component thingy-2-count #(rem/select-find-by :thingy [:foo] :name (:name @thingy-2))]])
        (should= 1 @thingy-render-count)
        (should= 1 @thingy-2-count)
        (db/tx (swap! thingy assoc :bar 12345))
        (wire/flush)
        (should= 1 @thingy-render-count)
        (should= 1 @thingy-2-count))

      (it "editing a field selected"
        (let [thingy-3-count (atom 0)
              thingy-4-count (atom 0)]
          (wire/render [:div
                        [thingy-component thingy-render-count #(rem/select-find-by :thingy [:foo] :name (:name @thingy))]
                        [thingy-component thingy-2-count #(rem/select-find-by :thingy [:foo] :name (:name @thingy-2))]
                        [thingy-component thingy-3-count #(rem/select-find-by :thingy [:bar] :name (:name @thingy-2))]
                        [thingy-component thingy-4-count #(rem/select-find-by :thingy [:name] :foo (:foo @thingy-2))]])
          (should= 1 @thingy-render-count)
          (should= 1 @thingy-2-count)
          (db/tx (swap! thingy assoc :foo "new foo"))
          (wire/flush)
          (should= 2 @thingy-render-count)
          (should= 2 @thingy-2-count)
          (should= 1 @thingy-3-count)
          (should= 2 @thingy-4-count)))

      (it "finding by id scopes re-renders to those entities"
        (let [thingy-3       (db/tx {:kind :thingy :name "thingy-3"})
              thingy-3-count (atom 0)]
          (wire/render [:div
                        [thingy-component thingy-render-count #(rem/select-find-by :thingy [:name] :id (:id @thingy))]
                        [thingy-component thingy-3-count #(rem/select-find-by :thingy [:foo] :name (:name thingy-3))]])
          (should= 1 @thingy-render-count)
          (should= 1 @thingy-3-count)
          (db/tx thingy-3 :name "new-thingy-3")
          (wire/flush)
          (should= 2 @thingy-3-count)
          (should= 1 @thingy-render-count)
          (db/tx @thingy :name (:name "new-thingy-1"))
          (wire/flush)
          (should= 3 @thingy-3-count)
          (should= 2 @thingy-render-count)))

      )

    )
  )
