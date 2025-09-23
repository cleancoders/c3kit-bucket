(ns c3kit.bucket.re-memory-spec
  (:require-macros [c3kit.bucket.api :refer [with-safety-off]]
                   [speclj.core :refer [around before context describe focus-context focus-it it redefs-around should should-be-nil should-contain should-not-contain should-not-throw should-throw should= with]])
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.time :as time]
            [c3kit.bucket.api]
            [c3kit.bucket.api :as db]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.memory-spec :as memory-spec]
            [c3kit.bucket.re-memory :as sut]
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
  (memory-spec/migrator-specs)

  (context "select-find-by"
    (context "without keyseq"
      (helperc/with-schemas config [spec/thingy spec/doodad])
      (before (init-entities!))

      (it "searching by id"
        (should= [(select-keys @thingy [:kind :id])] (sut/select-find-by :thingy :id (:id @thingy))))

      (it "searching by name"
        (should= [(select-keys @thingy [:kind :id :name])] (sut/select-find-by :thingy :name (:name @thingy))))

      (it "searching by multiple attrs"
        (db/tx (swap! thingy assoc :foo "hey foo"))
        (should= [(select-keys @thingy [:kind :id :name :foo])]
                 (sut/select-find-by :thingy :name (:name @thingy) :foo (:foo @thingy))))
      )

    (context "with keyseq"
      (helperc/with-schemas config [spec/thingy spec/doodad])
      (before (reset! thingy (db/tx {:kind :thingy :name "thingy" :foo "hey foo" :bar 12345 :fizz 6789})))

      (it "search by id, empty selection"
        (should= [(select-keys @thingy [:kind :id])] (sut/select-find-by :thingy [] :id (:id @thingy))))

      (it "search by id, one key in selection"
        (should= [(select-keys @thingy [:kind :id :name])] (sut/select-find-by :thingy [:name] :id (:id @thingy))))

      (it "search by id, multiple keys in selection"
        (should= [(select-keys @thingy [:kind :id :name :foo :bar :fizz])]
                 (sut/select-find-by :thingy [:name :foo :bar :fizz] :id (:id @thingy))))

      (it "search by multiple attrs, multiple keys in selection"
        (should= [(select-keys @thingy [:kind :id :name :foo :bar :fizz])]
                 (sut/select-find-by :thingy [:bar :fizz] :name (:name @thingy) :foo (:foo @thingy))))

      (it "select-find can take keyseq"
        (should= [(select-keys @thingy [:kind :id :name])]
                 (sut/select-find :thingy [:name] {:where [[:id (:id @thingy)]]})))
      )
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
                      [thingy-component thingy-render-count #(db/ffind-by :thingy :name (:name @thingy))]
                      [thingy-component thingy-2-count #(db/ffind-by :thingy :name (:name @thingy-2))]
                      [doodad-component doodad-render-count #(db/ffind-by :doodad :name (:name @doodad))]
                      [doodad-component doodad-2-count #(db/ffind-by :doodad :name (:name @doodad-2))]])
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

      (it "finding by id scopes re-renders to those entities"
        (let [thingy-3       (db/tx {:kind :thingy :name "thingy-3"})
              thingy-3-count (atom 0)]
          (wire/render [:div
                        [thingy-component thingy-render-count #(db/find-by :thingy :id [(:id @thingy) (:id @thingy-2)])]
                        [thingy-component thingy-3-count #(db/find-by :thingy :name (:name thingy-3))]])
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

    (context "select-find-by"

      (context "without keyseq"
        (it "editing a field not searched by"
          (wire/render [:div
                        [thingy-component thingy-render-count #(sut/select-find-by :thingy :name (:name @thingy))]
                        [thingy-component thingy-2-count #(sut/select-find-by :thingy :name (:name @thingy-2))]])
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
                          [thingy-component thingy-render-count #(sut/select-find-by :thingy :name (:name @thingy))]
                          [thingy-component thingy-2-count #(sut/select-find-by :thingy :name (:name @thingy-2))]
                          [thingy-component thingy-3-count #(sut/select-find-by :thingy :foo (:foo @thingy-2))]
                          [thingy-component thingy-4-count #(sut/select-find-by :thingy :name (:name @thingy-2)
                                                                                :foo (:foo @thingy-2))]])
            (should= 1 @thingy-render-count)
            (should= 1 @thingy-2-count)
            (db/tx (swap! thingy assoc :name "new name"))
            (wire/flush)
            (should= 2 @thingy-render-count)
            (should= 2 @thingy-2-count)
            (should= 1 @thingy-3-count)
            (should= 2 @thingy-4-count)))

        (it "finding by id scopes re-renders to those entities and attrs"
          (let [thingy-3       (atom nil)
                thingy-3-count (atom 0)]
            (reset! thingy-3 (db/tx {:kind :thingy :name "thingy-3"}))
            (wire/render [:div
                          [thingy-component thingy-render-count #(sut/select-find-by :thingy :id (:id @thingy))]
                          [thingy-component thingy-3-count #(sut/select-find-by :thingy :name (:name thingy-3))]])
            (should= 1 @thingy-render-count)
            (should= 1 @thingy-3-count)
            (db/tx (swap! thingy-3 assoc :name "new-thingy-3"))
            (wire/flush)
            (should= 2 @thingy-3-count)
            (should= 1 @thingy-render-count)
            (db/tx (swap! thingy assoc :name (:name "new-thingy-1")))
            (wire/flush)
            (should= 3 @thingy-3-count)
            (should= 1 @thingy-render-count)
            (db/tx* [(swap! thingy assoc :bar 123) (swap! thingy-3 assoc :bar 456)])
            (wire/flush)
            (should= 3 @thingy-3-count)
            (should= 1 @thingy-render-count)))
        )

      (it "editing a field not searched by or selected does not cause re-render"
        (wire/render [:div
                      [thingy-component thingy-render-count #(sut/select-find-by :thingy [:foo] :name (:name @thingy))]
                      [thingy-component thingy-2-count #(sut/select-find-by :thingy [:foo] :name (:name @thingy-2))]])
        (should= 1 @thingy-render-count)
        (should= 1 @thingy-2-count)
        (db/tx (swap! thingy assoc :bar 12345))
        (db/tx (swap! thingy-2 assoc :bar 12345))
        (wire/flush)
        (should= 1 @thingy-render-count)
        (should= 1 @thingy-2-count))

      (it "editing a field selected"
        (let [thingy-3-count (atom 0)
              thingy-4-count (atom 0)]
          (wire/render [:div
                        [thingy-component thingy-render-count #(sut/select-find-by :thingy [:foo] :name (:name @thingy))]
                        [thingy-component thingy-2-count #(sut/select-find-by :thingy [:foo] :name (:name @thingy-2))]
                        [thingy-component thingy-3-count #(sut/select-find-by :thingy [:bar] :name (:name @thingy-2))]
                        [thingy-component thingy-4-count #(sut/select-find-by :thingy [:name] :foo (:foo @thingy-2))]])
          (should= 1 @thingy-render-count)
          (should= 1 @thingy-2-count)
          (db/tx (swap! thingy assoc :foo "new foo"))
          (wire/flush)
          (should= 2 @thingy-render-count)
          (should= 2 @thingy-2-count)
          (should= 1 @thingy-3-count)
          (should= 2 @thingy-4-count)))

      (it "finding by id scopes re-renders to those entities and attrs"
        (let [thingy-3       (atom nil)
              thingy-3-count (atom 0)]
          (reset! thingy-3 (db/tx {:kind :thingy :name "thingy-3"}))
          (wire/render [:div
                        [thingy-component thingy-render-count #(sut/select-find-by :thingy [:name] :id (:id @thingy))]
                        [thingy-component thingy-3-count #(sut/select-find-by :thingy [:foo] :name (:name thingy-3))]])
          (should= 1 @thingy-render-count)
          (should= 1 @thingy-3-count)
          (db/tx (swap! thingy-3 assoc :name "new-thingy-3"))
          (wire/flush)
          (should= 2 @thingy-3-count)
          (should= 1 @thingy-render-count)
          (db/tx (swap! thingy assoc :name (:name "new-thingy-1")))
          (wire/flush)
          (should= 3 @thingy-3-count)
          (should= 2 @thingy-render-count)
          (db/tx* [(swap! thingy assoc :bar 123) (swap! thingy-3 assoc :bar 456)])
          (wire/flush)
          (should= 3 @thingy-3-count)
          (should= 2 @thingy-render-count)))

      )

    )

  (context "bucket tests for select-find"

    (context "find"
      (helperc/with-schemas config [spec/bibelot spec/thingy])

      (it "empty db"
        (should= [] (sut/select-find-by :bibelot :name "nothing")))

      (context "(populated db)"
        (before (db/clear)
                (db/tx {:kind :bibelot :name "hello"})
                (db/tx {:kind :bibelot :name "world"})
                (db/tx {:kind :bibelot :name "world" :size 2})
                (db/tx {:kind :bibelot :name "hi!" :size 2}))

        (it ":take option"
          (let [all (sut/select-find :bibelot)]
            (should= (take 1 all) (sut/select-find :bibelot {:take 1}))
            (should= (take 2 all) (sut/select-find :bibelot {:take 2}))
            (should= (take 3 all) (sut/select-find :bibelot {:take 3}))
            (should= all (sut/select-find :bibelot {:take 4}))
            (should= all (sut/select-find :bibelot {:take 99}))))

        (it ":drop option"
          (let [all (sut/select-find :bibelot)]
            (should= (drop 1 all) (sut/select-find :bibelot {:drop 1}))
            (should= (drop 2 all) (sut/select-find :bibelot {:drop 2}))
            (should= (drop 3 all) (sut/select-find :bibelot {:drop 3}))
            (should= [] (sut/select-find :bibelot {:drop 4}))
            (should= [] (sut/select-find :bibelot {:drop 99}))))

        (it "by :name"
          (let [[entity :as entities] (sut/select-find-by :bibelot :name "hello")]
            (should= 1 (count entities))
            (should= "hello" (:name entity))
            (should-not-contain :size entity)))

        (it "by :id"
          (let [b1     (sut/select-ffind-by :bibelot :name "hello")
                b2     (sut/select-ffind-by :bibelot :name "world" :size nil)
                b3     (sut/select-ffind-by :bibelot :name "world" :size 2)
                b4     (sut/select-ffind-by :bibelot :name "hi!")
                thingy (db/tx {:kind :thingy :id 123 :foo "bar"})]
            (should= [] (sut/select-find-by :bibelot :id []))
            (should= [] (sut/select-find-by :bibelot :id nil))
            (should= [] (sut/select-find-by :bibelot :id [nil]))
            (should= [(dissoc b1 :name)] (sut/select-find-by :bibelot :id [nil (:id b1)]))
            (should= [(select-keys b1 [:kind :id])] (sut/select-find-by :bibelot :id [(:id b1)]))
            (should= [(select-keys b1 [:kind :id])] (sut/select-find-by :bibelot :id (:id b1)))
            (should= (set (map #(select-keys % [:kind :id]) [b1 b2]))
                     (set (sut/select-find-by :bibelot :id (map :id [b1 b2]))))
            (should= (set (map #(dissoc % :size) [b2 b3 b4]))
                     (set (sut/select-find-by :bibelot [:name] :id ['not= (:id b1)])))
            (should= (set (map #(dissoc % :size) [b2 b3 b4]))
                     (set (sut/select-find-by :bibelot [:name] :id ['not= nil (:id b1)])))
            (should= (set (map #(dissoc % :size) [b2 b4]))
                     (set (sut/select-find-by :bibelot [:name] :id ['not= (:id b1) (:id b3)])))
            (should= [] (sut/select-find-by :bibelot :id (:id thingy)))))

        (it "by :id and other attributes"
          (let [b1 (sut/select-ffind-by :bibelot :name "hello")
                b2 (sut/select-ffind-by :bibelot :name "world" :size nil)
                b3 (sut/select-ffind-by :bibelot :name "world" :size 2)
                b4 (sut/select-ffind-by :bibelot :name "hi!" :size 2)]
            (should= [] (sut/select-find-by :bibelot :name "hello" :id nil))
            (should= [] (sut/select-find-by :bibelot :name "hello" :id [nil]))
            (should= [b1] (sut/select-find-by :bibelot :name "hello" :id (:id b1)))
            (should= [b1] (sut/select-find-by :bibelot :name "hello" :id [nil (:id b1)]))
            (should= [b2] (sut/select-find-by :bibelot :name "world" :id (:id b2)))
            (should= [b3] (sut/select-find-by :bibelot [:size] :name "world" :id ['not= (:id b2)]))
            (should= [b3] (sut/select-find-by :bibelot [:size] :name "world" :id ['not= nil (:id b2)]))
            (should= [] (sut/select-find-by :bibelot :name "world" :id ['not= (:id b2) (:id b3)]))
            (should= [(select-keys b3 [:size :id :kind])] (sut/select-find-by :bibelot :size 2 :id (:id b3)))
            (should= #{b2 (select-keys b3 [:name :id :kind])}
                     (set (sut/select-find-by :bibelot :name "world" :id [(:id b2) (:id b3)])))
            (should= (set (map #(select-keys % [:size :kind :id]) [b3 b4]))
                     (set (sut/select-find-by :bibelot :size 2 :id [(:id b3) (:id b4)])))
            ))

        (it "two attributes"
          (let [[entity :as entities] (sut/select-find-by :bibelot :name "world" :size 2)]
            (should= 1 (count entities))
            (should= "world" (:name entity))
            (should= 2 (:size entity))))

        (it "returns all found"
          (let [entities (sut/select-find-by :bibelot [:size] :name "world")
                world-1  (first (remove :size entities))
                world-2  (ccc/ffilter :size entities)]
            (should= 2 (count entities))
            (should= "world" (:name world-1))
            (should= nil (:size world-1))
            (should= "world" (:name world-2))
            (should= 2 (:size world-2))))

        (it "all by size"
          (let [entities (sut/select-find-by :bibelot [:name] :size 2)]
            (should= 2 (count entities))
            (should-contain "world" (map :name entities))
            (should-contain "hi!" (map :name entities))))

        (it "nil size"
          (let [entities (sut/select-find-by :bibelot :name "world" :size nil)]
            (should= 1 (count entities))
            (should= "world" (:name (first entities)))))

        (it "nil name"
          (should= [] (sut/select-find-by :bibelot :size 2 :name nil))
          (let [nil-name (db/tx {:kind :bibelot :name nil :size 2})]
            (should= [nil-name] (sut/select-find-by :bibelot :size 2 :name nil))))

        (it "ffind-by"
          (let [world (sut/select-ffind-by :bibelot :name "world" :size 2)]
            (should= "world" (:name world))))

        )
      )
    (context "multi-value fields"

      (helperc/with-schemas config [spec/doodad])

      (it "loading"
        (let [saved  (db/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
              loaded (db/entity (:id saved))]
          (should= (:id loaded) (:id saved))
          (should= #{"foo" "bar"} (set (:names loaded)))
          (should= #{8 42} (set (:numbers loaded)))))

      (it "find by attribute"
        (let [saved  (db/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
              loaded (sut/select-find-by :doodad :names "bar")]
          (should= 1 (count loaded))
          (should= (:id saved) (:id (first loaded)))))

      (it "retracting [string] value"
        (let [saved   (db/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
              updated (db/tx saved :names nil)]
          (should-be-nil (seq (:names updated)))))

      (it "retracting one value from [string]"
        (let [saved   (db/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
              updated (db/tx saved :names ["foo"])]
          (should= #{"foo"} (set (:names updated)))))

      (it "adding one value to [string]"
        (let [saved   (db/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
              updated (db/tx saved :names ["foo" "bar" "fizz"])]
          (should= #{"foo" "bar" "fizz"} (set (:names updated)))))

      (it "find 'not="
        (let [d1 (db/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
              d2 (db/tx {:kind :doodad :names ["foo" "bang"] :numbers [8 43]})]
          (should= [d1] (sut/select-find-by :doodad :names "foo" :numbers ['not= 43]))
          (should= [d2] (sut/select-find-by :doodad :names "foo" :numbers ['not= 42]))
          (should= [] (sut/select-find-by :doodad :names "foo" :numbers ['not= 42 43]))
          (should= [d2] (sut/select-find-by :doodad :names ['not= "bar"] :numbers 8))
          (should= [d1] (sut/select-find-by :doodad :names ['not= "bang"] :numbers 8))
          (should= [] (sut/select-find-by :doodad :names ['not= "bar" "bang"] :numbers 8))))

      (it "find or"
        (let [d1 (db/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
              d2 (db/tx {:kind :doodad :names ["foo" "bang"] :numbers [8 43]})]
          (should= (set [d1 d2]) (spec/set-find-by :doodad :names ["foo" "BLAH"]))
          (should= (set [d1 d2]) (spec/set-find-by :doodad :names ["bar" "bang"]))
          (should= [d1] (sut/select-find-by :doodad [:numbers] :names ["bar" "BLAH"]))
          (should= [] (sut/select-find-by :doodad :names ["ARG" "BLAH"]))))
      )

    (context "filters"
      (helperc/with-schemas config [spec/bibelot spec/thingy])

      (context "(populated db)"
        (before (db/clear)
                (db/tx {:kind :bibelot :name "hello"})
                (db/tx {:kind :bibelot :name "world"})
                (db/tx {:kind :bibelot :name "world" :size 2})
                (db/tx {:kind :bibelot :name "hi!" :size 2}))

        (it "="
          (let [result       (sut/select-find-by :bibelot :name ['= "hi!"])
                result-names (map :name result)]
            (should-contain "hi!" result-names)
            (should-not-contain "world" result-names)))

        (it "= many"
          (let [result       (sut/select-find-by :bibelot :name ['= "hi!" "world"])
                result-names (map :name result)]
            (should-contain "hi!" result-names)
            (should-contain "world" result-names)
            (should-not-contain "hello" result-names)))

        (it "not= nil"
          (let [result       (sut/select-find-by :bibelot [:name] :size ['not= nil])
                result-names (map :name result)]
            (should-not-contain "hello" result-names)
            (should-contain "world" result-names)
            (should-contain "hi!" result-names)))

        (it "or"
          (let [b1  (db/tx :kind :bibelot :name "Bee" :color "red" :size 1)
                b2  (db/tx :kind :bibelot :name "Bee" :color "blue" :size 2)
                b3  (db/tx :kind :bibelot :name "Ant" :color "blue" :size 1)
                _b4 (db/tx :kind :bibelot :color "blue" :size 1)]
            (should= (set [b1 b2 b3]) (spec/set-find-by :bibelot :name ["Bee" "Ant"]))
            (should= (set [b1 b2 b3]) (spec/set-find-by :bibelot :name #{"Bee" "Ant"}))
            (should= [b3] (sut/select-find-by :bibelot [:size :color] :name ["BLAH" "Ant"]))
            (should= [] (sut/select-find-by :bibelot :name ["BLAH" "BLAH" "BLAH"]))
            (should= [] (sut/select-find-by :bibelot :name ["BLAH" "ARG"]))
            (should= [] (sut/select-find-by :bibelot :name []))))

        (it "returns nothing for empty seq"
          (should= [] (sut/select-find-by :bibelot :id []))
          (should= [] (sut/select-find-by :bibelot :name []))
          (should= [] (sut/select-find-by :bibelot :size []))
          (should= [] (sut/select-find-by :bibelot :size [] :id []))
          (should= [] (sut/select-find-by :bibelot :size [] :name []))
          (should= [] (sut/select-find-by :bibelot :size 2 :name []))
          (should= [] (sut/select-find-by :bibelot :size [] :id (:id (db/ffind :bibelot))))
          (should= [] (sut/select-find-by :bibelot :size [] :name "hello"))
          (should= [] (sut/select-find-by :bibelot :size nil :name [])))

        (it "not= to nothing returns everything"
          (let [b1 (sut/select-ffind-by :bibelot :name "hello")
                b2 (sut/select-ffind-by :bibelot :name "world" :size nil)
                b3 (sut/select-ffind-by :bibelot :name "world" :size 2)
                b4 (sut/select-ffind-by :bibelot :name "hi!" :size 2)]
            (should= #{b3 b4} (set (sut/select-find-by :bibelot [:name] :size 2 :id ['not= nil])))
            (should= #{b3 b4} (set (sut/select-find-by :bibelot [:name] :size 2 :id ['not=])))
            (should= #{b3 b4} (set (sut/select-find-by :bibelot :size 2 :name ['not=])))
            (should= #{b2 b3} (set (sut/select-find-by :bibelot :size ['not=] :name "world")))
            (should= #{b1 b2 b3 b4} (set (sut/select-find-by :bibelot [:size :name] :id ['not=])))
            (should= #{b1 b2 b3 b4} (set (sut/select-find-by :bibelot [:size :name] :id ['not= nil])))
            (should= #{b1 b2 b3 b4} (set (sut/select-find-by :bibelot [:name] :size ['not=])))
            (should= #{b1 b2 b3 b4} (set (sut/select-find-by :bibelot [:size] :name ['not=])))
            (should= #{b1 b2 b3 b4} (set (sut/select-find-by :bibelot [:size] :name ['not=] :id ['not=])))
            (should= #{b1 b2 b3 b4} (set (sut/select-find-by :bibelot :name ['not=] :size ['not=])))
            ))

        (it "< > string"
          (let [result       (spec/set-find-by :bibelot :name ['> "g"] :name ['< "i"])
                result-names (map :name result)]
            (should-contain "hi!" result-names)
            (should-not-contain "world" result-names)))

        (it "<= >= string"
          (db/clear)
          (let [b1 (db/tx :kind :bibelot :name "aaa")
                b2 (db/tx :kind :bibelot :name "bbb")
                b3 (db/tx :kind :bibelot :name "ccc")]
            (should= (set [b1 b2 b3]) (spec/set-find-by :bibelot :name ['>= "aaa"]))
            (should= (set [b2 b3]) (spec/set-find-by :bibelot :name ['>= "bbb"]))
            (should= (set [b1 b2 b3]) (spec/set-find-by :bibelot :name ['<= "ccc"]))
            (should= (set [b1 b2]) (spec/set-find-by :bibelot :name ['<= "bbb"]))))

        (it "< <= > >= long"
          (db/clear)
          (let [b1 (db/tx :kind :bibelot :size 1)
                b2 (db/tx :kind :bibelot :size 2)
                b3 (db/tx :kind :bibelot :size 3)]
            (should= (set [b1 b2 b3]) (spec/set-find-by :bibelot :size ['> 0]))
            (should= (set [b1 b2 b3]) (spec/set-find-by :bibelot :size ['>= 1]))
            (should= (set [b2 b3]) (spec/set-find-by :bibelot :size ['> 1]))
            (should= [b3] (sut/select-find-by :bibelot :size ['> 2]))
            (should= [] (sut/select-find-by :bibelot :size ['> 3]))
            (should= (set [b1 b2 b3]) (spec/set-find-by :bibelot :size ['< 4]))
            (should= (set [b1 b2 b3]) (spec/set-find-by :bibelot :size ['<= 3]))
            (should= (set [b1 b2]) (spec/set-find-by :bibelot :size ['< 3]))
            (should= [b1] (sut/select-find-by :bibelot :size ['< 2]))
            (should= [] (sut/select-find-by :bibelot :size ['< 1]))))

        (it "< <= > >= date"
          (let [thing1 (db/tx :kind :thingy :id 123 :bang (-> 1 time/minutes time/ago))
                thing2 (db/tx :kind :thingy :id 456 :bang (-> 3 time/minutes time/ago))
                thing3 (db/tx :kind :thingy :id 789 :bang (-> 5 time/minutes time/ago))]
            (should= [thing2 thing3] (reverse (sort-by :bang (sut/select-find-by :thingy :bang ['< (-> 2 time/minutes time/ago)]))))
            (should= [thing2 thing3] (reverse (sort-by :bang (sut/select-find-by :thingy :bang ['<= (:bang thing2)]))))
            (should= [thing1] (sut/select-find-by :thingy :bang ['> (-> 2 time/minutes time/ago)]))
            (should= [thing1 thing2] (reverse (sort-by :bang (sut/select-find-by :thingy :bang ['>= (:bang thing2)]))))
            (should= [thing2] (sut/select-find-by :thingy :bang ['< (-> 2 time/minutes time/ago)] :bang ['> (-> 4 time/minutes time/ago)]))))

        (it "compare against entity with nil value"
          (db/clear)
          (let [b1  (db/tx :kind :bibelot :name "1" :size 1)
                _b2 (db/tx :kind :bibelot :name "nil" :size nil)]
            (should= [b1] (sut/select-find-by :bibelot [:name] :size ['>= 0]))))

        (it "compare against boolean value"
          (db/clear)
          (let [_thing1 (db/tx :kind :thingy :id 123 :truthy? true)
                thing2  (db/tx :kind :thingy :id 124 :truthy? false)
                _thing3 (db/tx :kind :thingy :id 125 :truthy? nil)]
            (should= [thing2] (sut/select-find-by :thingy :truthy? false))))

        (it "like fuzzy match with anything before or after"
          (let [result       (sut/select-find-by :bibelot :name ['like "%orl%"])
                result-names (map :name result)]
            (should-contain "world" result-names)
            (should-not-contain "hi!" result-names)))

        (it "case-insensitive fuzzy match"
          (let [result       (sut/select-find-by :bibelot :name ['ilike "%OrL%"])
                result-names (map :name result)]
            (should-contain "world" result-names)
            (should-not-contain "hi!" result-names)))

        (it "like fuzzy match with anything after"
          (let [_            (db/tx {:kind :bibelot :name "hello world"})
                result       (sut/select-find-by :bibelot :name ['like "worl%"])
                result-names (map :name result)]
            (should-contain "world" result-names)
            (should-not-contain "hello world" result-names)))

        (it "like fuzzy match with _"
          (let [_            (db/tx {:kind :bibelot :name "words"})
                result       (sut/select-find-by :bibelot :name ['like "wor__"])
                result-names (map :name result)]
            (should-contain "world" result-names)
            (should-contain "words" result-names)
            (should-not-contain "hello" result-names)))

        (it "like with exact match"
          (let [_            (db/tx {:kind :bibelot :name "words"})
                result       (sut/select-find-by :bibelot :name ['like "world"])
                result-names (map :name result)]
            (should-contain "world" result-names)
            (should-not-contain "words" result-names)
            (should-not-contain "hello" result-names)))
        )
      )

    (context "count"
      (helperc/with-schemas config [spec/bibelot spec/thingy])

      (it "empty db"
        (should= 0 (sut/select-count :bibelot))
        (should= 0 (sut/select-count-by :bibelot :name "nothing")))

      (context "populated db"
        (before (db/clear)
                (db/tx {:kind :bibelot :name "hello"})
                (db/tx {:kind :bibelot :name "world"})
                (db/tx {:kind :bibelot :name "world" :size 2})
                (db/tx {:kind :bibelot :name "hi!" :size 2}))

        (it "all"
          (db/tx {:kind :thingy :id 123 :name "world"})
          (should= 4 (sut/select-count :bibelot))
          (should= 1 (sut/select-count :thingy)))

        (it "select-count-by: :name"
          (should= 1 (sut/select-count-by :bibelot :name "hello")))

        (it "select-count-by: two attributes"
          (should= 1 (sut/select-count-by :bibelot :name "world" :size 2)))

        (it "by :id and other attributes"
          (let [b1 (db/ffind-by :bibelot :name "hello")
                b2 (db/ffind-by :bibelot :name "world" :size nil)
                b3 (db/ffind-by :bibelot :name "world" :size 2)
                b4 (db/ffind-by :bibelot :name "hi!")]
            (should= 1 (sut/select-count-by :bibelot :name "hello" :id (:id b1)))
            (should= 1 (sut/select-count-by :bibelot :name "world" :id (:id b2)))
            (should= 1 (sut/select-count-by :bibelot :name "world" :id ['not= (:id b2)]))
            (should= 0 (sut/select-count-by :bibelot :name "world" :id ['not= (:id b2) (:id b3)]))
            (should= 1 (sut/select-count-by :bibelot :size 2 :id (:id b3)))
            (should= 2 (sut/select-count-by :bibelot :name "world" :id [(:id b2) (:id b3)]))
            (should= 2 (sut/select-count-by :bibelot :size 2 :id [(:id b3) (:id b4)]))))
        )
      )

    (context "tx with select-find"
      (helperc/with-schemas config [spec/thingy spec/doodad])
      (before (init-entities!))

      (it "tx'ing an entity that was retrieved with select-find doesn't delete the missing fields"
        (let [full-entity (db/ffind-by :thingy :name "thingy-2")
              queried     (sut/select-ffind-by :thingy :name "thingy-2")
              updated     (db/tx queried :name "new-name-2")
              reloaded    (db/ffind-by :thingy :name "new-name-2")]
          (should= (assoc full-entity :name "new-name-2") reloaded)))

      (it "can delete an attr with an explicit nil"
        (let [original (db/ffind-by :thingy :name "thingy-2")]
          (should= original (db/tx (dissoc original :name)))
          (should= (dissoc original :name) (db/tx original :name nil))))
      )
    )
  )
