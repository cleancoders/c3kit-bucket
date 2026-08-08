(ns c3kit.bucket.memory-spec
  (:require [c3kit.apron.log :as log]
            [c3kit.bucket.history :as history]
            [c3kit.bucket.migrator :as migrator]
            [speclj.core #?(:clj :refer :cljs :refer-macros) [around before context describe it should-contain should-not-contain should-not-throw should-throw should= with]]
            [c3kit.bucket.api :as api #?(:clj :refer :cljs :refer-macros) [with-safety-off]]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.memory :as sut]
            #?(:clj [c3kit.bucket.memory-history]
               :cljs [c3kit.bucket.memory-history])))

(def config {:impl :memory})
(def history-config {:impl :memory-history :storage {:impl :memory}})
(declare db)

(defn migrator-specs []
  (describe "migrator"

    (with db (api/create-db config []))
    (before (api/clear- @db))

    (it "installed-schema-legend"
      (let [db (api/create-db config [spec/bibelot])]
        (should= {:bibelot spec/bibelot} (migrator/-installed-schema-legend db nil))))

    (it "install-schema!"
      (let [schema (assoc-in spec/bibelot [:kind :value] :bubble)
            _      (migrator/-install-schema! @db schema)
            result (migrator/-installed-schema-legend @db {:bubble schema})]
        (should= {:type :string} (-> result :bubble :name))
        (should= {:type :long} (-> result :bubble :size))
        (should= {:type :string} (-> result :bubble :color))))

    (it "install-schema! - enum"
      (migrator/-install-schema! @db spec/bibelot-states)
      (let [result (migrator/-installed-schema-legend @db {:bibelot.state spec/bibelot-states})
            values (-> result :bibelot.state :values)]
        (should= :bibelot.state (-> result :bibelot.state :enum))
        (should-contain :pending values)
        (should-contain :active values)
        (should-contain :disabled values)))

    (it "schema-exists?"
      (should= false (migrator/-schema-exists? @db spec/bibelot))
      (migrator/-install-schema! @db spec/bibelot)
      (should= true (migrator/-schema-exists? @db spec/bibelot)))

    (it "add-attribute!"
      (let [_      (migrator/-add-attribute! @db :gum :name {:type :string})
            result (migrator/-installed-schema-legend @db {:bibelot spec/bibelot})]
        (should= {:type :string} (-> result :gum :name))))

    (it "add-attribute! - schema attr"
      (let [_      (migrator/-add-attribute! @db (assoc-in spec/bibelot [:kind :value] :gum) :name)
            result (migrator/-installed-schema-legend @db {:bibelot spec/bibelot})]
        (should= {:type :string} (-> result :gum :name))))

    (it "remove-attribute!"
      (let [_          (migrator/-install-schema! @db spec/bibelot)
            bibelot    (api/tx- @db {:kind :bibelot :name "red" :size 2 :color "red"})
            _          (migrator/-remove-attribute! @db :bibelot :color)
            reloaded   (api/reload- @db bibelot)
            new-legend (migrator/-installed-schema-legend @db nil)]
        (should= nil (:color reloaded))
        (should-not-contain :color (:bibelot new-legend))))

    (it "remove-attribute! that doesn't exist"
      (migrator/-install-schema! @db spec/bibelot)
      (log/capture-logs
        (should-not-throw (migrator/-remove-attribute! @db :bibelot :fizz))
        (should-not-throw (migrator/-remove-attribute! @db :fizz :bang))))

    (it "rename-attribute!"
      (let [_          (migrator/-install-schema! @db spec/bibelot)
            bibelot    (api/tx- @db {:kind :bibelot :name "red" :size 2 :color "red"})
            _          (migrator/-rename-attribute! @db :bibelot :color :bibelot :hue)
            new-legend (migrator/-installed-schema-legend @db nil)
            reloaded   (api/reload- @db bibelot)]
        (should= nil (:color reloaded))
        (should-not-contain :color (:bibelot new-legend))
        (should= :string (get-in new-legend [:bibelot :hue :type]))))

    (it "rename-attribute! - can't change kind"
      (let [db (api/create-db config [spec/bibelot])]
        (should-throw (migrator/-rename-attribute! db :bibelot :color :blah :hue))))

    (it "rename-attribute! - new attribute exists"
      (migrator/-install-schema! @db spec/bibelot)
      (should-throw (migrator/-rename-attribute! @db :bibelot :color :bibelot :size)))

    (it "rename-attribute! - existing missing"
      (migrator/-install-schema! @db spec/bibelot)
      (log/capture-logs
        (should-not-throw (migrator/-rename-attribute! @db :blah :color :blah :size))))))

(describe "Memory DB"

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
  (spec/order-by-specs config)
  (spec/order-by-vector-specs config)
  (migrator-specs)

  (context "safety"
    (around [it] (with-redefs [api/*safety* true] (it)))

    (it "clear" (should-throw #?(:clj AssertionError :cljs js/Error) (sut/clear (api/create-db config nil))))
    (it "delete-all" (should-throw #?(:clj AssertionError :cljs js/Error) (sut/delete-all (api/create-db config nil) :foo))))

  (it "specifying the store"
    (let [store (atom {:foo :bar})
          db    (api/create-db {:impl :memory :store store} [])]
      (should= :bar (:foo @(.-store db)))))

  (context "history decorator"
    (spec/history-specs history-config)
    (spec/excise-specs history-config)

    (context "decorator specifics"
      (it "plain memory does not support history"
        (let [db (api/create-db config [spec/bibelot])]
          (should= false (history/supported? db))))

      (it "history starts at wrap time"
        (let [store (atom {})
              inner (api/create-db {:impl :memory :store store} [spec/bibelot])
              pre   (api/tx- inner {:kind :bibelot :name "Pre" :size 1 :color "gray"})
              db    (api/create-db {:impl :memory-history :storage {:impl :memory :store store}} [spec/bibelot])]
          (should= [] (history/history- db pre))
          (let [post (api/tx- db (assoc pre :size 2))]
            (should= 1 (count (history/history- db post))))))

      (it "clear resets the log"
        (let [db (api/create-db history-config [spec/bibelot])
              e  (api/tx- db {:kind :bibelot :name "ClearMe" :size 1 :color "red"})]
          (api/clear- db)
          (should= [] (history/history- db e))))

      (it "delete-all records deletion markers"
        (let [db (api/create-db history-config [spec/bibelot])
              a  (api/tx- db {:kind :bibelot :name "A" :size 1 :color "red"})
              b  (api/tx- db {:kind :bibelot :name "B" :size 1 :color "blue"})]
          (api/delete-all- db :bibelot)
          (should= true (:db/deleted? (last (history/history- db a))))
          (should= true (:db/deleted? (last (history/history- db b)))))))))
