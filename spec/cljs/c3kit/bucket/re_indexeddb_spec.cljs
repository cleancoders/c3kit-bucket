(ns c3kit.bucket.re-indexeddb-spec
  (:require-macros [speclj.core :refer [around before context describe it should should=]]
                   [c3kit.bucket.api :refer [with-safety-off]])
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as idb]
            [c3kit.bucket.re-indexeddb]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.memory-spec :as memory-spec]
            [c3kit.bucket.spec-helperc :as helperc]
            [c3kit.apron.schema :as s]
            [reagent.core :as r]
            [speclj.core]))

(def config {:impl :re-indexeddb :db-name "test-re-indexeddb"})

(def bibelot
  {:kind  (s/kind :bibelot)
   :id    {:type :long}
   :name  {:type :string}
   :size  {:type :long}
   :color {:type :string}})

(def thingy
  {:kind (s/kind :thingy)
   :id   {:type :int}
   :name {:type :string}
   :foo  {:type :string}})

(describe "ReIndexedDB"

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

  (it "uses reagent atom for store"
    (let [db (api/create-db {:impl :re-indexeddb :db-name "test-reidb-store"} [bibelot])]
      (should (satisfies? IAtom (.-store db)))))

  (context "online-fn"

    (it "defaults to (constantly true) when :online? not provided"
      (let [db (api/create-db {:impl :re-indexeddb :db-name "test-reidb-online-1"} [bibelot])]
        (should= true ((.-online-fn db)))))

    (it "uses the provided :online? callback"
      (let [online? (atom true)
            db      (api/create-db {:impl :re-indexeddb :db-name "test-reidb-online-2" :online? #(deref online?)} [bibelot])]
        (should= true ((.-online-fn db)))
        (reset! online? false)
        (should= false ((.-online-fn db))))))

  (context "offline tx (memory effects)"

    (before (reset! idb/offline-id-counter 0))

    (it "assigns negative ID on offline create"
      (let [online? (atom false)
            db      (api/create-db {:impl :re-indexeddb :db-name "test-reidb-mem-1" :online? #(deref online?)} [bibelot])
            _       (reset! api/impl db)
            saved   (api/-tx db {:kind :bibelot :name "offline-widget"})]
        (should= -1 (:id saved))
        (should= 1 (count (api/find-by- db :bibelot :name "offline-widget")))))

    (it "sync-complete! works with reagent store"
      (let [online? (atom false)
            db      (api/create-db {:impl :re-indexeddb :db-name "test-reidb-mem-2" :online? #(deref online?)} [bibelot])
            _       (reset! api/impl db)]
        (api/-tx db {:kind :bibelot :name "offline-widget"})
        (idb/sync-complete! db #{-1} [{:kind :bibelot :id 9001 :name "offline-widget"}])
        (should= 0 (count (api/find-by- db :bibelot :id -1)))
        (should= "offline-widget" (:name (api/entity- db :bibelot 9001)))))))
