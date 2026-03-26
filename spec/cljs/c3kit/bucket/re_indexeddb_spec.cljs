(ns c3kit.bucket.re-indexeddb-spec
  (:require-macros [speclj.core :refer [around around-all before context describe it should should= should-not-be-nil]]
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

  (context "persistence"

    (it "persists and rehydrates a single entity"
      (let [db (api/create-db {:impl :re-indexeddb :db-name "test-reidb-1"} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db] (api/-tx db {:kind :bibelot :name "widget" :size 5})))
            (.then (fn [saved]
                     (should= "widget" (:name saved))
                     (should-not-be-nil (:id saved))
                     (reset! (.-store db) {:all {}})
                     (idb/rehydrate! db)))
            (.then (fn [db]
                     (let [found (api/find-by- db :bibelot :name "widget")]
                       (should= 1 (count found))
                       (should= "widget" (:name (first found))))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-reidb-1"))))))

    (it "persists and rehydrates multiple entity kinds"
      (let [db (api/create-db {:impl :re-indexeddb :db-name "test-reidb-2"} [bibelot thingy])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (api/-tx* db [{:kind :bibelot :name "widget" :size 5}
                                   {:kind :thingy :name "gadget" :foo "bar"}])))
            (.then (fn [_]
                     (reset! (.-store db) {:all {}})
                     (idb/rehydrate! db)))
            (.then (fn [db]
                     (should= 1 (count (api/find-by- db :bibelot :name "widget")))
                     (should= 1 (count (api/find-by- db :thingy :name "gadget")))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-reidb-2"))))))

    (it "deletes entity from IDB"
      (let [db (api/create-db {:impl :re-indexeddb :db-name "test-reidb-3"} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db] (api/-tx db {:kind :bibelot :name "widget" :size 5})))
            (.then (fn [saved] (api/-tx db (assoc saved :db/delete? true))))
            (.then (fn [_]
                     (reset! (.-store db) {:all {}})
                     (idb/rehydrate! db)))
            (.then (fn [db]
                     (should= 0 (count (api/find-by- db :bibelot :name "widget")))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-reidb-3"))))))

    (it "updates existing entity in IDB"
      (let [db (api/create-db {:impl :re-indexeddb :db-name "test-reidb-5"} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db] (api/-tx db {:kind :bibelot :name "widget" :size 5})))
            (.then (fn [saved] (api/-tx db (assoc saved :size 10))))
            (.then (fn [_]
                     (reset! (.-store db) {:all {}})
                     (idb/rehydrate! db)))
            (.then (fn [db]
                     (let [found (first (api/find-by- db :bibelot :name "widget"))]
                       (should= 10 (:size found)))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-reidb-5"))))))
    )

  (context "online-fn"

    (it "defaults to (constantly true) when :online? not provided"
      (let [db (api/create-db {:impl :re-indexeddb :db-name "test-reidb-online-1"} [bibelot])]
        (should= true ((.-online-fn db)))))

    (it "uses the provided :online? callback"
      (let [online? (atom true)
            db      (api/create-db {:impl :re-indexeddb :db-name "test-reidb-online-2" :online? #(deref online?)} [bibelot])]
        (should= true ((.-online-fn db)))
        (reset! online? false)
        (should= false ((.-online-fn db)))))
    )

  (context "offline tx"

    (before (reset! idb/offline-id-counter 0))

    (it "assigns negative ID and marks dirty on offline create"
      (let [online? (atom false)
            db      (api/create-db {:impl :re-indexeddb :db-name "test-reidb-offline-1" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "offline-widget"})]
                       (should= -1 (:id saved))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= #{-1} dirty)
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-reidb-offline-1"))))))

    (it "sync-complete! works with reagent store"
      (let [online? (atom false)
            db      (api/create-db {:impl :re-indexeddb :db-name "test-reidb-offline-2" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :name "offline-widget"})
                     (idb/sync-complete! db #{-1} [{:kind :bibelot :id 9001 :name "offline-widget"}])))
            (.then (fn [_]
                     (should= 0 (count (api/find-by- db :bibelot :id -1)))
                     (let [found (api/entity- db :bibelot 9001)]
                       (should= "offline-widget" (:name found)))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-reidb-offline-2"))))))
    )

  (context "rollback on IDB failure"

    (it "rolls back reagent store on failed tx"
      (let [db           (api/create-db {:impl :re-indexeddb :db-name "test-reidb-rollback-1"} [bibelot])]
        (reset! api/impl db)
        (api/-tx db {:kind :bibelot :name "existing" :size 1})
        (let [store-before @(.-store db)]
          (reset! (.-idb-atom db) #js {:transaction (fn [] (throw (js/Error. "closed")))})
          (api/-tx db {:kind :bibelot :name "should-rollback" :size 99})
          (should= 1 (count (api/find-by- db :bibelot :name "should-rollback")))
          (js/Promise.
           (fn [resolve _]
             (js/setTimeout
              (fn []
                (should= store-before @(.-store db))
                (should= 0 (count (api/find-by- db :bibelot :name "should-rollback")))
                (resolve nil))
              50))))))

    (it "rolls back reagent store on failed tx*"
      (let [db           (api/create-db {:impl :re-indexeddb :db-name "test-reidb-rollback-2"} [bibelot])]
        (reset! api/impl db)
        (api/-tx db {:kind :bibelot :name "existing" :size 1})
        (let [store-before @(.-store db)]
          (reset! (.-idb-atom db) #js {:transaction (fn [] (throw (js/Error. "closed")))})
          (api/-tx* db [{:kind :bibelot :name "fail-1"} {:kind :bibelot :name "fail-2"}])
          (should= 1 (count (api/find-by- db :bibelot :name "fail-1")))
          (js/Promise.
           (fn [resolve _]
             (js/setTimeout
              (fn []
                (should= store-before @(.-store db))
                (should= 0 (count (api/find-by- db :bibelot :name "fail-1")))
                (should= 0 (count (api/find-by- db :bibelot :name "fail-2")))
                (resolve nil))
              50))))))
    )
  )
