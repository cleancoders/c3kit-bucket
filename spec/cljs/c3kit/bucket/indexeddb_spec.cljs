(ns c3kit.bucket.indexeddb-spec
  (:require-macros [speclj.core :refer [around around-all before context describe it should should= should-not-be-nil with]]
                   [c3kit.bucket.api :refer [with-safety-off]])
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.idb :as idb]
            [c3kit.bucket.indexeddb]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.memory-spec :as memory-spec]
            [c3kit.bucket.spec-helperc :as helperc]
            [c3kit.apron.schema :as s]
            [speclj.core]))

(def config {:impl :indexeddb :db-name "test-indexeddb"})

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

(describe "IndexedDB"

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

  (context "persistence"

    (it "persists and rehydrates a single entity"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-1"} [bibelot])]
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
                       (should= "widget" (:name (first found)))
                       (should= 5 (:size (first found))))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-1"))))))

    (it "persists and rehydrates multiple entity kinds"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-2"} [bibelot thingy])]
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
                     (.deleteDatabase js/indexedDB "test-idb-2"))))))

    (it "deletes entity from IDB"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-3"} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db] (api/-tx db {:kind :bibelot :name "widget" :size 5})))
            (.then (fn [saved] (api/-tx db (assoc saved :db/delete? true))))
            (.then (fn [_]
                     (reset! (.-store db) {:all {}})
                     (idb/rehydrate! db)))
            (.then (fn [db]
                     (should= 0 (count (api/find-by- db :bibelot :name "widget")))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-3"))))))

    (it "rehydrates only specified kinds"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-4"} [bibelot thingy])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (api/-tx* db [{:kind :bibelot :name "widget"}
                                   {:kind :thingy :name "gadget"}])))
            (.then (fn [_]
                     (reset! (.-store db) {:all {}})
                     (idb/rehydrate! db :bibelot)))
            (.then (fn [db]
                     (should= 1 (count (api/find-by- db :bibelot :name "widget")))
                     (should= 0 (count (api/find-by- db :thingy :name "gadget")))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-4"))))))

    (it "updates existing entity in IDB"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-5"} [bibelot])]
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
                     (.deleteDatabase js/indexedDB "test-idb-5"))))))

    (it "generates id for entities without one"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-6"} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db] (api/-tx db {:kind :bibelot :name "auto-id"})))
            (.then (fn [saved]
                     (should-not-be-nil (:id saved))
                     (should= "auto-id" (:name saved))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-6"))))))
    )

  (context "rollback on IDB failure"

    (it "rolls back store on failed tx"
      (let [db          (api/create-db {:impl :indexeddb :db-name "test-idb-rollback-1"} [bibelot])
            _           (api/-tx db {:kind :bibelot :name "existing" :size 1})
            store-before @(.-store db)]
        ;; Point idb-atom to a closed/invalid db to force IDB write failure
        (reset! (.-idb-atom db) #js {:transaction (fn [] (throw (js/Error. "closed")))})
        (api/-tx db {:kind :bibelot :name "should-rollback" :size 99})
        ;; Entity appears optimistically in memory
        (should= 1 (count (api/find-by- db :bibelot :name "should-rollback")))
        ;; After the promise rejects, store should roll back
        (js/Promise.
         (fn [resolve _]
           (js/setTimeout
            (fn []
              (should= store-before @(.-store db))
              (should= 0 (count (api/find-by- db :bibelot :name "should-rollback")))
              (resolve nil))
            50)))))

    (it "rolls back store on failed tx*"
      (let [db          (api/create-db {:impl :indexeddb :db-name "test-idb-rollback-2"} [bibelot])
            _           (api/-tx db {:kind :bibelot :name "existing" :size 1})
            store-before @(.-store db)]
        (reset! (.-idb-atom db) #js {:transaction (fn [] (throw (js/Error. "closed")))})
        (api/-tx* db [{:kind :bibelot :name "fail-1"} {:kind :bibelot :name "fail-2"}])
        ;; Entities appear optimistically
        (should= 1 (count (api/find-by- db :bibelot :name "fail-1")))
        ;; After the promise rejects, store should roll back
        (js/Promise.
         (fn [resolve _]
           (js/setTimeout
            (fn []
              (should= store-before @(.-store db))
              (should= 0 (count (api/find-by- db :bibelot :name "fail-1")))
              (should= 0 (count (api/find-by- db :bibelot :name "fail-2")))
              (resolve nil))
            50)))))
    )
  )
