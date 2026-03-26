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

  (context "online-fn"

    (it "defaults to (constantly true) when :online? not provided"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-online-1"} [bibelot])]
        (should= true ((.-online-fn db)))))

    (it "uses the provided :online? callback"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-online-2" :online? #(deref online?)} [bibelot])]
        (should= true ((.-online-fn db)))
        (reset! online? false)
        (should= false ((.-online-fn db)))))
    )

  (context "offline tx"

    (before (reset! idb/offline-id-counter 0))

    (it "offline create assigns negative ID and marks dirty"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-offline-1" :online? (constantly false)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "offline-widget"})]
                       (should= -1 (:id saved))
                       (should= "offline-widget" (:name saved))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= #{-1} dirty)
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-1"))))))

    (it "online create uses positive ID with no dirty tracking"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-offline-2"} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "online-widget"})]
                       (should (pos? (:id saved)))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= #{} dirty)
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-2"))))))

    (it "offline update marks existing entity dirty"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-offline-3" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "widget" :size 5})]
                       (reset! online? false)
                       (api/-tx db (assoc saved :size 10))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= 1 (count dirty))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-3"))))))

    (it "offline delete of server-known entity creates tombstone and marks dirty"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-offline-4" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "widget" :size 5})]
                       (reset! online? false)
                       (api/-tx db (assoc saved :db/delete? true))
                       (should= 0 (count (api/find-by- db :bibelot :name "widget")))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= 1 (count dirty))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-4"))))))

    (it "offline delete of offline-created entity cleans up without dirty tracking"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-offline-5" :online? (constantly false)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "offline-widget"})]
                       (should= -1 (:id saved))
                       (api/-tx db (assoc saved :db/delete? true))
                       (should= 0 (count (api/find-by- db :bibelot :name "offline-widget")))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= #{} dirty)
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-5"))))))

    (it "offline batch create assigns negative IDs and marks dirty"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-offline-6" :online? (constantly false)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [results (api/-tx* db [{:kind :bibelot :name "w1"} {:kind :bibelot :name "w2"}])]
                       (should= -1 (:id (first results)))
                       (should= -2 (:id (second results)))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= #{-1 -2} dirty)
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-6"))))))

    (it "mixed creates and deletes in offline batch"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-offline-7" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "existing"})]
                       (reset! online? false)
                       (api/-tx* db [{:kind :bibelot :name "new-offline"} (assoc saved :db/delete? true)])
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= 2 (count dirty))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-7"))))))

    )

  (context "sync lifecycle"

    (before (reset! idb/offline-id-counter 0))

    (it "sync! provides dirty entities to callback"
      (let [db       (api/create-db {:impl :indexeddb :db-name "test-idb-sync-1" :online? (constantly false)} [bibelot])
            received (atom nil)]
        (-> (idb/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :name "w1" :size 1})
                     (api/-tx db {:kind :bibelot :name "w2" :size 2})
                     (idb/sync! db (fn [entities] (reset! received entities)))))
            (.then (fn [_]
                     (should= 2 (count @received))
                     (should= #{-1 -2} (into #{} (map :id) @received))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-sync-1"))))))

    (it "sync! includes tombstones for offline-deleted server entities"
      (let [online?  (atom true)
            db       (api/create-db {:impl :indexeddb :db-name "test-idb-sync-2" :online? #(deref online?)} [bibelot])
            received (atom nil)]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "widget" :size 5})]
                       (reset! online? false)
                       (api/-tx db (assoc saved :db/delete? true))
                       (idb/sync! db (fn [entities] (reset! received entities))))))
            (.then (fn [_]
                     (should= 1 (count @received))
                     (should (:db/delete? (first @received)))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-sync-2"))))))

    (it "sync! returns empty vector when nothing is dirty"
      (let [db       (api/create-db {:impl :indexeddb :db-name "test-idb-sync-3"} [bibelot])
            received (atom nil)]
        (-> (idb/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :name "widget" :size 5})
                     (idb/sync! db (fn [entities] (reset! received entities)))))
            (.then (fn [_]
                     (should= 0 (count @received))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-sync-3"))))))

    (it "sync-complete! clears dirty state and replaces temp entities"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-sync-4" :online? (constantly false)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :name "offline-widget" :size 5})
                     (idb/sync-complete! db #{-1} [{:kind :bibelot :id 9001 :name "offline-widget" :size 5}])))
            (.then (fn [_]
                     (should= 0 (count (api/find-by- db :bibelot :id -1)))
                     (let [found (api/entity- db :bibelot 9001)]
                       (should= "offline-widget" (:name found)))
                     (idb/read-dirty-set @(.-idb-atom db))))
            (.then (fn [dirty]
                     (should= #{} dirty)
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-sync-4"))))))

    (it "sync-complete! clears tombstones from IDB"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-sync-5" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "widget" :size 5})]
                       (reset! online? false)
                       (api/-tx db (assoc saved :db/delete? true))
                       (idb/sync-complete! db #{(:id saved)} []))))
            (.then (fn [_]
                     (idb/read-dirty-set @(.-idb-atom db))))
            (.then (fn [dirty]
                     (should= #{} dirty)
                     (reset! (.-store db) {:all {}})
                     (idb/rehydrate! db)))
            (.then (fn [db]
                     (should= 0 (count (api/find-by- db :bibelot :name "widget")))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-sync-5"))))))

    )

  (context "refresh!"

    (before (reset! idb/offline-id-counter 0))

    (it "purges negative-ID entities and loads fresh data"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-refresh-1"} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :id -1 :name "offline-widget" :size 5})
                     (api/-tx db {:kind :bibelot :id 100 :name "server-widget" :size 10})
                     (idb/refresh! db [{:kind :bibelot :id 200 :name "fresh-widget" :size 20}])))
            (.then (fn [_]
                     (should= 0 (count (filter #(neg? (:id %)) (api/find-by- db :bibelot :name "offline-widget"))))
                     (let [e100 (api/entity- db :bibelot 100)]
                       (should= "server-widget" (:name e100)))
                     (let [e200 (api/entity- db :bibelot 200)]
                       (should= "fresh-widget" (:name e200)))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-refresh-1"))))))

    (it "returns empty list for empty input"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-refresh-2" :online? (constantly false)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :name "offline-widget" :size 5})
                     (let [result (idb/refresh! db [])]
                       (should= [] result)
                       (should= 1 (count (api/find-by- db :bibelot :name "offline-widget")))
                       (api/close db)
                       (.deleteDatabase js/indexedDB "test-idb-refresh-2")))))))

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
