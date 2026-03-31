(ns c3kit.bucket.indexeddb-spec
  (:require-macros [speclj.core :refer [around before context describe it should should= should-not-be-nil]]
                   [c3kit.bucket.api :refer [with-safety-off]])
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as idb]
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

  (context "online-fn"

    (it "defaults to (constantly true) when :online? not provided"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-online-1"} [bibelot])]
        (should= true ((.-online-fn db)))))

    (it "uses the provided :online? callback"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-online-2" :online? #(deref online?)} [bibelot])]
        (should= true ((.-online-fn db)))
        (reset! online? false)
        (should= false ((.-online-fn db))))))

  (context "idb-strategy"

    (it "defaults to :primary when not provided"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-strategy-1"} [bibelot])]
        (should= :primary (.-strategy db))))

    (it "uses the provided strategy"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-strategy-2" :idb-strategy :cache} [bibelot])]
        (should= :cache (.-strategy db))))

    (it "cache strategy clears memory store on init when online"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-strategy-3"
                               :idb-strategy :cache :online? (constantly true)} [bibelot])]
        (api/-tx db {:kind :bibelot :name "stale-widget" :size 5})
        (should= 1 (count (api/find-by- db :bibelot :name "stale-widget")))
        (idb/init! db)
        (should= 0 (count (api/find-by- db :bibelot :name "stale-widget")))))

    (it "cache strategy keeps memory store on init when offline"
      (let [online? (atom false)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-strategy-4"
                                    :idb-strategy :cache :online? #(deref online?)} [bibelot])]
        (api/-tx db {:kind :bibelot :name "offline-widget" :size 5})
        (should= 1 (count (api/find-by- db :bibelot :name "offline-widget")))
        (idb/init! db)
        (should= 1 (count (api/find-by- db :bibelot :name "offline-widget")))))

    (it "primary strategy keeps memory store on init when online"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-strategy-5"
                               :idb-strategy :primary :online? (constantly true)} [bibelot])]
        (api/-tx db {:kind :bibelot :name "primary-widget" :size 5})
        (should= 1 (count (api/find-by- db :bibelot :name "primary-widget")))
        (idb/init! db)
        (should= 1 (count (api/find-by- db :bibelot :name "primary-widget"))))))

  (context "offline tx (memory effects)"

    (before (reset! idb/offline-id-counter 0))

    (it "offline create assigns negative ID"
      (let [db    (api/create-db {:impl :indexeddb :db-name "test-mem-1" :online? (constantly false)} [bibelot])
            saved (api/-tx db {:kind :bibelot :name "offline-widget"})]
        (should= -1 (:id saved))
        (should= "offline-widget" (:name saved))
        (should= 1 (count (api/find-by- db :bibelot :name "offline-widget")))))

    (it "online create uses positive ID"
      (let [db    (api/create-db {:impl :indexeddb :db-name "test-mem-2"} [bibelot])
            saved (api/-tx db {:kind :bibelot :name "online-widget"})]
        (should (pos? (:id saved)))))

    (it "offline update changes entity in memory"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-mem-3" :online? #(deref online?)} [bibelot])
            saved   (api/-tx db {:kind :bibelot :name "widget" :size 5})]
        (reset! online? false)
        (api/-tx db (assoc saved :size 10))
        (should= 10 (:size (api/entity- db :bibelot (:id saved))))))

    (it "offline delete removes entity from memory"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-mem-4" :online? #(deref online?)} [bibelot])
            saved   (api/-tx db {:kind :bibelot :name "widget" :size 5})]
        (reset! online? false)
        (api/-tx db (assoc saved :db/delete? true))
        (should= 0 (count (api/find-by- db :bibelot :name "widget")))))

    (it "offline delete of offline-created entity"
      (let [db    (api/create-db {:impl :indexeddb :db-name "test-mem-5" :online? (constantly false)} [bibelot])
            saved (api/-tx db {:kind :bibelot :name "offline-widget"})]
        (should= -1 (:id saved))
        (api/-tx db (assoc saved :db/delete? true))
        (should= 0 (count (api/find-by- db :bibelot :name "offline-widget")))))

    (it "offline batch create assigns negative IDs"
      (let [db      (api/create-db {:impl :indexeddb :db-name "test-mem-6" :online? (constantly false)} [bibelot])
            results (api/-tx* db [{:kind :bibelot :name "w1"} {:kind :bibelot :name "w2"}])]
        (should= -1 (:id (first results)))
        (should= -2 (:id (second results)))))

    (it "mixed creates and deletes in offline batch"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-mem-7" :online? #(deref online?)} [bibelot])
            saved   (api/-tx db {:kind :bibelot :name "existing"})]
        (reset! online? false)
        (let [results (api/-tx* db [{:kind :bibelot :name "new-offline"} (assoc saved :db/delete? true)])]
          (should= 1 (count (api/find-by- db :bibelot :name "new-offline")))
          (should= 0 (count (api/find-by- db :bibelot :name "existing")))))))

  (context "sync lifecycle (memory effects)"

    (before (reset! idb/offline-id-counter 0))

    (it "sync! with nil idb invokes callback with empty vector"
      (let [db       (api/create-db {:impl :indexeddb :db-name "test-sync-mem-1"} [bibelot])
            received (atom nil)]
        (idb/sync! db (fn [entities] (reset! received entities)))
        (should= [] @received)))

    (it "sync-complete! soft-deletes negative IDs and adds server entities"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-sync-mem-2" :online? (constantly false)} [bibelot])]
        (api/-tx db {:kind :bibelot :name "offline-widget" :size 5})
        (idb/sync-complete! db #{-1} [{:kind :bibelot :id 9001 :name "offline-widget" :size 5}])
        (should= 0 (count (api/find-by- db :bibelot :id -1)))
        (should= "offline-widget" (:name (api/entity- db :bibelot 9001))))))

  (context "refresh! (memory effects)"

    (before (reset! idb/offline-id-counter 0))

    (it "purges negative-ID entities and loads fresh data"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-refresh-mem-1"} [bibelot])]
        (api/-tx db {:kind :bibelot :id -1 :name "offline-widget" :size 5})
        (api/-tx db {:kind :bibelot :id 100 :name "server-widget" :size 10})
        (idb/refresh! db [{:kind :bibelot :id 200 :name "fresh-widget" :size 20}])
        (should= 0 (count (filter #(neg? (:id %)) (api/find-by- db :bibelot :name "offline-widget"))))
        (should= "server-widget" (:name (api/entity- db :bibelot 100)))
        (should= "fresh-widget" (:name (api/entity- db :bibelot 200)))))

    (it "returns empty list for empty input"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-refresh-mem-2" :online? (constantly false)} [bibelot])]
        (api/-tx db {:kind :bibelot :name "offline-widget" :size 5})
        (let [result (idb/refresh! db [])]
          (should= [] result)
          (should= 1 (count (api/find-by- db :bibelot :name "offline-widget"))))))))
