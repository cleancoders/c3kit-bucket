# Offline Sync Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add offline sync capabilities to bucket's `:indexeddb` and `:re-indexeddb` implementations — dirty tracking via `_meta` store, negative offline IDs, tombstone deletes, sync/complete lifecycle, and a lightweight service-worker-compatible reader.

**Architecture:** The existing `idb.cljs` gains offline-aware transaction logic gated by an optional `:online?` callback in the config. A new `idb-common.cljs` namespace holds serialization and IDB open logic shared between the main thread and service worker. A new `idb-reader.cljs` provides a minimal SW-compatible API for reading and clearing dirty data.

**Tech Stack:** ClojureScript, IndexedDB, Speclj, Reagent (for re-indexeddb tests)

**Test runner:** `clj -M:test:cljs once` (one-time) or `clj -M:test:cljs` (auto-runner, preferred for TDD). Run from `/Users/AlexRoot-Roatch/current-projects/c3kit-bucket`.

---

## File Structure

| File | Role | Action |
|---|---|---|
| `src/cljs/c3kit/bucket/idb_common.cljs` | Serialization, version mgmt, IDB open — shared by main thread and SW | Create |
| `src/cljs/c3kit/bucket/idb.cljs` | Offline-aware transactions, dirty set mgmt, sync API, negative IDs | Modify |
| `src/cljs/c3kit/bucket/idb_reader.cljs` | SW-compatible dirty entity reader | Create |
| `src/cljs/c3kit/bucket/indexeddb.cljs` | Deftype gains `online-fn` field | Modify |
| `src/cljs/c3kit/bucket/re_indexeddb.cljs` | Deftype gains `online-fn` field | Modify |
| `spec/cljs/c3kit/bucket/idb_common_spec.cljs` | Tests for extracted shared logic | Create |
| `spec/cljs/c3kit/bucket/idb_spec.cljs` | Tests for dirty tracking, negative IDs, sync lifecycle | Modify |
| `spec/cljs/c3kit/bucket/idb_reader_spec.cljs` | Tests for SW reader | Create |
| `spec/cljs/c3kit/bucket/indexeddb_spec.cljs` | Tests for offline-aware persistence | Modify |
| `spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs` | Tests for offline-aware persistence (reagent) | Modify |

---

## Task 1: Extract `idb-common` from `idb`

Move serialization, version management, and IDB open logic into a new shared namespace. This is a pure refactor — no behavior change. After this task, `idb.cljs` requires `idb-common` instead of implementing these directly.

**Files:**
- Create: `src/cljs/c3kit/bucket/idb_common.cljs`
- Create: `spec/cljs/c3kit/bucket/idb_common_spec.cljs`
- Modify: `src/cljs/c3kit/bucket/idb.cljs`
- Modify: `spec/cljs/c3kit/bucket/idb_spec.cljs`

- [ ] **Step 1: Create `idb_common.cljs` with extracted functions**

```clojure
(ns c3kit.bucket.idb-common
  (:require [cljs.reader]))

;region Schema Hashing

(defn schema-hash [legend]
  (hash (into (sorted-map)
              (map (fn [[k v]] [k (into (sorted-map) v)]))
              legend)))

;endregion

;region Version Management

(defn idb-version [db-name legend]
  (let [current-hash (str (schema-hash legend))
        hash-key     (str db-name "-schema-hash")
        ver-key      (str db-name "-schema-ver")
        stored-hash  (.getItem js/localStorage hash-key)
        stored-ver   (or (some-> (.getItem js/localStorage ver-key) js/parseInt) 0)]
    (if (= current-hash stored-hash)
      stored-ver
      (let [new-ver (inc stored-ver)]
        (.setItem js/localStorage hash-key current-hash)
        (.setItem js/localStorage ver-key (str new-ver))
        new-ver))))

;endregion

;region Database Operations

(defn- ensure-object-stores [db legend]
  (let [existing (set (array-seq (.-objectStoreNames db)))
        expected (conj (set (map name (keys legend))) "_meta")]
    (doseq [store-name expected]
      (when-not (contains? existing store-name)
        (.createObjectStore db store-name #js {:keyPath "id"})))
    (doseq [store-name existing]
      (when-not (contains? expected store-name)
        (.deleteObjectStore db store-name)))))

(defn open [db-name legend]
  (let [version (idb-version db-name legend)]
    (js/Promise.
     (fn [resolve reject]
       (let [request (.open js/indexedDB db-name version)]
         (set! (.-onupgradeneeded request)
               (fn [event]
                 (let [db (.-result (.-target event))]
                   (ensure-object-stores db legend))))
         (set! (.-onsuccess request)
               (fn [event]
                 (resolve (.-result (.-target event)))))
         (set! (.-onerror request)
               (fn [event]
                 (reject (.-error (.-target event))))))))))

(defn close [db]
  (when db (.close db)))

;endregion

;region Serialization

(defn clj->js-entity [entity]
  #js {:id (:id entity) :data (pr-str entity)})

(defn js->clj-entity [js-obj]
  (when js-obj
    (cljs.reader/read-string (.-data js-obj))))

;endregion

;region Store Reading

(defn read-store [idb store-name]
  (js/Promise.
   (fn [resolve reject]
     (let [tx      (.transaction idb #js [store-name] "readonly")
           store   (.objectStore tx store-name)
           request (.getAll store)]
       (set! (.-onsuccess request)
             (fn [event]
               (resolve (map js->clj-entity (array-seq (.-result (.-target event)))))))
       (set! (.-onerror request)
             (fn [event]
               (reject (.-error (.-target event)))))))))

;endregion
```

- [ ] **Step 2: Create `idb_common_spec.cljs` with tests moved from `idb_spec.cljs`**

```clojure
(ns c3kit.bucket.idb-common-spec
  (:require-macros [speclj.core :refer [before context describe it should should= should-not=]])
  (:require [c3kit.bucket.idb-common :as sut]
            [speclj.core]))

(describe "IDB Common"

  (context "schema-hash"
    (it "produces a consistent hash from a legend"
      (let [legend {:user {:id {:type :long} :name {:type :string}}}]
        (should= (sut/schema-hash legend) (sut/schema-hash legend))))

    (it "produces different hashes for different legends"
      (let [legend-1 {:user {:id {:type :long} :name {:type :string}}}
            legend-2 {:user {:id {:type :long} :name {:type :string} :email {:type :string}}}]
        (should-not= (sut/schema-hash legend-1) (sut/schema-hash legend-2)))))

  (context "idb-version"
    (before (.removeItem js/localStorage "test-db-schema-hash")
            (.removeItem js/localStorage "test-db-schema-ver"))

    (it "returns 1 for a new database"
      (let [legend {:user {:id {:type :long} :name {:type :string}}}]
        (should= 1 (sut/idb-version "test-db" legend))))

    (it "returns same version when legend unchanged"
      (let [legend {:user {:id {:type :long} :name {:type :string}}}]
        (sut/idb-version "test-db" legend)
        (should= 1 (sut/idb-version "test-db" legend))))

    (it "increments version when legend changes"
      (let [legend-1 {:user {:id {:type :long} :name {:type :string}}}
            legend-2 {:user {:id {:type :long} :name {:type :string} :email {:type :string}}}]
        (sut/idb-version "test-db" legend-1)
        (should= 2 (sut/idb-version "test-db" legend-2)))))

  (context "serialization"
    (it "round-trips keyword values"
      (let [entity {:id 1 :kind :bibelot :name "widget" :color "red"}]
        (should= entity (sut/js->clj-entity (sut/clj->js-entity entity)))))

    (it "round-trips entities with vector values"
      (let [entity {:id 2 :kind :doodad :names ["alice" "bob"]}]
        (should= entity (sut/js->clj-entity (sut/clj->js-entity entity)))))

    (it "round-trips entities with keyword vector values"
      (let [entity {:id 3 :kind :doodad :letters [:a :b :c]}]
        (should= entity (sut/js->clj-entity (sut/clj->js-entity entity)))))

    (it "round-trips nil for nil input"
      (should= nil (sut/js->clj-entity nil)))))
```

- [ ] **Step 3: Update `idb.cljs` to require `idb-common` and remove extracted functions**

Replace the `ns` declaration and remove the functions that moved to `idb-common`. Keep all remaining functions (`put-entity`, `put-entities`, `delete-entity`, `clear-store`, `clear-all`, `rehydrate`, `prepare-entity`, `idb-tx`, `idb-tx*`, `init!`, `rehydrate!`). Update internal calls to use the `common/` prefix.

The updated `idb.cljs` should look like:

```clojure
(ns c3kit.bucket.idb
  (:refer-clojure :rename {reduce core-reduce})
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as common]
            [c3kit.bucket.memory :as memory]))

;region Entity Operations

(defn put-entity [idb entity]
  (js/Promise.
   (fn [resolve reject]
     (let [store-name (name (:kind entity))
           tx         (.transaction idb #js [store-name] "readwrite")
           store      (.objectStore tx store-name)
           request    (.put store (common/clj->js-entity entity))]
       (set! (.-onsuccess request) (fn [_] (resolve entity)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))

(defn put-entities [idb entities]
  (if (empty? entities)
    (js/Promise.resolve entities)
    (let [store-names (into #{} (map #(name (:kind %))) entities)]
      (js/Promise.
       (fn [resolve reject]
         (let [tx (.transaction idb (clj->js (vec store-names)) "readwrite")]
           (set! (.-oncomplete tx) (fn [_] (resolve entities)))
           (set! (.-onerror tx) (fn [event] (reject (.-error (.-target event)))))
           (set! (.-onabort tx) (fn [event] (reject (.-error (.-target event)))))
           (doseq [entity entities]
             (let [store (.objectStore tx (name (:kind entity)))]
               (.put store (common/clj->js-entity entity))))))))))

(defn delete-entity [idb kind id]
  (js/Promise.
   (fn [resolve reject]
     (let [store-name (name kind)
           tx         (.transaction idb #js [store-name] "readwrite")
           store      (.objectStore tx store-name)
           request    (.delete store id)]
       (set! (.-onsuccess request) (fn [_] (resolve nil)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))

(defn clear-store [idb kind]
  (js/Promise.
   (fn [resolve reject]
     (let [store-name (name kind)
           tx         (.transaction idb #js [store-name] "readwrite")
           store      (.objectStore tx store-name)
           request    (.clear store)]
       (set! (.-onsuccess request) (fn [_] (resolve nil)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))

(defn clear-all [idb]
  (let [store-names (->> (array-seq (.-objectStoreNames idb))
                         (remove #(= "_meta" %)))]
    (if (empty? store-names)
      (js/Promise.resolve nil)
      (js/Promise.
       (fn [resolve reject]
         (let [tx (.transaction idb (clj->js (vec store-names)) "readwrite")]
           (set! (.-oncomplete tx) (fn [_] (resolve nil)))
           (set! (.-onerror tx) (fn [event] (reject (.-error (.-target event)))))
           (doseq [store-name store-names]
             (.clear (.objectStore tx store-name)))))))))

(defn rehydrate [idb kinds tx-fn]
  (let [store-names (if (seq kinds)
                      (map name kinds)
                      (->> (array-seq (.-objectStoreNames idb))
                           (remove #(= "_meta" %))))]
    (-> (js/Promise.all (clj->js (map #(common/read-store idb %) store-names)))
        (.then (fn [results]
                 (let [all-entities (mapcat identity (array-seq results))]
                   (when (seq all-entities)
                     (tx-fn all-entities))))))))

;endregion

;region Shared Transaction Helpers

(defn- prepare-entity
  [db entity]
  (let [entity    (memory/ensure-id entity)
        new-store (memory/tx-entity @(.-legend db) @(.-store db) entity)
        result    (if (api/delete? entity)
                    (api/soft-delete entity)
                    (get-in new-store [:all (:id entity)]))]
    [new-store result]))

(defn idb-tx
  [db entity]
  (let [old-store @(.-store db)
        [new-store result] (prepare-entity db entity)]
    (reset! (.-store db) new-store)
    (when @(.-idb-atom db)
      (-> (if (api/delete? entity)
            (delete-entity @(.-idb-atom db) (:kind entity) (:id entity))
            (put-entity @(.-idb-atom db) result))
          (.catch (fn [_] (reset! (.-store db) old-store)))))
    result))

(defn idb-tx*
  [db entities]
  (let [old-store  @(.-store db)
        entities   (map memory/ensure-id entities)
        new-store  (core-reduce #(memory/tx-entity @(.-legend db) %1 %2) @(.-store db) entities)
        results    (mapv (fn [e]
                           (if (api/delete? e)
                             (api/soft-delete e)
                             (get-in new-store [:all (:id e)])))
                         entities)
        to-persist (remove api/delete? results)
        to-delete  (filter api/delete? entities)]
    (reset! (.-store db) new-store)
    (when @(.-idb-atom db)
      (-> (js/Promise.all
            (clj->js (cond-> []
                       (seq to-persist) (conj (put-entities @(.-idb-atom db) to-persist))
                       (seq to-delete)  (into (map #(delete-entity @(.-idb-atom db) (:kind %) (:id %)) to-delete)))))
          (.catch (fn [_] (reset! (.-store db) old-store)))))
    results))

(defn init!
  [db & kinds]
  (-> (common/open (.-db-name db) @(.-legend db))
      (.then (fn [idb-instance]
               (reset! (.-idb-atom db) idb-instance)
               db))
      (.then (fn [db]
               (rehydrate @(.-idb-atom db) (seq kinds)
                          (fn [entities] (memory/tx* db entities)))))
      (.then (fn [_] db))))

(defn rehydrate!
  [db & kinds]
  (-> (rehydrate @(.-idb-atom db) (seq kinds)
                 (fn [entities] (memory/tx* db entities)))
      (.then (fn [_] db))))

;endregion
```

- [ ] **Step 4: Update `idb_spec.cljs` to remove tests that moved to `idb_common_spec.cljs`**

The `idb_spec.cljs` should now only contain tests that remain specific to `idb.cljs` (none currently — the schema-hash, version, and serialization tests all moved). Replace the file with a minimal placeholder that will grow in later tasks:

```clojure
(ns c3kit.bucket.idb-spec
  (:require-macros [speclj.core :refer [describe]])
  (:require [c3kit.bucket.idb]
            [speclj.core]))

(describe "IDB")
```

- [ ] **Step 5: Run tests to verify the refactor is clean**

Run: `clj -M:test:cljs once` from `/Users/AlexRoot-Roatch/current-projects/c3kit-bucket`
Expected: All existing tests pass. The `idb-common-spec` tests pass. The `indexeddb-spec` and `re-indexeddb-spec` persistence tests pass (they use `idb/init!` which now delegates to `common/open`).

- [ ] **Step 6: Commit**

```bash
git add src/cljs/c3kit/bucket/idb_common.cljs spec/cljs/c3kit/bucket/idb_common_spec.cljs src/cljs/c3kit/bucket/idb.cljs spec/cljs/c3kit/bucket/idb_spec.cljs
git commit -m "extract idb-common from idb for shared serialization and versioning"
```

---

## Task 2: Add `online-fn` field to deftypes

Both `IndexedDB` and `ReIndexedDB` gain an `online-fn` field sourced from the `:online?` config key. Defaults to `(constantly true)`. No behavior change yet — just threading the field through.

**Files:**
- Modify: `src/cljs/c3kit/bucket/indexeddb.cljs`
- Modify: `src/cljs/c3kit/bucket/re_indexeddb.cljs`
- Modify: `spec/cljs/c3kit/bucket/indexeddb_spec.cljs`
- Modify: `spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs`

- [ ] **Step 1: Write test for `online-fn` on IndexedDB**

Add to `indexeddb_spec.cljs` after the existing `"persistence"` context:

```clojure
  (context "online-fn"

    (it "defaults to constantly true when :online? not provided"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-online-1"} [bibelot])]
        (should= true ((.-online-fn db)))))

    (it "uses provided :online? callback"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-online-2" :online? #(deref online?)} [bibelot])]
        (should= true ((.-online-fn db)))
        (reset! online? false)
        (should= false ((.-online-fn db))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — `online-fn` field does not exist on the deftype.

- [ ] **Step 3: Add `online-fn` to `IndexedDB` deftype**

In `src/cljs/c3kit/bucket/indexeddb.cljs`, update the deftype and factory:

```clojure
(ns c3kit.bucket.indexeddb
  (:refer-clojure :rename {find core-find count core-count reduce core-reduce})
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb :as idb]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as migrator]))

;region IndexedDB deftype

(deftype IndexedDB [legend store idb-atom db-name online-fn]
  api/DB
  (-clear [this]
    (memory/clear this)
    (when @idb-atom (idb/clear-all @idb-atom)))
  (close [_this] (idb/close @idb-atom))
  (-count [this kind options] (core-count (api/-find this kind options)))
  (-delete-all [this kind]
    (memory/delete-all this kind)
    (when @idb-atom (idb/clear-store @idb-atom kind)))
  (-entity [this kind id] (memory/entity this kind id))
  (-find [this kind options] (memory/do-find this kind options))
  (-reduce [this kind f init options] (core-reduce f init (api/-find this kind options)))
  (-tx [this entity] (idb/idb-tx this entity))
  (-tx* [this entities] (idb/idb-tx* this entities))
  migrator/Migrator
  (-schema-exists? [_this schema] (contains? @legend (api/-schema-kind schema)))
  (-installed-schema-legend [_this _expected-legend] @legend)
  (-install-schema! [this schema] (memory/do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [_this kind attr spec] (swap! legend assoc-in [kind attr] spec))
  (-remove-attribute! [this kind attr] (memory/do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (memory/do-rename-attribute! this kind attr new-kind new-attr)))

;endregion

;region Registration

(defmethod api/-create-impl :indexeddb [config schemas]
  (let [legend    (atom (legend/build schemas))
        store     (or (:store config) (atom {}))
        idb-atom  (atom nil)
        db-name   (or (:db-name config) "c3kit-bucket")
        online-fn (or (:online? config) (constantly true))]
    (IndexedDB. legend store idb-atom db-name online-fn)))

;endregion
```

- [ ] **Step 4: Run test to verify it passes**

Run: `clj -M:test:cljs once`
Expected: PASS

- [ ] **Step 5: Write test for `online-fn` on ReIndexedDB**

Add to `re_indexeddb_spec.cljs` after the existing `"rollback on IDB failure"` context:

```clojure
  (context "online-fn"

    (it "defaults to constantly true when :online? not provided"
      (let [db (api/create-db {:impl :re-indexeddb :db-name "test-reidb-online-1"} [bibelot])]
        (should= true ((.-online-fn db)))))

    (it "uses provided :online? callback"
      (let [online? (atom true)
            db      (api/create-db {:impl :re-indexeddb :db-name "test-reidb-online-2" :online? #(deref online?)} [bibelot])]
        (should= true ((.-online-fn db)))
        (reset! online? false)
        (should= false ((.-online-fn db))))))
```

- [ ] **Step 6: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — `online-fn` field does not exist on ReIndexedDB.

- [ ] **Step 7: Add `online-fn` to `ReIndexedDB` deftype**

In `src/cljs/c3kit/bucket/re_indexeddb.cljs`, update the deftype and factory:

```clojure
(ns c3kit.bucket.re-indexeddb
  (:refer-clojure :rename {find core-find count core-count reduce core-reduce})
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb :as idb]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as migrator]
            [c3kit.bucket.re-memory :as re-memory]
            [reagent.core :as r]))

;region ReIndexedDB deftype

(deftype ReIndexedDB [legend store idb-atom db-name online-fn]
  api/DB
  (-clear [this]
    (memory/clear this)
    (when @idb-atom (idb/clear-all @idb-atom)))
  (close [_this] (idb/close @idb-atom))
  (-count [this kind options] (core-count (api/-find this kind options)))
  (-delete-all [this kind]
    (memory/delete-all this kind)
    (when @idb-atom (idb/clear-store @idb-atom kind)))
  (-entity [this kind id] (re-memory/entity this kind id))
  (-find [this kind options] (re-memory/do-find this kind options))
  (-reduce [this kind f init options] (core-reduce f init (api/-find this kind options)))
  (-tx [this entity] (idb/idb-tx this entity))
  (-tx* [this entities] (idb/idb-tx* this entities))
  migrator/Migrator
  (-schema-exists? [_this schema] (contains? @legend (api/-schema-kind schema)))
  (-installed-schema-legend [_this _expected-legend] @legend)
  (-install-schema! [this schema] (memory/do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [_this kind attr spec] (swap! legend assoc-in [kind attr] spec))
  (-remove-attribute! [this kind attr] (memory/do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (memory/do-rename-attribute! this kind attr new-kind new-attr)))

;endregion

;region Registration

(defmethod api/-create-impl :re-indexeddb [config schemas]
  (let [legend    (atom (legend/build schemas))
        store     (or (:store config) (r/atom {}))
        idb-atom  (atom nil)
        db-name   (or (:db-name config) "c3kit-bucket")
        online-fn (or (:online? config) (constantly true))]
    (ReIndexedDB. legend store idb-atom db-name online-fn)))

;endregion
```

- [ ] **Step 8: Run tests to verify all pass**

Run: `clj -M:test:cljs once`
Expected: All tests pass including both new `online-fn` contexts.

- [ ] **Step 9: Commit**

```bash
git add src/cljs/c3kit/bucket/indexeddb.cljs src/cljs/c3kit/bucket/re_indexeddb.cljs spec/cljs/c3kit/bucket/indexeddb_spec.cljs spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs
git commit -m "add online-fn field to IndexedDB and ReIndexedDB deftypes"
```

---

## Task 3: Negative ID generation

Add a decrementing counter for offline ID generation in `idb.cljs`. When offline and an entity has no `:id`, assign a negative ID. When online, use `memory/ensure-id` as before.

**Files:**
- Modify: `spec/cljs/c3kit/bucket/idb_spec.cljs`
- Modify: `src/cljs/c3kit/bucket/idb.cljs`

- [ ] **Step 1: Write tests for negative ID generation**

In `idb_spec.cljs`:

```clojure
(ns c3kit.bucket.idb-spec
  (:require-macros [speclj.core :refer [after before context describe it should should< should=]])
  (:require [c3kit.bucket.idb :as sut]
            [c3kit.bucket.memory :as memory]
            [speclj.core]))

(describe "IDB"

  (context "ensure-offline-id"

    (before (reset! sut/offline-id-counter 0))

    (it "assigns negative decrementing ID to entity without ID"
      (let [result (sut/ensure-offline-id {:kind :bibelot :name "widget"})]
        (should= -1 (:id result))))

    (it "decrements for each new entity"
      (sut/ensure-offline-id {:kind :bibelot :name "first"})
      (let [result (sut/ensure-offline-id {:kind :bibelot :name "second"})]
        (should= -2 (:id result))))

    (it "preserves existing ID"
      (let [result (sut/ensure-offline-id {:kind :bibelot :id 42 :name "existing"})]
        (should= 42 (:id result))))

    (it "preserves existing negative ID"
      (let [result (sut/ensure-offline-id {:kind :bibelot :id -5 :name "already-offline"})]
        (should= -5 (:id result)))))

  (context "offline-ensure-id"

    (before (reset! sut/offline-id-counter 0))

    (it "uses negative ID when offline"
      (let [result (sut/offline-ensure-id (constantly false) {:kind :bibelot :name "widget"})]
        (should< (:id result) 0)))

    (it "uses positive ID when online"
      (let [result (sut/offline-ensure-id (constantly true) {:kind :bibelot :name "widget"})]
        (should< 0 (:id result))))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `clj -M:test:cljs once`
Expected: FAIL — `ensure-offline-id`, `offline-id-counter`, and `offline-ensure-id` don't exist.

- [ ] **Step 3: Implement negative ID generation in `idb.cljs`**

Add to `idb.cljs` after the ns declaration, before the Entity Operations region:

```clojure
;region Offline ID Generation

(def offline-id-counter (atom 0))

(defn ensure-offline-id [entity]
  (if (:id entity)
    entity
    (assoc entity :id (swap! offline-id-counter dec))))

(defn offline-ensure-id [online-fn entity]
  (if (online-fn)
    (memory/ensure-id entity)
    (ensure-offline-id entity)))

;endregion
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `clj -M:test:cljs once`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/cljs/c3kit/bucket/idb.cljs spec/cljs/c3kit/bucket/idb_spec.cljs
git commit -m "add negative ID generation for offline entities"
```

---

## Task 4: Dirty set operations

Add functions to read, add to, and remove from the dirty set in the `_meta` IDB object store. These are low-level primitives used by the transaction and sync logic in later tasks.

**Files:**
- Modify: `spec/cljs/c3kit/bucket/idb_spec.cljs`
- Modify: `src/cljs/c3kit/bucket/idb.cljs`

- [ ] **Step 1: Write tests for dirty set operations**

Add to `idb_spec.cljs`. These tests use real IDB so they return promises:

```clojure
  (context "dirty set"

    (it "reads empty dirty set from fresh db"
      (let [legend {:bibelot {:id {:type :long} :name {:type :string}}}]
        (-> (common/open "test-dirty-1" legend)
            (.then (fn [idb]
                     (-> (sut/read-dirty-set idb)
                         (.then (fn [dirty]
                                  (should= #{} dirty)
                                  (common/close idb)
                                  (.deleteDatabase js/indexedDB "test-dirty-1")))))))))

    (it "adds IDs to dirty set"
      (let [legend {:bibelot {:id {:type :long} :name {:type :string}}}]
        (-> (common/open "test-dirty-2" legend)
            (.then (fn [idb]
                     (-> (sut/add-to-dirty-set! idb #{-1 -2})
                         (.then (fn [_] (sut/read-dirty-set idb)))
                         (.then (fn [dirty]
                                  (should= #{-1 -2} dirty)
                                  (common/close idb)
                                  (.deleteDatabase js/indexedDB "test-dirty-2")))))))))

    (it "accumulates IDs across multiple adds"
      (let [legend {:bibelot {:id {:type :long} :name {:type :string}}}]
        (-> (common/open "test-dirty-3" legend)
            (.then (fn [idb]
                     (-> (sut/add-to-dirty-set! idb #{-1})
                         (.then (fn [_] (sut/add-to-dirty-set! idb #{-2 42})))
                         (.then (fn [_] (sut/read-dirty-set idb)))
                         (.then (fn [dirty]
                                  (should= #{-1 -2 42} dirty)
                                  (common/close idb)
                                  (.deleteDatabase js/indexedDB "test-dirty-3")))))))))

    (it "removes specific IDs from dirty set"
      (let [legend {:bibelot {:id {:type :long} :name {:type :string}}}]
        (-> (common/open "test-dirty-4" legend)
            (.then (fn [idb]
                     (-> (sut/add-to-dirty-set! idb #{-1 -2 42})
                         (.then (fn [_] (sut/remove-from-dirty-set! idb #{-2})))
                         (.then (fn [_] (sut/read-dirty-set idb)))
                         (.then (fn [dirty]
                                  (should= #{-1 42} dirty)
                                  (common/close idb)
                                  (.deleteDatabase js/indexedDB "test-dirty-4"))))))))))
```

Also add the `idb-common` require alias to the ns declaration:

```clojure
(ns c3kit.bucket.idb-spec
  (:require-macros [speclj.core :refer [after before context describe it should should< should=]])
  (:require [c3kit.bucket.idb :as sut]
            [c3kit.bucket.idb-common :as common]
            [c3kit.bucket.memory :as memory]
            [speclj.core]))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `clj -M:test:cljs once`
Expected: FAIL — `read-dirty-set`, `add-to-dirty-set!`, `remove-from-dirty-set!` don't exist.

- [ ] **Step 3: Implement dirty set operations in `idb.cljs`**

Add a new region to `idb.cljs`:

```clojure
;region Dirty Set Operations

(defn read-dirty-set [idb]
  (-> (common/read-store idb "_meta")
      (.then (fn [entries]
               (let [dirty-entry (first (filter #(= "dirty" (:id %)) entries))]
                 (or (:data dirty-entry) #{}))))))

(defn- write-dirty-set! [idb dirty-set]
  (js/Promise.
   (fn [resolve reject]
     (let [tx      (.transaction idb #js ["_meta"] "readwrite")
           store   (.objectStore tx "_meta")
           request (.put store (common/clj->js-entity {:id "dirty" :data dirty-set}))]
       (set! (.-onsuccess request) (fn [_] (resolve dirty-set)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))

(defn add-to-dirty-set! [idb ids]
  (-> (read-dirty-set idb)
      (.then (fn [current] (write-dirty-set! idb (into current ids))))))

(defn remove-from-dirty-set! [idb ids]
  (-> (read-dirty-set idb)
      (.then (fn [current] (write-dirty-set! idb (reduce disj current ids))))))

;endregion
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `clj -M:test:cljs once`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/cljs/c3kit/bucket/idb.cljs spec/cljs/c3kit/bucket/idb_spec.cljs
git commit -m "add dirty set operations for _meta IDB store"
```

---

## Task 5: Offline-aware `idb-tx`

Modify `idb-tx` to check `online-fn` and apply offline behavior: negative IDs for new entities, dirty set tracking for creates and updates, tombstones for deletes of server-known entities, and cleanup for deletes of offline-created entities.

**Files:**
- Modify: `spec/cljs/c3kit/bucket/indexeddb_spec.cljs`
- Modify: `src/cljs/c3kit/bucket/idb.cljs`

- [ ] **Step 1: Write test for offline create (negative ID + dirty)**

Add to `indexeddb_spec.cljs`, a new context after `"rollback on IDB failure"`:

```clojure
  (context "offline tx"

    (before (reset! idb/offline-id-counter 0))

    (it "assigns negative ID and marks dirty on offline create"
      (let [online? (atom false)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-offline-1" :online? #(deref online?)} [bibelot])]
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

    (it "uses positive ID and no dirty tracking when online"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-offline-2"} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "online-widget"})]
                       (should< 0 (:id saved))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= #{} dirty)
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-2"))))))
```

Add `[c3kit.bucket.idb :as idb]` to the ns `:require` of `indexeddb_spec.cljs` if not already present (it is already there).

- [ ] **Step 2: Run tests to verify they fail**

Run: `clj -M:test:cljs once`
Expected: FAIL — offline create still assigns positive ID.

- [ ] **Step 3: Write test for offline update (positive ID + dirty)**

Add to the `"offline tx"` context in `indexeddb_spec.cljs`:

```clojure
    (it "marks existing entity dirty on offline update"
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
```

- [ ] **Step 4: Write test for offline delete of server-known entity (tombstone + dirty)**

```clojure
    (it "creates tombstone on offline delete of server-known entity"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-offline-4" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "widget" :size 5})]
                       (reset! online? false)
                       (api/-tx db (assoc saved :db/delete? true))
                       ;; entity removed from memory
                       (should= 0 (count (api/find-by- db :bibelot :name "widget")))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= 1 (count dirty))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-4"))))))
```

- [ ] **Step 5: Write test for offline delete of offline-created entity (cleanup, not dirty)**

```clojure
    (it "cleans up on offline delete of offline-created entity"
      (let [online? (atom false)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-offline-5" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "temp-widget"})]
                       (should= -1 (:id saved))
                       (api/-tx db (assoc saved :db/delete? true))
                       ;; entity removed from memory
                       (should= 0 (count (api/find-by- db :bibelot :name "temp-widget")))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= #{} dirty)
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-5")))))))
```

- [ ] **Step 6: Run tests to verify they all fail**

Run: `clj -M:test:cljs once`
Expected: Multiple failures — current `idb-tx` doesn't check `online-fn`.

- [ ] **Step 7: Implement offline-aware `idb-tx`**

Replace `prepare-entity` and `idb-tx` in `idb.cljs`:

```clojure
(defn- prepare-entity [db entity]
  (let [online-fn (.-online-fn db)
        entity    (offline-ensure-id online-fn entity)
        new-store (memory/tx-entity @(.-legend db) @(.-store db) entity)
        result    (if (api/delete? entity)
                    (api/soft-delete entity)
                    (get-in new-store [:all (:id entity)]))]
    [new-store result]))

(defn- offline? [db] (not ((.-online-fn db))))

(defn- persist-online! [idb entity result old-store store-atom]
  (-> (if (api/delete? entity)
        (delete-entity idb (:kind entity) (:id entity))
        (put-entity idb result))
      (.catch (fn [_] (reset! store-atom old-store)))))

(defn- persist-offline-delete! [idb entity old-store store-atom]
  (if (neg? (:id entity))
    (-> (js/Promise.all
          #js [(delete-entity idb (:kind entity) (:id entity))
               (remove-from-dirty-set! idb #{(:id entity)})])
        (.catch (fn [_] (reset! store-atom old-store))))
    (-> (js/Promise.all
          #js [(put-entity idb (api/soft-delete entity))
               (add-to-dirty-set! idb #{(:id entity)})])
        (.catch (fn [_] (reset! store-atom old-store))))))

(defn- persist-offline-save! [idb result old-store store-atom]
  (-> (js/Promise.all
        #js [(put-entity idb result)
             (add-to-dirty-set! idb #{(:id result)})])
      (.catch (fn [_] (reset! store-atom old-store)))))

(defn idb-tx [db entity]
  (let [old-store @(.-store db)
        [new-store result] (prepare-entity db entity)]
    (reset! (.-store db) new-store)
    (when @(.-idb-atom db)
      (let [idb @(.-idb-atom db)]
        (if (offline? db)
          (if (api/delete? entity)
            (persist-offline-delete! idb entity old-store (.-store db))
            (persist-offline-save! idb result old-store (.-store db)))
          (persist-online! idb entity result old-store (.-store db)))))
    result))
```

- [ ] **Step 8: Run tests to verify they pass**

Run: `clj -M:test:cljs once`
Expected: All new offline tx tests pass, all existing tests still pass.

- [ ] **Step 9: Commit**

```bash
git add src/cljs/c3kit/bucket/idb.cljs spec/cljs/c3kit/bucket/indexeddb_spec.cljs
git commit -m "implement offline-aware idb-tx with dirty tracking and tombstones"
```

---

## Task 6: Offline-aware `idb-tx*`

Apply the same offline logic to batch transactions.

**Files:**
- Modify: `spec/cljs/c3kit/bucket/indexeddb_spec.cljs`
- Modify: `src/cljs/c3kit/bucket/idb.cljs`

- [ ] **Step 1: Write test for offline batch create**

Add to the `"offline tx"` context in `indexeddb_spec.cljs`:

```clojure
    (it "assigns negative IDs and marks dirty on offline batch create"
      (let [online? (atom false)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-offline-batch-1" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx* db [{:kind :bibelot :name "w1"} {:kind :bibelot :name "w2"}])]
                       (should= -1 (:id (first saved)))
                       (should= -2 (:id (second saved)))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= #{-1 -2} dirty)
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-batch-1"))))))
```

- [ ] **Step 2: Write test for offline batch with mixed create and delete**

```clojure
    (it "handles mixed creates and deletes in offline batch"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-offline-batch-2" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "existing" :size 5})]
                       (reset! online? false)
                       (api/-tx* db [{:kind :bibelot :name "new-offline"}
                                     (assoc saved :db/delete? true)])
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= 2 (count dirty))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-batch-2"))))))
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `clj -M:test:cljs once`
Expected: FAIL — `idb-tx*` still uses `memory/ensure-id` and doesn't track dirty.

- [ ] **Step 4: Implement offline-aware `idb-tx*`**

Replace `idb-tx*` in `idb.cljs`:

```clojure
(defn idb-tx* [db entities]
  (let [old-store  @(.-store db)
        online-fn  (.-online-fn db)
        entities   (map (partial offline-ensure-id online-fn) entities)
        new-store  (core-reduce #(memory/tx-entity @(.-legend db) %1 %2) @(.-store db) entities)
        results    (mapv (fn [e]
                           (if (api/delete? e)
                             (api/soft-delete e)
                             (get-in new-store [:all (:id e)])))
                         entities)
        to-persist (remove api/delete? results)
        to-delete  (filter api/delete? entities)]
    (reset! (.-store db) new-store)
    (when @(.-idb-atom db)
      (let [idb @(.-idb-atom db)]
        (if (offline? db)
          (let [offline-deletes-neg (filter #(neg? (:id %)) to-delete)
                offline-deletes-pos (remove #(neg? (:id %)) to-delete)
                tombstones          (map api/soft-delete offline-deletes-pos)
                all-to-persist      (concat to-persist tombstones)
                dirty-ids           (into (set (map :id all-to-persist))
                                          (map :id offline-deletes-pos))
                clean-ids           (set (map :id offline-deletes-neg))]
            (-> (js/Promise.all
                  (clj->js (cond-> []
                             (seq all-to-persist) (conj (put-entities idb all-to-persist))
                             (seq offline-deletes-neg) (into (map #(delete-entity idb (:kind %) (:id %)) offline-deletes-neg))
                             (seq dirty-ids) (conj (add-to-dirty-set! idb dirty-ids))
                             (seq clean-ids) (conj (remove-from-dirty-set! idb clean-ids)))))
                (.catch (fn [_] (reset! (.-store db) old-store)))))
          (-> (js/Promise.all
                (clj->js (cond-> []
                           (seq to-persist) (conj (put-entities idb to-persist))
                           (seq to-delete)  (into (map #(delete-entity idb (:kind %) (:id %)) to-delete)))))
              (.catch (fn [_] (reset! (.-store db) old-store)))))))
    results))
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `clj -M:test:cljs once`
Expected: All tests pass.

- [ ] **Step 6: Commit**

```bash
git add src/cljs/c3kit/bucket/idb.cljs spec/cljs/c3kit/bucket/indexeddb_spec.cljs
git commit -m "implement offline-aware idb-tx* with dirty tracking"
```

---

## Task 7: `sync!` and `sync-complete!`

Add the sync lifecycle functions: `sync!` reads dirty entities and passes them to a callback. `sync-complete!` clears dirty state and replaces temp entities with server data.

**Files:**
- Modify: `spec/cljs/c3kit/bucket/indexeddb_spec.cljs`
- Modify: `src/cljs/c3kit/bucket/idb.cljs`

- [ ] **Step 1: Write test for `sync!` reading dirty entities**

Add a new context in `indexeddb_spec.cljs`:

```clojure
  (context "sync lifecycle"

    (before (reset! idb/offline-id-counter 0))

    (it "sync! provides dirty entities to callback"
      (let [online?  (atom false)
            db       (api/create-db {:impl :indexeddb :db-name "test-idb-sync-1" :online? #(deref online?)} [bibelot])
            received (atom nil)]
        (-> (idb/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :name "offline-1"})
                     (api/-tx db {:kind :bibelot :name "offline-2"})
                     (idb/sync! db (fn [entities] (reset! received entities)))))
            (.then (fn [_]
                     (should= 2 (count @received))
                     (should= #{-1 -2} (set (map :id @received)))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-sync-1"))))))

    (it "sync! includes tombstones for offline-deleted server entities"
      (let [online?  (atom true)
            db       (api/create-db {:impl :indexeddb :db-name "test-idb-sync-2" :online? #(deref online?)} [bibelot])
            received (atom nil)]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "server-widget" :size 5})]
                       (reset! online? false)
                       (api/-tx db (assoc saved :db/delete? true))
                       (idb/sync! db (fn [entities] (reset! received entities))))))
            (.then (fn [_]
                     (should= 1 (count @received))
                     (should= true (:db/delete? (first @received)))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-sync-2"))))))

    (it "sync! returns empty vector when nothing is dirty"
      (let [db       (api/create-db {:impl :indexeddb :db-name "test-idb-sync-3"} [bibelot])
            received (atom nil)]
        (-> (idb/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :name "clean-widget"})
                     (idb/sync! db (fn [entities] (reset! received entities)))))
            (.then (fn [_]
                     (should= 0 (count @received))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-sync-3")))))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `clj -M:test:cljs once`
Expected: FAIL — `sync!` does not exist.

- [ ] **Step 3: Implement `sync!`**

Add to `idb.cljs`:

```clojure
;region Sync Lifecycle

(defn sync! [db callback]
  (let [idb @(.-idb-atom db)]
    (if-not idb
      (do (callback []) (js/Promise.resolve nil))
      (-> (read-dirty-set idb)
          (.then (fn [dirty-ids]
                   (if (empty? dirty-ids)
                     (do (callback []) nil)
                     (let [kind-stores (->> (array-seq (.-objectStoreNames idb))
                                            (remove #(= "_meta" %)))]
                       (-> (js/Promise.all (clj->js (map #(common/read-store idb %) kind-stores)))
                           (.then (fn [results]
                                    (let [all-entities (mapcat identity (array-seq results))
                                          dirty       (filter #(contains? dirty-ids (:id %)) all-entities)]
                                      (callback (vec dirty))))))))))))))

;endregion
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `clj -M:test:cljs once`
Expected: PASS

- [ ] **Step 5: Write test for `sync-complete!`**

Add to the `"sync lifecycle"` context:

```clojure
    (it "sync-complete! clears dirty state and replaces temp entities with server data"
      (let [online?  (atom false)
            db       (api/create-db {:impl :indexeddb :db-name "test-idb-sync-4" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :name "offline-widget" :size 5})
                     (should= -1 (:id (first (api/find-by- db :bibelot :name "offline-widget"))))
                     (let [server-entity {:kind :bibelot :id 9001 :name "offline-widget" :size 5}]
                       (idb/sync-complete! db #{-1} [server-entity]))))
            (.then (fn [_]
                     ;; negative ID entity purged, server entity present
                     (should= 0 (count (filter #(neg? (:id %)) (api/find- db :bibelot))))
                     (should= 1 (count (api/find-by- db :bibelot :name "offline-widget")))
                     (should= 9001 (:id (first (api/find-by- db :bibelot :name "offline-widget"))))
                     ;; dirty set cleared
                     (idb/read-dirty-set @(.-idb-atom db))))
            (.then (fn [dirty]
                     (should= #{} dirty)
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-sync-4"))))))

    (it "sync-complete! clears tombstones from IDB"
      (let [online?  (atom true)
            db       (api/create-db {:impl :indexeddb :db-name "test-idb-sync-5" :online? #(deref online?)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "to-delete" :size 5})]
                       (reset! online? false)
                       (api/-tx db (assoc saved :db/delete? true))
                       (idb/sync-complete! db #{(:id saved)} []))))
            (.then (fn [_]
                     (idb/read-dirty-set @(.-idb-atom db))))
            (.then (fn [dirty]
                     (should= #{} dirty)
                     ;; rehydrate to confirm tombstone is gone from IDB
                     (reset! (.-store db) {:all {}})
                     (idb/rehydrate! db)))
            (.then (fn [db]
                     (should= 0 (count (api/find- db :bibelot)))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-sync-5"))))))
```

- [ ] **Step 6: Run tests to verify they fail**

Run: `clj -M:test:cljs once`
Expected: FAIL — `sync-complete!` does not exist.

- [ ] **Step 7: Implement `sync-complete!`**

Add to the `"Sync Lifecycle"` region in `idb.cljs`:

```clojure
(defn sync-complete! [db dirty-ids server-entities]
  (let [idb       @(.-idb-atom db)
        neg-ids   (filter neg? dirty-ids)
        all-ids   dirty-ids]
    ;; Purge negative-ID entities from in-memory store
    (doseq [id neg-ids]
      (when-let [entity (get-in @(.-store db) [:all id])]
        (swap! (.-store db) #(memory/tx-entity @(.-legend db) % (api/soft-delete (:kind entity) id)))))
    ;; tx* server entities into memory (bypassing idb-tx to avoid dirty tracking)
    (when (seq server-entities)
      (memory/tx* db server-entities))
    ;; Async: clear dirty IDs, remove dirty entities from IDB, persist server entities to IDB
    (if-not idb
      (js/Promise.resolve nil)
      (-> (js/Promise.all
            (clj->js (cond-> [(remove-from-dirty-set! idb all-ids)]
                       (seq neg-ids)       (into (map #(delete-entity idb (:kind (get-in @(.-store db) [:all %]) :unknown) %) neg-ids))
                       (seq dirty-ids)     (into (map (fn [id]
                                                        (if-let [entity (get-in @(.-store db) [:all id])]
                                                          (put-entity idb entity)
                                                          (delete-entity idb :unknown id)))
                                                      (filter pos? dirty-ids)))
                       (seq server-entities) (conj (put-entities idb server-entities)))))))))
```

Wait — this is getting complex and has issues with looking up already-deleted entities. Let me simplify. The dirty IDs to clean up fall into two categories: negative IDs (delete from IDB) and positive IDs that were tombstones (also delete from IDB). We just need to delete all dirty IDs from IDB and persist the server entities:

```clojure
(defn sync-complete! [db dirty-ids server-entities]
  (let [idb     @(.-idb-atom db)
        neg-ids (filter neg? dirty-ids)]
    ;; Purge negative-ID entities from in-memory store
    (doseq [id neg-ids]
      (when-let [entity (get-in @(.-store db) [:all id])]
        (swap! (.-store db) #(memory/tx-entity @(.-legend db) % (api/soft-delete (:kind entity) id)))))
    ;; tx* server entities into memory (bypassing idb-tx to avoid dirty tracking)
    (when (seq server-entities)
      (memory/tx* db server-entities))
    ;; Async IDB cleanup
    (if-not idb
      (js/Promise.resolve nil)
      (let [;; We need to know the kind for each dirty ID to delete from the right object store.
            ;; For negative IDs still in memory: already purged, but we stored them in IDB under their kind.
            ;; Simplest: read dirty entities from IDB, then delete them, then persist server entities.
            ]
        (-> (read-dirty-set idb)
            (.then (fn [current-dirty]
                     (let [ids-to-clear (filter (fn [id] (contains? current-dirty id)) dirty-ids)]
                       ;; Read all stores to find entities to delete by ID
                       (let [kind-stores (->> (array-seq (.-objectStoreNames idb))
                                              (remove #(= "_meta" %)))]
                         (-> (js/Promise.all (clj->js (map #(common/read-store idb %) kind-stores)))
                             (.then (fn [results]
                                      (let [all-idb     (mapcat identity (array-seq results))
                                            to-delete   (filter #(contains? (set ids-to-clear) (:id %)) all-idb)]
                                        (js/Promise.all
                                          (clj->js (concat
                                                     [(remove-from-dirty-set! idb (set dirty-ids))]
                                                     (map #(delete-entity idb (:kind %) (:id %)) to-delete)
                                                     (when (seq server-entities)
                                                       [(put-entities idb server-entities)]))))))))))))))))
```

Hmm, this is still complex. Let me simplify further — we know the kinds from the entities we synced (the callback received them). The caller passes dirty-ids as a set. We can just iterate all stores and delete matching IDs:

```clojure
(defn sync-complete! [db dirty-ids server-entities]
  (let [idb       @(.-idb-atom db)
        dirty-set (set dirty-ids)
        neg-ids   (filter neg? dirty-ids)]
    ;; 1. Purge negative-ID entities from in-memory store
    (doseq [id neg-ids]
      (when-let [entity (get-in @(.-store db) [:all id])]
        (swap! (.-store db) #(memory/tx-entity @(.-legend db) % (api/soft-delete (:kind entity) id)))))
    ;; 2. Load server entities into memory (bypass idb-tx to avoid dirty tracking)
    (when (seq server-entities)
      (memory/tx* db server-entities))
    ;; 3. Async: clean up IDB
    (if-not idb
      (js/Promise.resolve nil)
      (let [kind-stores (->> (array-seq (.-objectStoreNames idb))
                             (remove #(= "_meta" %)))]
        (-> (js/Promise.all (clj->js (map #(common/read-store idb %) kind-stores)))
            (.then (fn [results]
                     (let [all-idb   (mapcat identity (array-seq results))
                           to-delete (filter #(contains? dirty-set (:id %)) all-idb)]
                       (js/Promise.all
                         (clj->js (cond-> [(remove-from-dirty-set! idb dirty-set)]
                                    (seq to-delete)      (into (map #(delete-entity idb (:kind %) (:id %)) to-delete))
                                    (seq server-entities) (conj (put-entities idb server-entities)))))))))))))
```

- [ ] **Step 8: Run tests to verify they pass**

Run: `clj -M:test:cljs once`
Expected: All tests pass.

- [ ] **Step 9: Commit**

```bash
git add src/cljs/c3kit/bucket/idb.cljs spec/cljs/c3kit/bucket/indexeddb_spec.cljs
git commit -m "implement sync! and sync-complete! for offline sync lifecycle"
```

---

## Task 8: `idb-reader` for service workers

Create the lightweight SW-compatible reader that depends only on `idb-common`.

**Files:**
- Create: `src/cljs/c3kit/bucket/idb_reader.cljs`
- Create: `spec/cljs/c3kit/bucket/idb_reader_spec.cljs`

- [ ] **Step 1: Write tests for `idb-reader`**

```clojure
(ns c3kit.bucket.idb-reader-spec
  (:require-macros [speclj.core :refer [before context describe it should should=]])
  (:require [c3kit.bucket.idb-common :as common]
            [c3kit.bucket.idb-reader :as sut]
            [speclj.core]))

(def legend {:bibelot {:id {:type :long} :name {:type :string} :size {:type :long}}})

(describe "IDB Reader"

  (context "dirty-entities"

    (it "reads dirty entities from IDB"
      (-> (common/open "test-reader-1" legend)
          (.then (fn [idb]
                   ;; Manually seed IDB with a dirty entity and dirty set
                   (let [tx    (.transaction idb #js ["bibelot" "_meta"] "readwrite")
                         store (.objectStore tx "bibelot")
                         meta  (.objectStore tx "_meta")]
                     (.put store (common/clj->js-entity {:id -1 :kind :bibelot :name "offline" :size 5}))
                     (.put meta (common/clj->js-entity {:id "dirty" :data #{-1}}))
                     (js/Promise.
                      (fn [resolve _]
                        (set! (.-oncomplete tx) #(resolve idb)))))))
          (.then (fn [idb]
                   (-> (sut/dirty-entities idb)
                       (.then (fn [entities]
                                (should= 1 (count entities))
                                (should= -1 (:id (first entities)))
                                (should= "offline" (:name (first entities)))
                                (common/close idb)
                                (.deleteDatabase js/indexedDB "test-reader-1"))))))))))

  (context "clear-dirty!"

    (it "removes dirty IDs and their entities from IDB"
      (-> (common/open "test-reader-2" legend)
          (.then (fn [idb]
                   (let [tx    (.transaction idb #js ["bibelot" "_meta"] "readwrite")
                         store (.objectStore tx "bibelot")
                         meta  (.objectStore tx "_meta")]
                     (.put store (common/clj->js-entity {:id -1 :kind :bibelot :name "w1"}))
                     (.put store (common/clj->js-entity {:id -2 :kind :bibelot :name "w2"}))
                     (.put meta (common/clj->js-entity {:id "dirty" :data #{-1 -2}}))
                     (js/Promise.
                      (fn [resolve _]
                        (set! (.-oncomplete tx) #(resolve idb)))))))
          (.then (fn [idb]
                   (-> (sut/clear-dirty! idb #{-1})
                       (.then (fn [_]
                                ;; Read remaining dirty set
                                (common/read-store idb "_meta")))
                       (.then (fn [meta-entries]
                                (let [dirty-entry (first (filter #(= "dirty" (:id %)) meta-entries))]
                                  (should= #{-2} (:data dirty-entry)))
                                ;; Verify entity -1 is gone, -2 remains
                                (common/read-store idb "bibelot")))
                       (.then (fn [entities]
                                (should= 1 (count entities))
                                (should= -2 (:id (first entities)))
                                (common/close idb)
                                (.deleteDatabase js/indexedDB "test-reader-2")))))))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `clj -M:test:cljs once`
Expected: FAIL — `idb-reader` namespace does not exist.

- [ ] **Step 3: Implement `idb-reader`**

```clojure
(ns c3kit.bucket.idb-reader
  (:require [c3kit.bucket.idb-common :as common]))

(defn dirty-entities
  "Reads dirty entities from IDB. Returns a promise resolving to a vector.
   Only depends on idb-common — safe for service worker builds."
  [idb]
  (-> (common/read-store idb "_meta")
      (.then (fn [entries]
               (let [dirty-entry (first (filter #(= "dirty" (:id %)) entries))
                     dirty-ids   (or (:data dirty-entry) #{})]
                 (if (empty? dirty-ids)
                   (js/Promise.resolve [])
                   (let [kind-stores (->> (array-seq (.-objectStoreNames idb))
                                          (remove #(= "_meta" %)))]
                     (-> (js/Promise.all (clj->js (map #(common/read-store idb %) kind-stores)))
                         (.then (fn [results]
                                  (let [all-entities (mapcat identity (array-seq results))]
                                    (vec (filter #(contains? dirty-ids (:id %)) all-entities)))))))))))))

(defn clear-dirty!
  "Removes specified IDs from the dirty set and deletes their entities from IDB.
   Returns a promise. Only depends on idb-common — safe for service worker builds."
  [idb ids-to-clear]
  (let [id-set (set ids-to-clear)]
    (-> (common/read-store idb "_meta")
        (.then (fn [entries]
                 (let [dirty-entry (first (filter #(= "dirty" (:id %)) entries))
                       current     (or (:data dirty-entry) #{})
                       updated     (reduce disj current id-set)]
                   ;; First: update dirty set in _meta
                   (js/Promise.
                    (fn [resolve reject]
                      (let [tx      (.transaction idb #js ["_meta"] "readwrite")
                            store   (.objectStore tx "_meta")
                            request (.put store (common/clj->js-entity {:id "dirty" :data updated}))]
                        (set! (.-onsuccess request) (fn [_] (resolve nil)))
                        (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))))
        (.then (fn [_]
                 ;; Then: delete entities from their kind stores
                 (let [kind-stores (->> (array-seq (.-objectStoreNames idb))
                                        (remove #(= "_meta" %)))]
                   (-> (js/Promise.all (clj->js (map #(common/read-store idb %) kind-stores)))
                       (.then (fn [results]
                                (let [all-entities (mapcat identity (array-seq results))
                                      to-delete   (filter #(contains? id-set (:id %)) all-entities)]
                                  (js/Promise.all
                                    (clj->js (map (fn [entity]
                                                    (js/Promise.
                                                     (fn [resolve reject]
                                                       (let [store-name (name (:kind entity))
                                                             tx         (.transaction idb #js [store-name] "readwrite")
                                                             store      (.objectStore tx store-name)
                                                             request    (.delete store (:id entity))]
                                                         (set! (.-onsuccess request) (fn [_] (resolve nil)))
                                                         (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))
                                                  to-delete))))))))))))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `clj -M:test:cljs once`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/cljs/c3kit/bucket/idb_reader.cljs spec/cljs/c3kit/bucket/idb_reader_spec.cljs
git commit -m "add idb-reader for service worker compatible dirty entity access"
```

---

## Task 9: ReIndexedDB offline sync tests

Verify that the offline sync features work identically with the `:re-indexeddb` impl (reagent atoms).

**Files:**
- Modify: `spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs`

- [ ] **Step 1: Add offline tx tests to `re_indexeddb_spec.cljs`**

Add after the `"rollback on IDB failure"` context:

```clojure
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
                     (should= 0 (count (filter #(neg? (:id %)) (api/find- db :bibelot))))
                     (should= 9001 (:id (first (api/find-by- db :bibelot :name "offline-widget"))))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-reidb-offline-2")))))))
```

- [ ] **Step 2: Run tests to verify they pass**

Run: `clj -M:test:cljs once`
Expected: PASS — the offline logic is in `idb.cljs` shared by both impls, so these should pass without code changes.

- [ ] **Step 3: Commit**

```bash
git add spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs
git commit -m "add offline sync tests for re-indexeddb impl"
```

---

## Task 10: Update design doc with final implementation details

Update the spec to reflect any implementation decisions that diverged from the original design.

**Files:**
- Modify: `docs/superpowers/specs/2026-03-25-offline-sync-design.md`

- [ ] **Step 1: Review and update the design doc**

Compare the implementation against the spec. Update function signatures, namespace names, and any behavioral details that changed during implementation.

- [ ] **Step 2: Commit**

```bash
git add docs/superpowers/specs/2026-03-25-offline-sync-design.md
git commit -m "update offline sync design doc with final implementation details"
```
