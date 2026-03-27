# IDB Code Review Fixes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Address code review findings: eliminate DRY violations, fix dirty set race condition, optimize performance with targeted reads, add error logging, and clean up idb_reader delegation.

**Architecture:** The dirty set format changes from `#{id ...}` (bare ID set) to `{id kind, ...}` (map of ID to kind). This enables targeted IDB reads by kind+ID instead of reading all entities and filtering. A promise chain serializes dirty set mutations to prevent race conditions. `idb_reader` is updated to use shared operations instead of reimplementing them.

**Tech Stack:** ClojureScript, IndexedDB, speclj

**Run commands:**
- ClojureScript test autorunner: `clj -M:test:cljs`
- One-time run: `clj -M:test:cljs once`

---

### Task 1: Fix idb_io_spec describe name

**Files:**
- Modify: `spec/cljs/c3kit/bucket/idb_io_spec.cljs:6`

- [ ] **Step 1: Fix the describe name**

Change "IDB Common" to "IDB IO" — this is a leftover from before the refactoring split.

```clojure
;; Before
(describe "IDB Common"

;; After
(describe "IDB IO"
```

- [ ] **Step 2: Run tests to verify**

Run: `clj -M:test:cljs once`
Expected: All idb_io_spec tests pass

- [ ] **Step 3: Commit**

```bash
git add spec/cljs/c3kit/bucket/idb_io_spec.cljs
git commit -m "fix idb_io_spec describe name: 'IDB Common' -> 'IDB IO'"
```

---

### Task 2: Add error logging in with-rollback

**Files:**
- Modify: `src/cljs/c3kit/bucket/idb_common.cljs:1-5,25-26`

The `with-rollback` function silently swallows IDB errors. Add a warning log so failures aren't invisible.

- [ ] **Step 1: Add log require and update with-rollback**

In `idb_common.cljs`, add `c3kit.apron.log` to the require:

```clojure
(ns c3kit.bucket.idb-common
  (:refer-clojure :rename {reduce core-reduce})
  (:require [c3kit.apron.log :as log]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-io :as io]
            [c3kit.bucket.memory :as memory]))
```

Update `with-rollback`:

```clojure
(defn- with-rollback [promise store-atom old-store]
  (.catch promise (fn [e]
                    (log/warn "IDB write failed, rolling back in-memory store:" e)
                    (reset! store-atom old-store))))
```

- [ ] **Step 2: Run tests to verify**

Run: `clj -M:test:cljs once`
Expected: All tests pass. Rollback tests in indexeddb_spec and re_indexeddb_spec still work (they force IDB failures and verify rollback).

- [ ] **Step 3: Commit**

```bash
git add src/cljs/c3kit/bucket/idb_common.cljs
git commit -m "add warning log to with-rollback on IDB write failure"
```

---

### Task 3: Remove duplicate close from idb_common

**Files:**
- Modify: `src/cljs/c3kit/bucket/idb_common.cljs:267-268` (remove `close`)
- Modify: `src/cljs/c3kit/bucket/indexeddb.cljs:6,16` (add io require, use `io/close`)
- Modify: `src/cljs/c3kit/bucket/re_indexeddb.cljs:8,18` (add io require, use `io/close`)

`idb_common/close` is identical to `io/close`. Remove it and have the deftypes call `io/close` directly.

- [ ] **Step 1: Run existing tests to confirm green baseline**

Run: `clj -M:test:cljs once`
Expected: All tests pass

- [ ] **Step 2: Update indexeddb.cljs**

Add `idb-io` require and change `close` method:

```clojure
(ns c3kit.bucket.indexeddb
  (:refer-clojure :rename {find core-find count core-count reduce core-reduce})
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as idb]
            [c3kit.bucket.idb-io :as io]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as migrator]))
```

In the deftype, change:
```clojure
;; Before
(close [_this] (idb/close @idb-atom))

;; After
(close [_this] (io/close @idb-atom))
```

- [ ] **Step 3: Update re_indexeddb.cljs**

Add `idb-io` require and change `close` method:

```clojure
(ns c3kit.bucket.re-indexeddb
  (:refer-clojure :rename {find core-find count core-count reduce core-reduce})
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as idb]
            [c3kit.bucket.idb-io :as io]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as migrator]
            [c3kit.bucket.re-memory :as re-memory]
            [reagent.core :as r]))
```

In the deftype, change:
```clojure
;; Before
(close [_this] (idb/close @idb-atom))

;; After
(close [_this] (io/close @idb-atom))
```

- [ ] **Step 4: Remove close from idb_common.cljs**

Remove lines 267-268:
```clojure
;; Remove entirely:
(defn close [idb]
  (when idb (.close idb)))
```

- [ ] **Step 5: Run tests to verify**

Run: `clj -M:test:cljs once`
Expected: All tests pass

- [ ] **Step 6: Commit**

```bash
git add src/cljs/c3kit/bucket/idb_common.cljs src/cljs/c3kit/bucket/indexeddb.cljs src/cljs/c3kit/bucket/re_indexeddb.cljs
git commit -m "remove duplicate close from idb_common; deftypes use io/close directly"
```

---

### Task 4: Add read-entity to idb_io

**Files:**
- Modify: `src/cljs/c3kit/bucket/idb_io.cljs` (add `read-entity` function)
- Modify: `spec/cljs/c3kit/bucket/idb_io_spec.cljs` (add test)

Add a function to read a single entity by ID from a named store. This is needed for targeted reads (instead of reading all entities and filtering).

- [ ] **Step 1: Write the failing test**

In `idb_io_spec.cljs`, add a new context. This requires adding `before` and async test support. Add to requires:

```clojure
(ns c3kit.bucket.idb-io-spec
  (:require-macros [speclj.core :refer [before context describe it should should= should-not=]])
  (:require [c3kit.bucket.idb-io :as sut]
            [speclj.core]))
```

Add a new context inside the describe block, after the serialization context:

```clojure
  (context "read-entity"

    (it "reads a single entity by ID from a store"
      (let [legend {:bibelot {:id {:type :long} :name {:type :string}}}]
        (-> (sut/open "test-read-entity-1" legend)
            (.then (fn [idb]
                     (let [tx    (.transaction idb #js ["bibelot"] "readwrite")
                           store (.objectStore tx "bibelot")]
                       (.put store (sut/clj->js-entity {:id 1 :kind :bibelot :name "widget"}))
                       (sut/request->promise (.put store (sut/clj->js-entity {:id 2 :kind :bibelot :name "gadget"}))
                                             (constantly idb)))))
            (.then (fn [idb]
                     (-> (sut/read-entity idb "bibelot" 1)
                         (.then (fn [entity]
                                  (should= {:id 1 :kind :bibelot :name "widget"} entity)
                                  (sut/close idb)
                                  (.deleteDatabase js/indexedDB "test-read-entity-1"))))))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — `read-entity` is not defined

- [ ] **Step 3: Write the implementation**

In `idb_io.cljs`, add to the "Read Operations" region (after `read-store`):

```clojure
(defn read-entity
  "Reads a single entity by ID from a specific store. Returns a promise of the entity or nil."
  [idb store-name id]
  (let [tx      (.transaction idb #js [store-name] "readonly")
        store   (.objectStore tx store-name)
        request (.get store id)]
    (request->promise request js->clj-entity)))
```

- [ ] **Step 4: Run tests to verify pass**

Run: `clj -M:test:cljs once`
Expected: All tests pass

- [ ] **Step 5: Write test for nil case**

```clojure
    (it "returns nil for non-existent entity"
      (let [legend {:bibelot {:id {:type :long} :name {:type :string}}}]
        (-> (sut/open "test-read-entity-2" legend)
            (.then (fn [idb]
                     (-> (sut/read-entity idb "bibelot" 999)
                         (.then (fn [entity]
                                  (should= nil entity)
                                  (sut/close idb)
                                  (.deleteDatabase js/indexedDB "test-read-entity-2"))))))))))
```

- [ ] **Step 6: Run tests to verify pass**

Run: `clj -M:test:cljs once`
Expected: All tests pass (the `js->clj-entity` already handles nil)

- [ ] **Step 7: Commit**

```bash
git add src/cljs/c3kit/bucket/idb_io.cljs spec/cljs/c3kit/bucket/idb_io_spec.cljs
git commit -m "add io/read-entity for targeted single-entity reads by ID"
```

---

### Task 5: Change dirty set format and fix race condition

**Files:**
- Modify: `src/cljs/c3kit/bucket/idb_io.cljs:122-132` (read-dirty-set returns `{}`, handle old format)
- Modify: `src/cljs/c3kit/bucket/idb_common.cljs:72-82` (format change + promise chain serialization)
- Modify: `spec/cljs/c3kit/bucket/idb_common_spec.cljs` (update dirty set assertions)

The dirty set format changes from `#{id ...}` to `{id kind ...}` (a map of ID to kind). This enables targeted reads in later tasks. Simultaneously, dirty set mutations are serialized through a promise chain to prevent the read-modify-write race condition.

#### Part A: Update io/read-dirty-set return format

- [ ] **Step 1: Update idb_common_spec "read-dirty-set returns empty set" test**

```clojure
    (it "read-dirty-set returns empty map from fresh db"
      (-> (io/open "test-dirty-1" legend)
          (.then (fn [idb]
                   (-> (sut/read-dirty-set idb)
                       (.then (fn [result]
                                (should= {} result)
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-1"))))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — expected `{}` but got `#{}`

- [ ] **Step 3: Update io/read-dirty-set**

In `idb_io.cljs`, update `read-dirty-set` to return a map and handle old set format gracefully:

```clojure
(defn read-dirty-set [idb]
  (let [tx      (.transaction idb #js ["_meta"] "readonly")
        store   (.objectStore tx "_meta")
        request (.get store "dirty")]
    (request->promise request
                      (fn [result]
                        (let [data (when result (:data (js->clj-entity result)))]
                          (if (or (nil? data) (set? data)) {} data))))))
```

Note: Old databases storing the set format (`#{1 2 3}`) will read as empty `{}`. This is a conscious migration — the old format lacks kind information required for targeted reads. Any unsynced offline data in old format will need to be re-created.

- [ ] **Step 4: Run test to verify pass**

Run: `clj -M:test:cljs once`
Expected: The "empty map" test passes. Other dirty set tests will fail because `add-to-dirty-set!` still writes old format.

#### Part B: Update add-to-dirty-set! format and serialization

- [ ] **Step 5: Update "add-to-dirty-set! adds IDs" test**

```clojure
    (it "add-to-dirty-set! adds entries to the map"
      (-> (io/open "test-dirty-2" legend)
          (.then (fn [idb]
                   (-> (sut/add-to-dirty-set! idb {1 :bibelot 2 :bibelot 3 :bibelot})
                       (.then (fn [_] (sut/read-dirty-set idb)))
                       (.then (fn [result]
                                (should= {1 :bibelot 2 :bibelot 3 :bibelot} result)
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-2"))))))))
```

- [ ] **Step 6: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL

- [ ] **Step 7: Add dirty-chain atom and update add-to-dirty-set!**

In `idb_common.cljs`, replace the dirty set operations region:

```clojure
;region Dirty Set Operations

(def read-dirty-set io/read-dirty-set)
(def dirty-chain (atom (js/Promise.resolve nil)))

(defn add-to-dirty-set! [idb entries]
  (swap! dirty-chain
    (fn [chain]
      (-> chain
          (.catch (fn [_] nil))
          (.then (fn [_] (io/read-dirty-set idb)))
          (.then (fn [current] (io/write-dirty-set! idb (merge current entries))))))))

(defn remove-from-dirty-set! [idb ids]
  (swap! dirty-chain
    (fn [chain]
      (-> chain
          (.catch (fn [_] nil))
          (.then (fn [_] (io/read-dirty-set idb)))
          (.then (fn [current] (io/write-dirty-set! idb (apply dissoc current ids))))))))

;endregion
```

Key changes:
- `add-to-dirty-set!` now accepts a map `{id kind ...}` instead of a set `#{id ...}`
- `remove-from-dirty-set!` accepts a seq/set of bare IDs and `dissoc`s them from the map
- Both chain onto `dirty-chain` atom to serialize read-modify-write cycles
- `.catch` at start of each chain ensures a prior failure doesn't block subsequent operations

- [ ] **Step 8: Run test to verify "add" test passes**

Run: `clj -M:test:cljs once`
Expected: The "add entries" test passes

- [ ] **Step 9: Update "multiple add calls accumulate" test**

```clojure
    (it "multiple add-to-dirty-set! calls accumulate entries"
      (-> (io/open "test-dirty-3" legend)
          (.then (fn [idb]
                   (-> (sut/add-to-dirty-set! idb {1 :bibelot 2 :bibelot})
                       (.then (fn [_] (sut/add-to-dirty-set! idb {3 :bibelot 4 :bibelot})))
                       (.then (fn [_] (sut/read-dirty-set idb)))
                       (.then (fn [result]
                                (should= {1 :bibelot 2 :bibelot 3 :bibelot 4 :bibelot} result)
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-3"))))))))
```

- [ ] **Step 10: Run test to verify pass**

Run: `clj -M:test:cljs once`
Expected: Pass

- [ ] **Step 11: Update "remove-from-dirty-set!" test**

```clojure
    (it "remove-from-dirty-set! removes specific IDs"
      (-> (io/open "test-dirty-4" legend)
          (.then (fn [idb]
                   (-> (sut/add-to-dirty-set! idb {1 :bibelot 2 :bibelot 3 :bibelot 4 :bibelot})
                       (.then (fn [_] (sut/remove-from-dirty-set! idb #{2 4})))
                       (.then (fn [_] (sut/read-dirty-set idb)))
                       (.then (fn [result]
                                (should= {1 :bibelot 3 :bibelot} result)
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "test-dirty-4"))))))))
```

- [ ] **Step 12: Run test to verify pass**

Run: `clj -M:test:cljs once`
Expected: Pass

- [ ] **Step 13: Add dirty-chain reset to idb_common_spec before block**

The `dirty-chain` atom persists across tests and could carry rejected promises between contexts. Add a reset to the spec. At the top of the describe block in `idb_common_spec.cljs`:

```clojure
(describe "IDB"

  (before (reset! sut/dirty-chain (js/Promise.resolve nil)))

  (context "dirty set"
  ...
```

Also add the `before` macro to the require-macros if not already there (it already is).

- [ ] **Step 14: Run idb_common_spec to verify all dirty set tests pass**

Run: `clj -M:test:cljs once`
Expected: All 4 dirty set tests pass

- [ ] **Step 15: Commit**

```bash
git add src/cljs/c3kit/bucket/idb_io.cljs src/cljs/c3kit/bucket/idb_common.cljs spec/cljs/c3kit/bucket/idb_common_spec.cljs
git commit -m "change dirty set format to {id kind} map; serialize mutations via promise chain"
```

---

### Task 6: Update persist-offline! and persist-batch-offline! callers

**Files:**
- Modify: `src/cljs/c3kit/bucket/idb_common.cljs:167-209` (persist functions)
- Modify: `spec/cljs/c3kit/bucket/indexeddb_spec.cljs` (dirty set assertions)
- Modify: `spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs` (dirty set assertions)

Update the optimistic transaction functions to pass `{id kind}` map entries to the dirty set, and update all test assertions.

#### Part A: Update persist-offline!

- [ ] **Step 1: Update indexeddb_spec "offline create assigns negative ID and marks dirty" test**

Change the dirty set assertion from `#{-1}` to `{-1 :bibelot}`:

```clojure
    (it "offline create assigns negative ID and marks dirty"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-offline-1" :online? (constantly false)} [bibelot])]
        (-> (idb/init! db)
            (.then (fn [db]
                     (let [saved (api/-tx db {:kind :bibelot :name "offline-widget"})]
                       (should= -1 (:id saved))
                       (should= "offline-widget" (:name saved))
                       (idb/read-dirty-set @(.-idb-atom db)))))
            (.then (fn [dirty]
                     (should= {-1 :bibelot} dirty)
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-idb-offline-1"))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — `persist-offline!` still passes old format

- [ ] **Step 3: Update persist-offline!**

```clojure
(defn- persist-offline! [idb entity result]
  (let [id      (:id result)
        kind    (:kind entity)
        delete? (api/delete? entity)]
    (if (and delete? (neg? id))
      (-> (delete-entity idb kind id)
          (.then (fn [_] (remove-from-dirty-set! idb #{id}))))
      (-> (put-entity idb result)
          (.then (fn [_] (add-to-dirty-set! idb {id kind})))))))
```

- [ ] **Step 4: Run test to verify pass**

Run: `clj -M:test:cljs once`
Expected: The "offline create" test passes

- [ ] **Step 5: Update remaining indexeddb_spec offline tx dirty set assertions**

Update each test's dirty set assertion. Here are the changes for each test:

"online create uses positive ID with no dirty tracking" — `(should= #{} dirty)` → `(should= {} dirty)`:
```clojure
            (.then (fn [dirty]
                     (should= {} dirty)
```

"offline update marks existing entity dirty" — `(should= 1 (count dirty))` stays (works for maps too).

"offline delete of server-known entity creates tombstone" — `(should= 1 (count dirty))` stays.

"offline delete of offline-created entity cleans up" — `(should= #{} dirty)` → `(should= {} dirty)`:
```clojure
            (.then (fn [dirty]
                     (should= {} dirty)
```

- [ ] **Step 6: Run tests to verify pass**

Run: `clj -M:test:cljs once`
Expected: All updated offline tx tests pass

#### Part B: Update persist-batch-offline!

- [ ] **Step 7: Update "offline batch create" test assertion**

Change `(should= #{-1 -2} dirty)` → `(should= {-1 :bibelot -2 :bibelot} dirty)`:
```clojure
            (.then (fn [dirty]
                     (should= {-1 :bibelot -2 :bibelot} dirty)
```

- [ ] **Step 8: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL

- [ ] **Step 9: Update persist-batch-offline!**

```clojure
(defn- persist-batch-offline! [idb entities results]
  (let [neg-deletes  (filterv #(and (api/delete? %) (neg? (:id %))) entities)
        pos-deletes  (filterv #(and (:db/delete? %) (pos? (:id %))) results)
        to-persist   (filterv #(not (:db/delete? %)) results)
        dirty-add    (into {} (map (juxt :id :kind)) (concat to-persist pos-deletes))
        dirty-remove (into #{} (map :id) neg-deletes)]
    (js/Promise.all
      (clj->js (cond-> []
                 (seq to-persist) (conj (put-entities idb to-persist))
                 (seq pos-deletes) (conj (put-entities idb pos-deletes))
                 (seq neg-deletes) (into (map #(delete-entity idb (:kind %) (:id %)) neg-deletes))
                 (seq dirty-add) (conj (add-to-dirty-set! idb dirty-add))
                 (seq dirty-remove) (conj (remove-from-dirty-set! idb dirty-remove)))))))
```

Key change: `dirty-add` is now `(into {} (map (juxt :id :kind)) ...)` instead of `(into #{} (map :id) ...)`.

- [ ] **Step 10: Run test to verify pass**

Run: `clj -M:test:cljs once`
Expected: Pass

- [ ] **Step 11: Update "mixed creates and deletes in offline batch" test assertion**

```clojure
            (.then (fn [dirty]
                     (should= 2 (count dirty))
```

This assertion uses `count` which works for maps too. No change needed.

#### Part C: Update re_indexeddb_spec assertions

- [ ] **Step 12: Update re_indexeddb_spec dirty set assertions**

"assigns negative ID and marks dirty" — `(should= #{-1} dirty)` → `(should= {-1 :bibelot} dirty)`:
```clojure
            (.then (fn [dirty]
                     (should= {-1 :bibelot} dirty)
```

- [ ] **Step 13: Run all tests to verify everything is green**

Run: `clj -M:test:cljs once`
Expected: All tests pass

#### Part D: Update sync lifecycle dirty set assertions

- [ ] **Step 14: Update indexeddb_spec sync lifecycle dirty set assertions**

"sync-complete! clears dirty state" — `(should= #{} dirty)` → `(should= {} dirty)`:
```clojure
            (.then (fn [dirty]
                     (should= {} dirty)
```

"sync-complete! clears tombstones" — `(should= #{} dirty)` → `(should= {} dirty)`:
```clojure
            (.then (fn [dirty]
                     (should= {} dirty)
```

- [ ] **Step 15: Add dirty-chain reset to indexeddb_spec and re_indexeddb_spec**

In `indexeddb_spec.cljs`, the `before` blocks in "offline tx" and "sync lifecycle" contexts already reset `idb/offline-id-counter`. Add dirty-chain reset alongside:

```clojure
  (context "offline tx"
    (before (reset! idb/offline-id-counter 0)
            (reset! idb/dirty-chain (js/Promise.resolve nil)))
    ...)

  (context "sync lifecycle"
    (before (reset! idb/offline-id-counter 0)
            (reset! idb/dirty-chain (js/Promise.resolve nil)))
    ...)
```

In `re_indexeddb_spec.cljs`:
```clojure
  (context "offline tx"
    (before (reset! idb/offline-id-counter 0)
            (reset! idb/dirty-chain (js/Promise.resolve nil)))
    ...)
```

- [ ] **Step 16: Run all tests to verify everything is green**

Run: `clj -M:test:cljs once`
Expected: All tests pass

- [ ] **Step 17: Commit**

```bash
git add src/cljs/c3kit/bucket/idb_common.cljs spec/cljs/c3kit/bucket/indexeddb_spec.cljs spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs
git commit -m "update persist functions and tests for {id kind} dirty set format"
```

---

### Task 7: Use targeted reads in sync! and delete-dirty-entities

**Files:**
- Modify: `src/cljs/c3kit/bucket/idb_common.cljs:88-107` (sync!, delete-dirty-entities)

Replace the read-all-then-filter pattern with targeted reads using `io/read-entity`. The dirty set now contains `{id kind}` pairs, so we can read each entity directly by kind+ID.

- [ ] **Step 1: Run sync lifecycle tests to confirm green baseline**

Run: `clj -M:test:cljs once`
Expected: All sync lifecycle tests pass

- [ ] **Step 2: Update sync!**

Replace reading all entities and filtering with targeted reads:

```clojure
(defn sync! [db callback]
  (let [idb @(.-idb-atom db)]
    (-> (io/read-dirty-set idb)
        (.then (fn [dirty-entries]
                 (if (empty? dirty-entries)
                   (callback [])
                   (-> (js/Promise.all
                         (clj->js (map (fn [[id kind]] (io/read-entity idb (name kind) id)) dirty-entries)))
                       (.then (fn [results]
                                (callback (vec (remove nil? (array-seq results)))))))))))))
```

Key changes:
- `dirty-ids` renamed to `dirty-entries` (now a map)
- Instead of `io/read-all-entities` + `filterv`, reads each dirty entity directly by kind+ID
- `remove nil?` handles entities that were deleted from IDB but still in the dirty set

- [ ] **Step 3: Run sync lifecycle tests**

Run: `clj -M:test:cljs once`
Expected: All sync lifecycle tests pass

- [ ] **Step 4: Update delete-dirty-entities**

Replace reading all entities and filtering with direct deletes:

```clojure
(defn- delete-dirty-entities [idb dirty-entries]
  (if (empty? dirty-entries)
    (js/Promise.resolve nil)
    (js/Promise.all
      (clj->js (map (fn [[id kind]] (delete-entity idb kind id)) dirty-entries)))))
```

Key changes:
- Takes `dirty-entries` (map of `{id kind}`) instead of `dirty-ids` (set of IDs)
- Deletes each entity directly by kind+ID — no need to read all entities first

- [ ] **Step 5: Update sync-complete! to pass dirty-entries**

`sync-complete!` currently passes `dirty-ids` (bare IDs) to `delete-dirty-entities`. It needs to pass the `{id kind}` entries instead. But `sync-complete!`'s public API receives bare `dirty-ids` from the consumer. We need to read the dirty set to get the kind info.

Actually, `sync-complete!` already knows what to delete: the entities identified by `dirty-ids`. Since the consumer calls `sync!` first (which reads the dirty set), and the dirty set hasn't changed between `sync!` and `sync-complete!`, we can read the dirty set again to get the entries:

```clojure
(defn sync-complete! [db dirty-ids server-entities]
  (let [idb    @(.-idb-atom db)
        id-set (set dirty-ids)]
    (soft-delete-neg-ids! db dirty-ids)
    (when (seq server-entities) (memory/tx* db server-entities))
    (-> (io/read-dirty-set idb)
        (.then (fn [dirty-entries]
                 (let [entries-to-delete (select-keys dirty-entries id-set)]
                   (-> (delete-dirty-entities idb entries-to-delete)
                       (.then (fn [_] (remove-from-dirty-set! idb id-set)))
                       (.then (fn [_]
                                (if (seq server-entities)
                                  (put-entities idb server-entities)
                                  (js/Promise.resolve nil)))))))))))
```

- [ ] **Step 6: Run all tests**

Run: `clj -M:test:cljs once`
Expected: All tests pass

- [ ] **Step 7: Commit**

```bash
git add src/cljs/c3kit/bucket/idb_common.cljs
git commit -m "use targeted reads in sync! and delete-dirty-entities instead of read-all-then-filter"
```

---

### Task 8: Update idb_reader to use targeted reads and new dirty set format

**Files:**
- Modify: `src/cljs/c3kit/bucket/idb_reader.cljs` (use targeted reads, delegate delete)
- Modify: `spec/cljs/c3kit/bucket/idb_reader_spec.cljs` (update dirty set format in seeds)

`idb_reader` is used by service workers for offline sync. Update it to use the new `{id kind}` dirty set format and targeted reads via `io/read-entity`.

- [ ] **Step 1: Update seed helper in idb_reader_spec to use new format**

Change `seed-entity-and-dirty!` to write the map format:

```clojure
(defn- seed-entity-and-dirty! [idb entities dirty-entries]
  (let [tx    (.transaction idb #js ["bibelot" "_meta"] "readwrite")
        store (.objectStore tx "bibelot")
        meta  (.objectStore tx "_meta")]
    (doseq [entity entities]
      (.put store (io/clj->js-entity entity)))
    (.put meta (io/clj->js-entity {:id "dirty" :data dirty-entries}))
    (js/Promise.
     (fn [resolve _]
       (set! (.-oncomplete tx) #(resolve idb))))))
```

Note the parameter name change: `dirty-ids` → `dirty-entries`.

- [ ] **Step 2: Update "reads dirty entities from IDB" test call**

Change the seed call from `[-1]` to `{-1 :bibelot}`:

```clojure
    (it "reads dirty entities from IDB"
      (-> (io/open "test-reader-1" legend)
          (.then (fn [idb]
                   (seed-entity-and-dirty! idb [{:id -1 :kind :bibelot :name "offline" :size 5}] {-1 :bibelot})))
          (.then (fn [idb]
                   (-> (sut/dirty-entities idb)
                       (.then (fn [result]
                                (should= 1 (count result))
                                (should= -1 (:id (first result)))
                                (should= "offline" (:name (first result)))
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "test-reader-1"))))))))
```

- [ ] **Step 3: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — `dirty-entities` reads old format

- [ ] **Step 4: Update dirty-entities implementation**

```clojure
(ns c3kit.bucket.idb-reader
  (:require [c3kit.bucket.idb-io :as io]))

;region Reading Dirty Entities

(defn dirty-entities [idb]
  (-> (io/read-dirty-set idb)
      (.then (fn [dirty-entries]
               (if (empty? dirty-entries)
                 (js/Promise.resolve [])
                 (-> (js/Promise.all
                       (clj->js (map (fn [[id kind]] (io/read-entity idb (name kind) id)) dirty-entries)))
                     (.then (fn [results]
                              (vec (remove nil? (array-seq results)))))))))))

;endregion
```

- [ ] **Step 5: Run test to verify pass**

Run: `clj -M:test:cljs once`
Expected: The "reads dirty entities" test passes

- [ ] **Step 6: Update "clear-dirty!" test seed and assertion**

Update the seed to use map format, and the dirty set assertion to expect a map:

```clojure
  (context "clear-dirty!"

    (it "removes dirty IDs and their entities from IDB"
      (-> (io/open "test-reader-3" legend)
          (.then (fn [idb]
                   (seed-entity-and-dirty! idb
                                           [{:id -1 :kind :bibelot :name "first" :size 1}
                                            {:id -2 :kind :bibelot :name "second" :size 2}]
                                           {-1 :bibelot -2 :bibelot})))
          (.then (fn [idb]
                   (-> (sut/clear-dirty! idb #{-1})
                       (.then (fn [_]
                                (let [tx      (.transaction idb #js ["_meta"] "readonly")
                                      store   (.objectStore tx "_meta")
                                      request (.get store "dirty")]
                                  (js/Promise.
                                   (fn [resolve _]
                                     (set! (.-onsuccess request)
                                           (fn [event]
                                             (let [result (io/js->clj-entity (.-result (.-target event)))]
                                               (should= {-2 :bibelot} (:data result))
                                               (resolve idb)))))))))
                       (.then (fn [idb]
                                (let [tx      (.transaction idb #js ["bibelot"] "readonly")
                                      store   (.objectStore tx "bibelot")
                                      request (.get store -1)]
                                  (js/Promise.
                                   (fn [resolve _]
                                     (set! (.-onsuccess request)
                                           (fn [event]
                                             (should= nil (.-result (.-target event)))
                                             (resolve idb))))))))
                       (.then (fn [idb]
                                (let [tx      (.transaction idb #js ["bibelot"] "readonly")
                                      store   (.objectStore tx "bibelot")
                                      request (.get store -2)]
                                  (js/Promise.
                                   (fn [resolve _]
                                     (set! (.-onsuccess request)
                                           (fn [event]
                                             (let [entity (io/js->clj-entity (.-result (.-target event)))]
                                               (should= "second" (:name entity))
                                               (io/close idb)
                                               (resolve (.deleteDatabase js/indexedDB "test-reader-3"))))))))))))))
  ))
```

- [ ] **Step 7: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — `clear-dirty!` still uses old format

- [ ] **Step 8: Update clear-dirty! implementation**

Replace `delete-entities-by-ids` (which reads all entities) with targeted deletes, and use the dirty set map format:

```clojure
;region Clearing Dirty Entities

(defn clear-dirty! [idb ids-to-clear]
  (let [id-set (set ids-to-clear)]
    (-> (io/read-dirty-set idb)
        (.then (fn [dirty-entries]
                 (let [entries-to-delete (select-keys dirty-entries id-set)
                       remaining         (apply dissoc dirty-entries id-set)]
                   (-> (io/write-dirty-set! idb remaining)
                       (.then (fn [_]
                                (if (empty? entries-to-delete)
                                  (js/Promise.resolve nil)
                                  (let [store-names (into #{} (map (comp name val)) entries-to-delete)]
                                    (io/batch-tx idb store-names
                                                     (fn [tx]
                                                       (doseq [[id kind] entries-to-delete]
                                                         (.delete (.objectStore tx (name kind)) id)))
                                                     nil))))))))))))

;endregion
```

Key changes:
- Uses `select-keys` on the dirty map to find entries to delete (has kind info)
- Writes updated dirty map directly (no read-modify-write — already has the data)
- Deletes entities by kind+ID using batch-tx (no need to read all entities first)

- [ ] **Step 9: Run all tests to verify pass**

Run: `clj -M:test:cljs once`
Expected: All tests pass

- [ ] **Step 10: Commit**

```bash
git add src/cljs/c3kit/bucket/idb_reader.cljs spec/cljs/c3kit/bucket/idb_reader_spec.cljs
git commit -m "update idb_reader: targeted reads, new dirty set format, eliminate read-all-then-filter"
```
