# IndexedDB Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add IndexedDB persistence to c3kit-bucket with two impls: `:indexeddb` (plain atom) and `:re-indexeddb` (reagent/atom with reactivity).

**Architecture:** IDB is the source of truth. An in-memory store (Memory or ReMemory) serves as a synchronous read cache. Writes go to IDB first, then sync to the in-memory store on success. Reads are always synchronous from the in-memory store.

**Tech Stack:** ClojureScript, IndexedDB (browser API), Reagent (for `:re-indexeddb`), speclj

---

## File Structure

| File | Responsibility |
|------|---------------|
| `src/cljs/c3kit/bucket/idb.cljs` | Core IDB operations: open, read, write, delete, versioning, rehydrate. No protocol implementation — pure IDB plumbing. |
| `src/cljs/c3kit/bucket/indexeddb.cljs` | `IndexedDB` deftype implementing `api/DB` + `migrator/Migrator`. Delegates reads to Memory, writes through IDB then Memory. |
| `src/cljs/c3kit/bucket/re_indexeddb.cljs` | `ReIndexedDB` deftype implementing `api/DB` + `migrator/Migrator`. Delegates reads to ReMemory, writes through IDB then ReMemory. |
| `spec/cljs/c3kit/bucket/idb_spec.cljs` | Tests for core IDB operations (open, versioning, read/write/delete). |
| `spec/cljs/c3kit/bucket/indexeddb_spec.cljs` | Tests for `:indexeddb` impl — runs shared impl_spec suites + IDB-specific tests. |
| `spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs` | Tests for `:re-indexeddb` impl — runs shared impl_spec suites + reactivity tests. |

---

### Task 1: Core IDB Operations — Open and Schema Versioning

**Files:**
- Create: `src/cljs/c3kit/bucket/idb.cljs`
- Create: `spec/cljs/c3kit/bucket/idb_spec.cljs`

- [ ] **Step 1: Write failing test — open-db creates an IDB database**

```clojure
;; spec/cljs/c3kit/bucket/idb_spec.cljs
(ns c3kit.bucket.idb-spec
  (:require-macros [speclj.core :refer [around before describe it should should-be-nil should-not-be-nil should= with]])
  (:require [c3kit.bucket.idb :as sut]
            [speclj.core]))

(describe "IDB"

  (before (.deleteDatabase js/indexedDB "test-db")
          (.removeItem js/localStorage "test-db-schema-hash")
          (.removeItem js/localStorage "test-db-schema-ver"))

  (it "opens a database"
    (let [result (atom nil)]
      (-> (sut/open "test-db" {:user {:id {:type :long} :name {:type :string}}})
          (.then (fn [db]
                   (reset! result db)
                   (.close db))))
      ;; speclj async — need to wait for promise
      ;; Use a short timeout to let the promise resolve
      ))
  )
```

**Note:** IndexedDB is async and browser-based. ClojureScript tests run in a browser via speclj. We need to understand how async tests work in this project's test setup before proceeding. Let me check the test runner configuration.

Actually, let me reconsider the approach. The IDB operations are async (promise-based), but the existing speclj test framework used in this project is synchronous. Testing raw IDB operations directly will require async test support.

However, the **protocol-level tests** (via `impl_spec`) are synchronous because reads go through the in-memory store. The async part (IDB writes) resolves before the in-memory store is updated, so we need to handle that in tests.

Let me restructure the plan to account for this reality.

---

### Task 1: Core IDB Plumbing — Schema Hashing and Version Management

**Files:**
- Create: `src/cljs/c3kit/bucket/idb.cljs`
- Create: `spec/cljs/c3kit/bucket/idb_spec.cljs`

- [ ] **Step 1: Write failing test — schema-hash produces consistent hash from legend**

```clojure
;; spec/cljs/c3kit/bucket/idb_spec.cljs
(ns c3kit.bucket.idb-spec
  (:require-macros [speclj.core :refer [around before context describe it should should= should-not=]])
  (:require [c3kit.bucket.idb :as sut]
            [speclj.core]))

(describe "IDB"

  (context "schema-hash"

    (it "produces a consistent hash from a legend"
      (let [legend {:user {:id {:type :long} :name {:type :string}}}]
        (should= (sut/schema-hash legend) (sut/schema-hash legend))))

    (it "produces different hashes for different legends"
      (let [legend-1 {:user {:id {:type :long} :name {:type :string}}}
            legend-2 {:user {:id {:type :long} :name {:type :string} :email {:type :string}}}]
        (should-not= (sut/schema-hash legend-1) (sut/schema-hash legend-2))))
    )
  )
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — `sut/schema-hash` not found

- [ ] **Step 3: Implement schema-hash**

```clojure
;; src/cljs/c3kit/bucket/idb.cljs
(ns c3kit.bucket.idb)

(defn schema-hash
  "Produces a deterministic hash of a legend for IDB versioning.
   Any change to kinds or attributes produces a different hash."
  [legend]
  (hash (into (sorted-map)
              (map (fn [[k v]] [k (into (sorted-map) v)]))
              legend)))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `clj -M:test:cljs once`
Expected: PASS

- [ ] **Step 5: Write failing test — idb-version returns 1 for new database**

```clojure
;; Add to idb_spec.cljs, inside the "IDB" describe block

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
      (should= 2 (sut/idb-version "test-db" legend-2))))
  )
```

- [ ] **Step 6: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — `sut/idb-version` not found

- [ ] **Step 7: Implement idb-version**

```clojure
;; Add to src/cljs/c3kit/bucket/idb.cljs

(defn idb-version
  "Returns the IDB version number for the given db-name and legend.
   Stores schema hash and version counter in localStorage.
   Returns the same version if legend is unchanged, increments if changed."
  [db-name legend]
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
```

- [ ] **Step 8: Run test to verify it passes**

Run: `clj -M:test:cljs once`
Expected: PASS

- [ ] **Step 9: Commit**

```bash
git add src/cljs/c3kit/bucket/idb.cljs spec/cljs/c3kit/bucket/idb_spec.cljs
git commit -m "Add IDB schema hashing and version management"
```

---

### Task 2: Core IDB Plumbing — Open, Close, and Object Store Creation

**Files:**
- Modify: `src/cljs/c3kit/bucket/idb.cljs`
- Modify: `spec/cljs/c3kit/bucket/idb_spec.cljs`

**Note on async testing:** IndexedDB operations return promises. The speclj test framework in this project is synchronous. We'll need a helper to block on promises in tests. ClojureScript can't truly block, but we can use speclj's async support or a helper that resolves promises within a test tick. Check if `c3kit.wire.spec-helper` or similar provides async test utilities. If not, we'll use a pattern where the IDB is opened in a `before` block and tests verify the state after the promise resolves via `should-eventually` or by using `js/Promise` chaining with done callbacks.

**Practical approach:** Since IDB operations in tests happen against an in-memory IDB (browser test environment), they resolve almost instantly. We can use a `with` block that opens the database and stores it, combined with `before` blocks that wait. Alternatively, we test IDB indirectly through the protocol implementation where reads are synchronous and writes can be flushed synchronously in tests by awaiting the promise.

For the core `idb.cljs` tests, let's use a pragmatic approach: test the synchronous functions directly (schema-hash, idb-version), and test the async IDB operations through the protocol-level tests in Tasks 4 and 5. The protocol tests use the shared `impl_spec` which calls synchronous reads after writes — we just need to ensure writes resolve before reads.

- [ ] **Step 1: Implement open-db**

```clojure
;; Add to src/cljs/c3kit/bucket/idb.cljs

(defn- ensure-object-stores
  "Called during onupgradeneeded to reconcile object stores with the legend."
  [db legend]
  (let [existing (set (array-seq (.-objectStoreNames db)))
        expected (set (map name (keys legend)))
        to-create (disj expected "_meta")
        all-expected (conj expected "_meta")]
    ;; Create missing stores
    (doseq [store-name (conj to-create "_meta")]
      (when-not (contains? existing store-name)
        (.createObjectStore db store-name #js {:keyPath "id"})))
    ;; Remove stale stores
    (doseq [store-name existing]
      (when-not (contains? all-expected store-name)
        (.deleteObjectStore db store-name)))))

(defn open
  "Opens an IndexedDB database with object stores matching the legend.
   Returns a js/Promise resolving to the IDB database instance."
  [db-name legend]
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
```

- [ ] **Step 2: Implement close**

```clojure
;; Add to src/cljs/c3kit/bucket/idb.cljs

(defn close
  "Closes an IndexedDB database connection."
  [db]
  (when db (.close db)))
```

- [ ] **Step 3: Run existing tests to verify nothing is broken**

Run: `clj -M:test:cljs once`
Expected: All existing tests PASS

- [ ] **Step 4: Commit**

```bash
git add src/cljs/c3kit/bucket/idb.cljs
git commit -m "Add IDB open, close, and object store creation"
```

---

### Task 3: Core IDB Plumbing — Read, Write, Delete, Clear, and Rehydrate

**Files:**
- Modify: `src/cljs/c3kit/bucket/idb.cljs`

- [ ] **Step 1: Implement put-entity (write single entity to IDB)**

```clojure
;; Add to src/cljs/c3kit/bucket/idb.cljs

(defn- clj->js-entity
  "Converts a Clojure entity map to a JS object suitable for IDB storage."
  [entity]
  (clj->js entity :keyword-fn #(.-fqn %)))

(defn- js->clj-entity
  "Converts a JS object from IDB back to a Clojure entity map."
  [js-obj]
  (when js-obj
    (let [m (js->clj js-obj :keywordize-keys true)]
      (if-let [kind (:kind m)]
        (assoc m :kind (keyword kind))
        m))))

(defn put-entity
  "Writes a single entity to its kind's object store.
   Returns a js/Promise resolving to the entity."
  [idb entity]
  (js/Promise.
   (fn [resolve reject]
     (let [store-name (name (:kind entity))
           tx         (.transaction idb #js [store-name] "readwrite")
           store      (.objectStore tx store-name)
           request    (.put store (clj->js-entity entity))]
       (set! (.-onsuccess request) (fn [_] (resolve entity)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))
```

- [ ] **Step 2: Implement put-entities (write batch to IDB)**

```clojure
;; Add to src/cljs/c3kit/bucket/idb.cljs

(defn put-entities
  "Writes multiple entities to IDB in a single transaction.
   All entities must succeed or the entire transaction rolls back.
   Returns a js/Promise resolving to the entities."
  [idb entities]
  (if (empty? entities)
    (js/Promise.resolve entities)
    (let [store-names (into #{} (map #(name (:kind %))) entities)]
      (js/Promise.
       (fn [resolve reject]
         (let [tx    (.transaction idb (clj->js (vec store-names)) "readwrite")]
           (set! (.-oncomplete tx) (fn [_] (resolve entities)))
           (set! (.-onerror tx) (fn [event] (reject (.-error (.-target event)))))
           (set! (.-onabort tx) (fn [event] (reject (.-error (.-target event)))))
           (doseq [entity entities]
             (let [store (.objectStore tx (name (:kind entity)))]
               (.put store (clj->js-entity entity))))))))))
```

- [ ] **Step 3: Implement delete-entity**

```clojure
;; Add to src/cljs/c3kit/bucket/idb.cljs

(defn delete-entity
  "Deletes a single entity from its kind's object store by id.
   Returns a js/Promise."
  [idb kind id]
  (js/Promise.
   (fn [resolve reject]
     (let [store-name (name kind)
           tx         (.transaction idb #js [store-name] "readwrite")
           store      (.objectStore tx store-name)
           request    (.delete store id)]
       (set! (.-onsuccess request) (fn [_] (resolve nil)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))
```

- [ ] **Step 4: Implement clear-store and clear-all**

```clojure
;; Add to src/cljs/c3kit/bucket/idb.cljs

(defn clear-store
  "Clears all entities from a single object store.
   Returns a js/Promise."
  [idb kind]
  (js/Promise.
   (fn [resolve reject]
     (let [store-name (name kind)
           tx         (.transaction idb #js [store-name] "readwrite")
           store      (.objectStore tx store-name)
           request    (.clear store)]
       (set! (.-onsuccess request) (fn [_] (resolve nil)))
       (set! (.-onerror request) (fn [event] (reject (.-error (.-target event)))))))))

(defn clear-all
  "Clears all object stores except _meta.
   Returns a js/Promise."
  [idb]
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
```

- [ ] **Step 5: Implement rehydrate**

```clojure
;; Add to src/cljs/c3kit/bucket/idb.cljs

(defn- read-store
  "Reads all entities from a single object store.
   Returns a js/Promise resolving to a seq of Clojure entity maps."
  [idb store-name]
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

(defn rehydrate
  "Loads entities from IDB into the in-memory store via tx-fn.
   kinds - optional seq of kind keywords. If empty, loads all kinds.
   tx-fn - function that accepts a seq of entities and loads them into the in-memory store.
   Returns a js/Promise."
  [idb kinds tx-fn]
  (let [store-names (if (seq kinds)
                      (map name kinds)
                      (->> (array-seq (.-objectStoreNames idb))
                           (remove #(= "_meta" %))))]
    (-> (js/Promise.all (clj->js (map #(read-store idb %) store-names)))
        (.then (fn [results]
                 (let [all-entities (mapcat identity (array-seq results))]
                   (when (seq all-entities)
                     (tx-fn all-entities))))))))
```

- [ ] **Step 6: Run existing tests to verify nothing is broken**

Run: `clj -M:test:cljs once`
Expected: All existing tests PASS

- [ ] **Step 7: Commit**

```bash
git add src/cljs/c3kit/bucket/idb.cljs
git commit -m "Add IDB read, write, delete, clear, and rehydrate operations"
```

---

### Task 4: IndexedDB Protocol Implementation (`:indexeddb` — plain atom, no reactivity)

**Files:**
- Create: `src/cljs/c3kit/bucket/indexeddb.cljs`
- Create: `spec/cljs/c3kit/bucket/indexeddb_spec.cljs`

- [ ] **Step 1: Write failing test — basic tx and entity lookup**

```clojure
;; spec/cljs/c3kit/bucket/indexeddb_spec.cljs
(ns c3kit.bucket.indexeddb-spec
  (:require-macros [speclj.core :refer [around around-all before context describe it should should-be-nil should= with]]
                   [c3kit.bucket.api :refer [with-safety-off]])
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.indexeddb :as sut]
            [c3kit.bucket.idb :as idb]
            [c3kit.apron.schema :as s]
            [speclj.core]))

(def bibelot
  {:kind  (s/kind :bibelot)
   :id    {:type :long}
   :name  {:type :string}
   :size  {:type :long}
   :color {:type :string}})

(describe "IndexedDB"
  (around [it] (with-safety-off (it)))

  (around-all [it]
    (let [db (api/create-db {:impl :indexeddb :db-name "test-indexeddb"} [bibelot])]
      (with-redefs [api/*safety* false
                    api/impl     (atom db)]
        (-> (sut/rehydrate! db)
            (.then (fn []
                     (try (it)
                          (finally
                            (.close db)
                            (.deleteDatabase js/indexedDB "test-indexeddb")))))))))

  (before (api/clear))

  (it "tx returns a promise that resolves to the saved entity"
    (-> (api/tx {:kind :bibelot :name "widget" :size 5})
        (.then (fn [result]
                 (should= "widget" (:name result))
                 (should= 5 (:size result))))))

  (it "entity retrieves from in-memory store after tx resolves"
    (-> (api/tx {:kind :bibelot :name "widget" :size 5})
        (.then (fn [result]
                 (let [loaded (api/entity :bibelot (:id result))]
                   (should= "widget" (:name loaded)))))))
  )
```

**Note on async test strategy:** The exact test setup depends on how speclj handles async in the project's ClojureScript test runner. The pattern above assumes promise-based returns. If speclj doesn't support async, we may need a different approach — e.g., using `js/Promise` with a done callback or testing synchronously by awaiting IDB writes in `before` blocks. Adjust the test patterns based on what the test runner supports.

- [ ] **Step 2: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — `c3kit.bucket.indexeddb` namespace not found

- [ ] **Step 3: Implement IndexedDB deftype**

```clojure
;; src/cljs/c3kit/bucket/indexeddb.cljs
(ns c3kit.bucket.indexeddb
  (:refer-clojure :rename {find core-find count core-count reduce core-reduce})
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb :as idb]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as migrator]))

;region Helpers

(defn- idb-tx
  "Writes entity to IDB, then syncs to in-memory store on success."
  [db entity]
  (let [entity (memory/ensure-id entity)]
    (if (api/delete? entity)
      (-> (idb/delete-entity (.-idb db) (:kind entity) (:id entity))
          (.then (fn [_]
                   (swap! (.-store db) #(memory/tx-entity @(.-legend db) % entity))
                   (memory/tx-result db entity))))
      (let [coerced (memory/tx db entity)]
        ;; tx already updated memory; now persist to IDB
        (-> (idb/put-entity (.-idb db) coerced)
            (.then (fn [_] coerced)))))))

;endregion

;region Protocol Implementation

(deftype IndexedDB [legend store idb]
  api/DB
  (-clear [this]
    (memory/clear this)
    (idb/clear-all idb))
  (close [_this] (idb/close idb))
  (-count [this kind options] (core-count (memory/do-find this kind options)))
  (-delete-all [this kind]
    (memory/delete-all this kind)
    (idb/clear-store idb kind))
  (-entity [this kind id] (memory/entity this kind id))
  (-find [this kind options] (memory/do-find this kind options))
  (-reduce [this kind f init options] (core-reduce f init (memory/do-find this kind options)))
  (-tx [this entity] (idb-tx this entity))
  (-tx* [this entities]
    (let [entities (map memory/ensure-id entities)]
      ;; Write to IDB first, then sync to memory on success
      (let [to-delete (filter api/delete? entities)
            to-save   (remove api/delete? entities)]
        ;; Sync to memory first to get coerced entities
        (swap! (.-store this) (fn [store]
                                (core-reduce #(memory/tx-entity @(.-legend this) %1 %2) store entities)))
        (let [coerced (map #(memory/tx-result this %) entities)]
          (-> (idb/put-entities idb (remove api/delete? coerced))
              (.then (fn [_] coerced)))))))
  migrator/Migrator
  (-schema-exists? [_this schema] (contains? @legend (api/-schema-kind schema)))
  (-installed-schema-legend [_this _expected-legend] @legend)
  (-install-schema! [this schema] (memory/do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [_this kind attr spec] (swap! legend assoc-in [kind attr] spec))
  (-remove-attribute! [this kind attr] (memory/do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (memory/do-rename-attribute! this kind attr new-kind new-attr)))

;endregion

;region Public API

(defn rehydrate!
  "Loads entities from IDB into the in-memory store.
   Call with no kind args to load all kinds, or specific kinds to load selectively.
   Returns a js/Promise."
  [db & kinds]
  (idb/rehydrate (.-idb db) kinds (fn [entities] (memory/tx* db entities))))

;endregion
```

**Wait — there's a design issue.** The `idb-tx` function above first writes to memory (via `memory/tx`), then persists to IDB. But our design says IDB is the source of truth — memory should only update after IDB confirms. Let me fix this.

The challenge: `memory/tx` handles ID generation, coercion, and merge-with-original. We need those operations to produce the entity, but not commit it to the store until IDB confirms.

**Revised approach:** Use `memory/tx-entity` directly on a throwaway store copy to get the coerced entity, then write to IDB, then apply to the real store.

- [ ] **Step 3 (revised): Implement IndexedDB deftype with IDB-first writes**

```clojure
;; src/cljs/c3kit/bucket/indexeddb.cljs
(ns c3kit.bucket.indexeddb
  (:refer-clojure :rename {find core-find count core-count reduce core-reduce})
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb :as idb]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as migrator]))

;region Helpers

(defn- prepare-entity
  "Coerces and merges entity against the current store without committing.
   Returns [new-store coerced-entity]."
  [db entity]
  (let [entity (memory/ensure-id entity)
        new-store (memory/tx-entity @(.-legend db) @(.-store db) entity)
        result (if (api/delete? entity)
                 (api/soft-delete entity)
                 (get-in new-store [:all (:id entity)]))]
    [new-store result]))

(defn- idb-tx
  "Writes entity to IDB first, then syncs to in-memory store on success."
  [db entity]
  (let [[new-store result] (prepare-entity db entity)]
    (if (api/delete? entity)
      (-> (idb/delete-entity (.-idb db) (:kind entity) (:id entity))
          (.then (fn [_]
                   (reset! (.-store db) new-store)
                   result)))
      (-> (idb/put-entity (.-idb db) result)
          (.then (fn [_]
                   (reset! (.-store db) new-store)
                   result))))))

(defn- idb-tx*
  "Writes multiple entities to IDB first, then syncs to in-memory store on success."
  [db entities]
  (let [entities (map memory/ensure-id entities)
        new-store (clojure.core/reduce #(memory/tx-entity @(.-legend db) %1 %2) @(.-store db) entities)
        results (map (fn [e]
                       (if (api/delete? e)
                         (api/soft-delete e)
                         (get-in new-store [:all (:id e)])))
                     entities)
        to-persist (remove api/delete? results)
        to-delete (filter api/delete? entities)]
    (-> (js/Promise.all
         (clj->js (cond-> []
                    (seq to-persist) (conj (idb/put-entities (.-idb db) to-persist))
                    (seq to-delete) (into (map #(idb/delete-entity (.-idb db) (:kind %) (:id %)) to-delete)))))
        (.then (fn [_]
                 (reset! (.-store db) new-store)
                 results)))))

;endregion

;region Protocol Implementation

(deftype IndexedDB [legend store idb]
  api/DB
  (-clear [this]
    (memory/clear this)
    (idb/clear-all idb))
  (close [_this] (idb/close idb))
  (-count [this kind options] (core-count (api/-find this kind options)))
  (-delete-all [this kind]
    (memory/delete-all this kind)
    (idb/clear-store idb kind))
  (-entity [this kind id] (memory/entity this kind id))
  (-find [this kind options] (memory/do-find this kind options))
  (-reduce [this kind f init options] (core-reduce f init (api/-find this kind options)))
  (-tx [this entity] (idb-tx this entity))
  (-tx* [this entities] (idb-tx* this entities))
  migrator/Migrator
  (-schema-exists? [_this schema] (contains? @legend (api/-schema-kind schema)))
  (-installed-schema-legend [_this _expected-legend] @legend)
  (-install-schema! [this schema] (memory/do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [_this kind attr spec] (swap! legend assoc-in [kind attr] spec))
  (-remove-attribute! [this kind attr] (memory/do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (memory/do-rename-attribute! this kind attr new-kind new-attr)))

;endregion

;region Public API

(defn rehydrate!
  "Loads entities from IDB into the in-memory store.
   Call with no kind args to load all kinds, or specific kinds to load selectively.
   Returns a js/Promise."
  [db & kinds]
  (idb/rehydrate (.-idb db) kinds (fn [entities] (memory/tx* db entities))))

;endregion

;region Registration

(defmethod api/-create-impl :indexeddb [config schemas]
  (let [legend (atom (legend/build schemas))
        store  (or (:store config) (atom {}))
        db-name (or (:db-name config) "c3kit-bucket")]
    ;; Open IDB synchronously-ish — the DB instance arrives via promise
    ;; The caller must call rehydrate! before using the db
    ;; For now, store a nil idb and set it during open
    (let [db (IndexedDB. legend store nil)]
      (-> (idb/open db-name @legend)
          (.then (fn [idb-instance]
                   (set! (.-idb db) idb-instance)
                   db)))
      db)))

;endregion
```

**Problem:** `create-db` is synchronous but IDB open is async. The `IndexedDB` deftype needs the `idb` instance, but it's not available until the promise resolves. Two options:

1. Store the IDB promise on the deftype and await it before each IDB write
2. Require the caller to await initialization before using the db

Option 2 is cleaner. Let's make `create-db` return the deftype immediately with a nil idb, and provide an `init!` function that opens IDB and rehydrates. The caller must `(.then ...)` on `init!` before using the db.

Actually, let's store the idb in an atom on the deftype so it can be set asynchronously:

- [ ] **Step 3 (final): Implement IndexedDB deftype**

```clojure
;; src/cljs/c3kit/bucket/indexeddb.cljs
(ns c3kit.bucket.indexeddb
  (:refer-clojure :rename {find core-find count core-count reduce core-reduce})
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb :as idb]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as migrator]))

;region Helpers

(defn- -idb [db] @(.-idb-atom db))

(defn- prepare-entity
  "Coerces and merges entity against the current store without committing.
   Returns [new-store coerced-entity-or-soft-delete]."
  [db entity]
  (let [entity (memory/ensure-id entity)
        new-store (memory/tx-entity @(.-legend db) @(.-store db) entity)
        result (if (api/delete? entity)
                 (api/soft-delete entity)
                 (get-in new-store [:all (:id entity)]))]
    [new-store result]))

(defn- idb-tx
  "Writes entity to IDB first, then syncs to in-memory store on success."
  [db entity]
  (let [[new-store result] (prepare-entity db entity)]
    (if (api/delete? entity)
      (-> (idb/delete-entity (-idb db) (:kind entity) (:id entity))
          (.then (fn [_]
                   (reset! (.-store db) new-store)
                   result)))
      (-> (idb/put-entity (-idb db) result)
          (.then (fn [_]
                   (reset! (.-store db) new-store)
                   result))))))

(defn- idb-tx*
  "Writes multiple entities to IDB first, then syncs to in-memory store on success."
  [db entities]
  (let [entities (map memory/ensure-id entities)
        new-store (clojure.core/reduce #(memory/tx-entity @(.-legend db) %1 %2) @(.-store db) entities)
        results (mapv (fn [e]
                        (if (api/delete? e)
                          (api/soft-delete e)
                          (get-in new-store [:all (:id e)])))
                      entities)
        to-persist (remove api/delete? results)
        to-delete (filter api/delete? entities)]
    (-> (js/Promise.all
         (clj->js (cond-> []
                    (seq to-persist) (conj (idb/put-entities (-idb db) to-persist))
                    (seq to-delete) (into (map #(idb/delete-entity (-idb db) (:kind %) (:id %)) to-delete)))))
        (.then (fn [_]
                 (reset! (.-store db) new-store)
                 results)))))

;endregion

;region Protocol Implementation

(deftype IndexedDB [legend store idb-atom]
  api/DB
  (-clear [this]
    (memory/clear this)
    (idb/clear-all @idb-atom))
  (close [_this] (idb/close @idb-atom))
  (-count [this kind options] (core-count (api/-find this kind options)))
  (-delete-all [this kind]
    (memory/delete-all this kind)
    (idb/clear-store @idb-atom kind))
  (-entity [this kind id] (memory/entity this kind id))
  (-find [this kind options] (memory/do-find this kind options))
  (-reduce [this kind f init options] (core-reduce f init (api/-find this kind options)))
  (-tx [this entity] (idb-tx this entity))
  (-tx* [this entities] (idb-tx* this entities))
  migrator/Migrator
  (-schema-exists? [_this schema] (contains? @legend (api/-schema-kind schema)))
  (-installed-schema-legend [_this _expected-legend] @legend)
  (-install-schema! [this schema] (memory/do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [_this kind attr spec] (swap! legend assoc-in [kind attr] spec))
  (-remove-attribute! [this kind attr] (memory/do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (memory/do-rename-attribute! this kind attr new-kind new-attr)))

;endregion

;region Public API

(defn init!
  "Opens the IDB database and optionally rehydrates the in-memory store.
   Must be called and awaited before using the db.
   Returns a js/Promise resolving to the db."
  [db & kinds]
  (let [db-name (or (.-db-name db) "c3kit-bucket")]
    (-> (idb/open db-name @(.-legend db))
        (.then (fn [idb-instance]
                 (reset! (.-idb-atom db) idb-instance)
                 db))
        (.then (fn [db]
                 (idb/rehydrate @(.-idb-atom db) kinds
                                (fn [entities] (memory/tx* db entities)))))
        (.then (fn [_] db)))))

;endregion

;region Registration

(defmethod api/-create-impl :indexeddb [config schemas]
  (let [legend   (atom (legend/build schemas))
        store    (or (:store config) (atom {}))
        idb-atom (atom nil)
        db-name  (or (:db-name config) "c3kit-bucket")]
    (IndexedDB. legend store idb-atom)))

;endregion
```

**Wait — the `db-name` isn't stored on the deftype.** We need to either add it as a field or pass it to `init!`. Let's add it as a field.

Let me simplify. The deftype fields will be: `legend`, `store`, `idb-atom`, `db-name`.

- [ ] **Step 3 (truly final): Implement IndexedDB deftype**

Replace the above with the corrected version that has `db-name` as a field on the deftype. The full corrected `indexeddb.cljs` is:

```clojure
;; src/cljs/c3kit/bucket/indexeddb.cljs
(ns c3kit.bucket.indexeddb
  (:refer-clojure :rename {find core-find count core-count reduce core-reduce})
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb :as idb]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as migrator]))

;region Helpers

(defn- prepare-entity
  "Coerces and merges entity against the current store without committing.
   Returns [new-store coerced-entity-or-soft-delete]."
  [db entity]
  (let [entity   (memory/ensure-id entity)
        new-store (memory/tx-entity @(.-legend db) @(.-store db) entity)
        result    (if (api/delete? entity)
                    (api/soft-delete entity)
                    (get-in new-store [:all (:id entity)]))]
    [new-store result]))

(defn- idb-tx [db entity]
  (let [[new-store result] (prepare-entity db entity)]
    (if (api/delete? entity)
      (-> (idb/delete-entity @(.-idb-atom db) (:kind entity) (:id entity))
          (.then (fn [_]
                   (reset! (.-store db) new-store)
                   result)))
      (-> (idb/put-entity @(.-idb-atom db) result)
          (.then (fn [_]
                   (reset! (.-store db) new-store)
                   result))))))

(defn- idb-tx* [db entities]
  (let [entities  (map memory/ensure-id entities)
        new-store (clojure.core/reduce #(memory/tx-entity @(.-legend db) %1 %2) @(.-store db) entities)
        results   (mapv (fn [e]
                          (if (api/delete? e)
                            (api/soft-delete e)
                            (get-in new-store [:all (:id e)])))
                        entities)
        to-persist (remove api/delete? results)
        to-delete  (filter api/delete? entities)]
    (-> (js/Promise.all
          (clj->js (cond-> []
                     (seq to-persist) (conj (idb/put-entities @(.-idb-atom db) to-persist))
                     (seq to-delete)  (into (map #(idb/delete-entity @(.-idb-atom db) (:kind %) (:id %)) to-delete)))))
        (.then (fn [_]
                 (reset! (.-store db) new-store)
                 results)))))

;endregion

;region Protocol Implementation

(deftype IndexedDB [legend store idb-atom db-name]
  api/DB
  (-clear [this]
    (memory/clear this)
    (idb/clear-all @idb-atom))
  (close [_this] (idb/close @idb-atom))
  (-count [this kind options] (core-count (api/-find this kind options)))
  (-delete-all [this kind]
    (memory/delete-all this kind)
    (idb/clear-store @idb-atom kind))
  (-entity [this kind id] (memory/entity this kind id))
  (-find [this kind options] (memory/do-find this kind options))
  (-reduce [this kind f init options] (core-reduce f init (api/-find this kind options)))
  (-tx [this entity] (idb-tx this entity))
  (-tx* [this entities] (idb-tx* this entities))
  migrator/Migrator
  (-schema-exists? [_this schema] (contains? @legend (api/-schema-kind schema)))
  (-installed-schema-legend [_this _expected-legend] @legend)
  (-install-schema! [this schema] (memory/do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [_this kind attr spec] (swap! legend assoc-in [kind attr] spec))
  (-remove-attribute! [this kind attr] (memory/do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (memory/do-rename-attribute! this kind attr new-kind new-attr)))

;endregion

;region Public API

(defn init!
  "Opens the IDB database and optionally rehydrates the in-memory store.
   Must be called and awaited before using the db.
   Returns a js/Promise resolving to the db instance."
  [db & kinds]
  (-> (idb/open (.-db-name db) @(.-legend db))
      (.then (fn [idb-instance]
               (reset! (.-idb-atom db) idb-instance)
               db))
      (.then (fn [db]
               (if (seq kinds)
                 (idb/rehydrate @(.-idb-atom db) kinds (fn [entities] (memory/tx* db entities)))
                 (idb/rehydrate @(.-idb-atom db) nil (fn [entities] (memory/tx* db entities))))))
      (.then (fn [_] db))))

;endregion

;region Registration

(defmethod api/-create-impl :indexeddb [config schemas]
  (let [legend   (atom (legend/build schemas))
        store    (or (:store config) (atom {}))
        idb-atom (atom nil)
        db-name  (or (:db-name config) "c3kit-bucket")]
    (IndexedDB. legend store idb-atom db-name)))

;endregion
```

- [ ] **Step 4: Run tests to verify compilation**

Run: `clj -M:test:cljs once`
Expected: Existing tests PASS, new test FAIL (need to handle async in tests)

- [ ] **Step 5: Investigate async test patterns in speclj ClojureScript**

Check how speclj handles async tests. Look for patterns in existing specs or speclj docs. If no built-in async support, we'll write a synchronous test helper that uses `init!` in a before block and tests reads synchronously after writes resolve.

**Pragmatic test approach:** Since IDB writes are near-instant in test environments, and the shared `impl_spec` tests call `tx` then immediately read via `entity`/`find-by`, we need the promise to resolve before the synchronous read. This won't work with raw promises.

**Solution:** For testing, provide a synchronous test mode where `tx` writes to memory immediately (like `MemoryDB`) AND fires the IDB write in the background. Tests verify the in-memory state synchronously. A separate set of IDB-specific tests verifies persistence by rehydrating after writes.

Actually — the simplest approach is: the existing `impl_spec` tests won't work directly with async `tx`. Instead, write **IDB-specific tests** that test the persistence guarantees (write → rehydrate → data is there), and keep the `impl_spec` for the Memory/ReMemory impls.

- [ ] **Step 5 (revised): Write IDB-specific persistence tests**

```clojure
;; spec/cljs/c3kit/bucket/indexeddb_spec.cljs
(ns c3kit.bucket.indexeddb-spec
  (:require-macros [speclj.core :refer [around-all before context describe it should should-be-nil should= with]])
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.indexeddb :as sut]
            [c3kit.bucket.idb :as idb]
            [c3kit.apron.schema :as s]
            [speclj.core]))

(def bibelot
  {:kind  (s/kind :bibelot)
   :id    {:type :long}
   :name  {:type :string}
   :size  {:type :long}
   :color {:type :string}})

(def thingy
  {:kind    (s/kind :thingy)
   :id      {:type :int}
   :name    {:type :string}
   :foo     {:type :string}})

(describe "IndexedDB"

  ;; Since tx returns promises, these tests verify persistence by:
  ;; 1. Writing via tx (returns promise)
  ;; 2. After promise resolves, checking in-memory store (synchronous)
  ;; 3. Clearing memory and rehydrating from IDB to verify persistence

  (context "persistence"

    (it "persists and rehydrates a single entity"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-persist-1"} [bibelot])]
        (-> (sut/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :name "widget" :size 5})))
            (.then (fn [saved]
                     ;; Verify in-memory
                     (should= "widget" (:name saved))
                     ;; Clear memory, rehydrate from IDB
                     (reset! (.-store db) {:all {}})
                     (sut/init! db)))
            (.then (fn [db]
                     (let [found (api/find-by- db :bibelot :name "widget")]
                       (should= 1 (count found))
                       (should= "widget" (:name (first found))))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-persist-1"))))))

    (it "persists and rehydrates multiple entities"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-persist-2"} [bibelot thingy])]
        (-> (sut/init! db)
            (.then (fn [db]
                     (api/-tx* db [{:kind :bibelot :name "widget" :size 5}
                                   {:kind :thingy :name "gadget" :foo "bar"}])))
            (.then (fn [_]
                     (reset! (.-store db) {:all {}})
                     (sut/init! db)))
            (.then (fn [db]
                     (should= 1 (count (api/find-by- db :bibelot :name "widget")))
                     (should= 1 (count (api/find-by- db :thingy :name "gadget")))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-persist-2"))))))

    (it "deletes entity from IDB"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-persist-3"} [bibelot])]
        (-> (sut/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :name "widget" :size 5})))
            (.then (fn [saved]
                     (api/-tx db (assoc saved :db/delete? true))))
            (.then (fn [_]
                     (reset! (.-store db) {:all {}})
                     (sut/init! db)))
            (.then (fn [db]
                     (should= 0 (count (api/find-by- db :bibelot :name "widget")))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-persist-3"))))))

    (it "rehydrates only specified kinds"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-persist-4"} [bibelot thingy])]
        (-> (sut/init! db)
            (.then (fn [db]
                     (api/-tx* db [{:kind :bibelot :name "widget"}
                                   {:kind :thingy :name "gadget"}])))
            (.then (fn [_]
                     (reset! (.-store db) {:all {}})
                     (sut/init! db :bibelot)))
            (.then (fn [db]
                     (should= 1 (count (api/find-by- db :bibelot :name "widget")))
                     (should= 0 (count (api/find-by- db :thingy :name "gadget")))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-persist-4"))))))
    )
  )
```

- [ ] **Step 6: Run tests**

Run: `clj -M:test:cljs once`
Expected: Tests compile and run. Pass/fail depends on async handling in speclj.

- [ ] **Step 7: Debug and iterate until tests pass**

This step may require adjusting the test setup based on how speclj handles promises in ClojureScript. The key things to investigate:
- Does speclj support returning promises from `it` blocks?
- If not, use `speclj.core/with-stubs` and `speclj.core/should-eventually` or a callback-based approach.

- [ ] **Step 8: Commit**

```bash
git add src/cljs/c3kit/bucket/indexeddb.cljs spec/cljs/c3kit/bucket/indexeddb_spec.cljs
git commit -m "Add :indexeddb protocol implementation with persistence tests"
```

---

### Task 5: ReIndexedDB Protocol Implementation (`:re-indexeddb` — reagent/atom, reactive)

**Files:**
- Create: `src/cljs/c3kit/bucket/re_indexeddb.cljs`
- Create: `spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs`

- [ ] **Step 1: Write failing test — basic persistence with reactivity**

```clojure
;; spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs
(ns c3kit.bucket.re-indexeddb-spec
  (:require-macros [speclj.core :refer [around-all before context describe it should should= with]]
                   [c3kit.bucket.api :refer [with-safety-off]])
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.re-indexeddb :as sut]
            [c3kit.bucket.idb :as idb]
            [c3kit.apron.schema :as s]
            [reagent.core :as r]
            [speclj.core]))

(def bibelot
  {:kind  (s/kind :bibelot)
   :id    {:type :long}
   :name  {:type :string}
   :size  {:type :long}
   :color {:type :string}})

(describe "ReIndexedDB"

  (context "persistence"

    (it "persists and rehydrates with reagent reactivity"
      (let [db (api/create-db {:impl :re-indexeddb :db-name "test-re-persist-1"} [bibelot])]
        (-> (sut/init! db)
            (.then (fn [db]
                     (api/-tx db {:kind :bibelot :name "widget" :size 5})))
            (.then (fn [saved]
                     (should= "widget" (:name saved))
                     ;; Verify the store is a reagent atom (reactive)
                     (should (instance? reagent.ratom/RAtom (.-store db)))
                     ;; Clear memory, rehydrate from IDB
                     (reset! (.-store db) {})
                     (sut/init! db)))
            (.then (fn [db]
                     (let [found (api/find-by- db :bibelot :name "widget")]
                       (should= 1 (count found))
                       (should= "widget" (:name (first found))))
                     (api/close db)
                     (.deleteDatabase js/indexedDB "test-re-persist-1"))))))
    )
  )
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — namespace not found

- [ ] **Step 3: Implement ReIndexedDB deftype**

```clojure
;; src/cljs/c3kit/bucket/re_indexeddb.cljs
(ns c3kit.bucket.re-indexeddb
  (:refer-clojure :rename {find core-find count core-count reduce core-reduce})
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb :as idb]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as migrator]
            [c3kit.bucket.re-memory :as re-memory]
            [reagent.core :as r]))

;region Helpers

(defn- prepare-entity [db entity]
  (let [entity    (memory/ensure-id entity)
        new-store (memory/tx-entity @(.-legend db) @(.-store db) entity)
        result    (if (api/delete? entity)
                    (api/soft-delete entity)
                    (get-in new-store [:all (:id entity)]))]
    [new-store result]))

(defn- idb-tx [db entity]
  (let [[new-store result] (prepare-entity db entity)]
    (if (api/delete? entity)
      (-> (idb/delete-entity @(.-idb-atom db) (:kind entity) (:id entity))
          (.then (fn [_]
                   (reset! (.-store db) new-store)
                   result)))
      (-> (idb/put-entity @(.-idb-atom db) result)
          (.then (fn [_]
                   (reset! (.-store db) new-store)
                   result))))))

(defn- idb-tx* [db entities]
  (let [entities  (map memory/ensure-id entities)
        new-store (clojure.core/reduce #(memory/tx-entity @(.-legend db) %1 %2) @(.-store db) entities)
        results   (mapv (fn [e]
                          (if (api/delete? e)
                            (api/soft-delete e)
                            (get-in new-store [:all (:id e)])))
                        entities)
        to-persist (remove api/delete? results)
        to-delete  (filter api/delete? entities)]
    (-> (js/Promise.all
          (clj->js (cond-> []
                     (seq to-persist) (conj (idb/put-entities @(.-idb-atom db) to-persist))
                     (seq to-delete)  (into (map #(idb/delete-entity @(.-idb-atom db) (:kind %) (:id %)) to-delete)))))
        (.then (fn [_]
                 (reset! (.-store db) new-store)
                 results)))))

;endregion

;region Protocol Implementation

(deftype ReIndexedDB [legend store idb-atom db-name]
  api/DB
  (-clear [this]
    (memory/clear this)
    (idb/clear-all @idb-atom))
  (close [_this] (idb/close @idb-atom))
  (-count [this kind options] (core-count (api/-find this kind options)))
  (-delete-all [this kind]
    (memory/delete-all this kind)
    (idb/clear-store @idb-atom kind))
  (-entity [this kind id] (re-memory/entity this kind id))
  (-find [this kind options] (re-memory/do-find this kind options))
  (-reduce [this kind f init options] (core-reduce f init (api/-find this kind options)))
  (-tx [this entity] (idb-tx this entity))
  (-tx* [this entities] (idb-tx* this entities))
  migrator/Migrator
  (-schema-exists? [_this schema] (contains? @legend (api/-schema-kind schema)))
  (-installed-schema-legend [_this _expected-legend] @legend)
  (-install-schema! [this schema] (memory/do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [_this kind attr spec] (swap! legend assoc-in [kind attr] spec))
  (-remove-attribute! [this kind attr] (memory/do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (memory/do-rename-attribute! this kind attr new-kind new-attr)))

;endregion

;region Public API

(defn init!
  "Opens the IDB database and optionally rehydrates the in-memory store.
   Must be called and awaited before using the db.
   Returns a js/Promise resolving to the db instance."
  [db & kinds]
  (-> (idb/open (.-db-name db) @(.-legend db))
      (.then (fn [idb-instance]
               (reset! (.-idb-atom db) idb-instance)
               db))
      (.then (fn [db]
               (idb/rehydrate @(.-idb-atom db) (seq kinds)
                              (fn [entities] (memory/tx* db entities)))))
      (.then (fn [_] db))))

;endregion

;region Registration

(defmethod api/-create-impl :re-indexeddb [config schemas]
  (let [legend   (atom (legend/build schemas))
        store    (or (:store config) (r/atom {}))
        idb-atom (atom nil)
        db-name  (or (:db-name config) "c3kit-bucket")]
    (ReIndexedDB. legend store idb-atom db-name)))

;endregion
```

- [ ] **Step 4: Run tests**

Run: `clj -M:test:cljs once`
Expected: Tests compile. Persistence test should pass if async is handled correctly.

- [ ] **Step 5: Debug and iterate until tests pass**

- [ ] **Step 6: Commit**

```bash
git add src/cljs/c3kit/bucket/re_indexeddb.cljs spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs
git commit -m "Add :re-indexeddb protocol implementation with Reagent reactivity"
```

---

### Task 6: Expose `memory/do-find` as Public

**Files:**
- Modify: `src/cljc/c3kit/bucket/memory.cljc`

The `do-find` function in `memory.cljc` is private but both `IndexedDB` and `ReIndexedDB` need it for reads. The `ReMemoryDB` already has its own `do-find`, but `IndexedDB` needs memory's version.

- [ ] **Step 1: Check if `do-find` is already accessible**

Check `memory.cljc` line 136 — `do-find` is `defn-` (private). The `IndexedDB` deftype references `memory/do-find` which won't work for a private fn from another namespace.

- [ ] **Step 2: Make `do-find` public**

```clojure
;; In src/cljc/c3kit/bucket/memory.cljc, change line 136 from:
(defn- do-find [db kind options]
;; to:
(defn do-find [db kind options]
```

- [ ] **Step 3: Run all tests to verify nothing breaks**

Run: `clj -M:test:cljs once` and `clj -M:test:spec`
Expected: All PASS

- [ ] **Step 4: Commit**

```bash
git add src/cljc/c3kit/bucket/memory.cljc
git commit -m "Make memory/do-find public for use by IndexedDB impls"
```

---

### Task 7: Expose `re-memory/do-find` and `re-memory/entity` for ReIndexedDB

**Files:**
- Modify: `src/cljs/c3kit/bucket/re_memory.cljs`

The `ReIndexedDB` deftype calls `re-memory/entity` (already public) and `re-memory/do-find` (private). We need `do-find` to be accessible.

- [ ] **Step 1: Check `re-memory/do-find` visibility**

Line 56 of `re_memory.cljs` — `do-find` is `defn-` (private).

- [ ] **Step 2: Make `do-find` public**

```clojure
;; In src/cljs/c3kit/bucket/re_memory.cljs, change line 56 from:
(defn- do-find [db kind options]
;; to:
(defn do-find [db kind options]
```

- [ ] **Step 3: Run all tests**

Run: `clj -M:test:cljs once`
Expected: All PASS

- [ ] **Step 4: Commit**

```bash
git add src/cljs/c3kit/bucket/re_memory.cljs
git commit -m "Make re-memory/do-find public for use by ReIndexedDB"
```

---

### Task 8: Serialization — Handle Clojure Data Types in IDB

**Files:**
- Modify: `src/cljs/c3kit/bucket/idb.cljs`
- Modify: `spec/cljs/c3kit/bucket/idb_spec.cljs`

IndexedDB stores JavaScript objects. Clojure keywords, sets, and other types need proper serialization/deserialization.

- [ ] **Step 1: Write failing test — round-trip keyword values**

```clojure
;; Add to idb_spec.cljs

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

  (it "round-trips entities with nil values"
    (let [entity {:id 4 :kind :bibelot :name nil :size 5}]
      (should= entity (sut/js->clj-entity (sut/clj->js-entity entity)))))
  )
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clj -M:test:cljs once`
Expected: FAIL — either functions not public or round-trip fails due to keyword handling

- [ ] **Step 3: Implement robust serialization**

The naive `clj->js` / `js->clj` won't preserve keywords in values (e.g., `:letters [:a :b :c]`). We need to use transit or EDN for the entity body.

```clojure
;; Replace clj->js-entity and js->clj-entity in src/cljs/c3kit/bucket/idb.cljs

(defn clj->js-entity
  "Converts a Clojure entity to an IDB-storable JS object.
   Stores as {id: <id>, data: <EDN string>} to preserve Clojure types."
  [entity]
  #js {:id (:id entity) :data (pr-str entity)})

(defn js->clj-entity
  "Converts an IDB-stored JS object back to a Clojure entity."
  [js-obj]
  (when js-obj
    (cljs.reader/read-string (.-data js-obj))))
```

Add `cljs.reader` to the require:

```clojure
(ns c3kit.bucket.idb
  (:require [cljs.reader]))
```

- [ ] **Step 4: Run tests to verify round-trips pass**

Run: `clj -M:test:cljs once`
Expected: All serialization tests PASS

- [ ] **Step 5: Commit**

```bash
git add src/cljs/c3kit/bucket/idb.cljs spec/cljs/c3kit/bucket/idb_spec.cljs
git commit -m "Add EDN serialization for IDB entity storage"
```

---

### Task 9: Integration Testing — Full Workflow

**Files:**
- Modify: `spec/cljs/c3kit/bucket/indexeddb_spec.cljs`
- Modify: `spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs`

- [ ] **Step 1: Add tests for edge cases**

Add to `indexeddb_spec.cljs`:

```clojure
(context "edge cases"

  (it "handles tx of entity with no id (generates one)"
    (let [db (api/create-db {:impl :indexeddb :db-name "test-edge-1"} [bibelot])]
      (-> (sut/init! db)
          (.then (fn [db]
                   (api/-tx db {:kind :bibelot :name "auto-id"})))
          (.then (fn [saved]
                   (should (some? (:id saved)))
                   (should= "auto-id" (:name saved))
                   (api/close db)
                   (.deleteDatabase js/indexedDB "test-edge-1"))))))

  (it "handles tx* with mix of creates and deletes"
    (let [db (api/create-db {:impl :indexeddb :db-name "test-edge-2"} [bibelot])]
      (-> (sut/init! db)
          (.then (fn [db]
                   (api/-tx db {:kind :bibelot :name "keeper" :size 1})))
          (.then (fn [keeper]
                   (api/-tx* db [{:kind :bibelot :name "new-one" :size 2}
                                 (assoc keeper :db/delete? true)])))
          (.then (fn [_]
                   (reset! (.-store db) {:all {}})
                   (sut/init! db)))
          (.then (fn [db]
                   (should= 1 (count (api/find-by- db :bibelot :name "new-one")))
                   (should= 0 (count (api/find-by- db :bibelot :name "keeper")))
                   (api/close db)
                   (.deleteDatabase js/indexedDB "test-edge-2"))))))

  (it "updates existing entity in IDB"
    (let [db (api/create-db {:impl :indexeddb :db-name "test-edge-3"} [bibelot])]
      (-> (sut/init! db)
          (.then (fn [db]
                   (api/-tx db {:kind :bibelot :name "widget" :size 5})))
          (.then (fn [saved]
                   (api/-tx db (assoc saved :size 10))))
          (.then (fn [_]
                   (reset! (.-store db) {:all {}})
                   (sut/init! db)))
          (.then (fn [db]
                   (let [found (first (api/find-by- db :bibelot :name "widget"))]
                     (should= 10 (:size found)))
                   (api/close db)
                   (.deleteDatabase js/indexedDB "test-edge-3"))))))
  )
```

- [ ] **Step 2: Run all tests**

Run: `clj -M:test:cljs once`
Expected: All PASS

- [ ] **Step 3: Commit**

```bash
git add spec/cljs/c3kit/bucket/indexeddb_spec.cljs spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs
git commit -m "Add integration and edge case tests for IndexedDB impls"
```

---

### Task 10: Convenience `rehydrate!` via `api/impl`

**Files:**
- Modify: `src/cljs/c3kit/bucket/indexeddb.cljs`
- Modify: `src/cljs/c3kit/bucket/re_indexeddb.cljs`

Both impls expose `init!` that opens IDB and rehydrates. But after initial setup, a user may want to re-rehydrate (e.g., after clearing memory). Add a standalone `rehydrate!` that works on an already-opened db.

- [ ] **Step 1: Add `rehydrate!` to `idb.cljs` that works on an already-open connection**

This is already handled by the `rehydrate` function in `idb.cljs`. The `init!` function in each impl calls it. No additional work needed — `init!` already handles reopening if called again.

Actually, let's provide a `rehydrate!` on each impl that doesn't re-open IDB:

```clojure
;; Add to both indexeddb.cljs and re_indexeddb.cljs

(defn rehydrate!
  "Rehydrates the in-memory store from IDB. Use after init! has been called.
   Call with no kinds to load all, or specific kinds to load selectively.
   Returns a js/Promise."
  [db & kinds]
  (-> (idb/rehydrate @(.-idb-atom db) (seq kinds)
                     (fn [entities] (memory/tx* db entities)))
      (.then (fn [_] db))))
```

- [ ] **Step 2: Run all tests**

Run: `clj -M:test:cljs once`
Expected: All PASS

- [ ] **Step 3: Commit**

```bash
git add src/cljs/c3kit/bucket/indexeddb.cljs src/cljs/c3kit/bucket/re_indexeddb.cljs
git commit -m "Add standalone rehydrate! for already-opened IDB connections"
```

---

### Summary of Key Design Decisions in Implementation

1. **`create-db` is synchronous, `init!` is async.** `create-db` returns the deftype immediately. The caller must `(.then ...)` on `init!` to open IDB and rehydrate before using the db.

2. **Writes are IDB-first.** `prepare-entity` computes what the new store would look like, writes to IDB, then applies to the in-memory store on success. Uses `reset!` rather than `swap!` to apply the pre-computed store snapshot.

3. **Reads delegate to Memory/ReMemory.** `IndexedDB` uses `memory/do-find` and `memory/entity`. `ReIndexedDB` uses `re-memory/do-find` and `re-memory/entity` for Reagent reactivity.

4. **EDN serialization.** Entities are stored in IDB as `{id: <id>, data: <EDN string>}` to preserve Clojure types (keywords, sets, etc.).

5. **Schema versioning via localStorage.** A hash of the legend is compared to localStorage on each open. Version increments trigger `onupgradeneeded` to reconcile object stores.

6. **`do-find` made public on both memory and re-memory.** Required for the IndexedDB impls to delegate reads.
