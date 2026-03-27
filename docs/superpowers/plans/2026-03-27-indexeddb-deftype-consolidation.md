# IndexedDB Deftype Consolidation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Merge the nearly-identical `IndexedDB` and `ReIndexedDB` deftypes into a single `IndexedDB` deftype with injected `entity-fn` and `find-fn`.

**Architecture:** The `IndexedDB` deftype gains two function fields (`entity-fn`, `find-fn`). The `:indexeddb` factory injects `memory/entity` + `memory/do-find`; the `:re-indexeddb` factory (in its own file) injects `re-memory/entity` + `re-memory/do-find`. `re_indexeddb.cljs` shrinks from a full deftype to a thin factory.

**Tech Stack:** ClojureScript, speclj

---

## File Structure

- **Modify:** `src/cljs/c3kit/bucket/indexeddb.cljs` — add `entity-fn` and `find-fn` fields to deftype, update factory
- **Rewrite:** `src/cljs/c3kit/bucket/re_indexeddb.cljs` — remove deftype, keep only factory function that imports `IndexedDB`
- **No changes needed** to test files — both specs should pass as-is since the consumer API (`:impl` keywords, config keys, field accessors) is unchanged

### Task 1: Update `IndexedDB` deftype to accept injected functions

**Files:**
- Modify: `src/cljs/c3kit/bucket/indexeddb.cljs:12-46`

- [ ] **Step 1: Run the existing indexeddb tests to confirm they pass**

Run: `clj -M:test:cljs once`

Expected: All `IndexedDB` and `ReIndexedDB` tests pass.

- [ ] **Step 2: Add `entity-fn` and `find-fn` fields to the `IndexedDB` deftype**

In `src/cljs/c3kit/bucket/indexeddb.cljs`, change the deftype from:

```clojure
(deftype IndexedDB [legend store idb-atom db-name online-fn]
  api/DB
  ...
  (-entity [this kind id] (memory/entity this kind id))
  (-find [this kind options] (memory/do-find this kind options))
  ...
```

to:

```clojure
(deftype IndexedDB [legend store idb-atom db-name online-fn entity-fn find-fn]
  api/DB
  ...
  (-entity [this kind id] (entity-fn this kind id))
  (-find [this kind options] (find-fn this kind options))
  ...
```

All other methods stay exactly the same.

- [ ] **Step 3: Update the `:indexeddb` factory to pass `memory/entity` and `memory/do-find`**

In the same file, change the factory from:

```clojure
(defmethod api/-create-impl :indexeddb [config schemas]
  (let [legend   (atom (legend/build schemas))
        store    (or (:store config) (atom {}))
        idb-atom (atom nil)
        db-name    (or (:db-name config) "c3kit-bucket")
        online-fn  (or (:online? config) (constantly true))]
    (IndexedDB. legend store idb-atom db-name online-fn)))
```

to:

```clojure
(defmethod api/-create-impl :indexeddb [config schemas]
  (let [legend    (atom (legend/build schemas))
        store     (or (:store config) (atom {}))
        idb-atom  (atom nil)
        db-name   (or (:db-name config) "c3kit-bucket")
        online-fn (or (:online? config) (constantly true))]
    (IndexedDB. legend store idb-atom db-name online-fn memory/entity memory/do-find)))
```

- [ ] **Step 4: Run the indexeddb tests to confirm they still pass**

Run: `clj -M:test:cljs once`

Expected: All `IndexedDB` tests pass. `ReIndexedDB` tests will fail (the `ReIndexedDB` deftype still has 5 fields but that's fine — we fix it in Task 2).

- [ ] **Step 5: Commit**

```bash
git add src/cljs/c3kit/bucket/indexeddb.cljs
git commit -m "add entity-fn and find-fn fields to IndexedDB deftype"
```

### Task 2: Replace `ReIndexedDB` deftype with thin factory

**Files:**
- Rewrite: `src/cljs/c3kit/bucket/re_indexeddb.cljs`

- [ ] **Step 1: Replace `re_indexeddb.cljs` with a thin factory**

Replace the entire contents of `src/cljs/c3kit/bucket/re_indexeddb.cljs` with:

```clojure
(ns c3kit.bucket.re-indexeddb
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.indexeddb :refer [IndexedDB]]
            [c3kit.bucket.re-memory :as re-memory]
            [reagent.core :as r]))

(defmethod api/-create-impl :re-indexeddb [config schemas]
  (let [legend    (atom (legend/build schemas))
        store     (or (:store config) (r/atom {}))
        idb-atom  (atom nil)
        db-name   (or (:db-name config) "c3kit-bucket")
        online-fn (or (:online? config) (constantly true))]
    (IndexedDB. legend store idb-atom db-name online-fn re-memory/entity re-memory/do-find)))
```

- [ ] **Step 2: Run all tests to confirm both IndexedDB and ReIndexedDB specs pass**

Run: `clj -M:test:cljs once`

Expected: All tests pass. The `ReIndexedDB` spec creates instances via `api/create-db` with `{:impl :re-indexeddb}`, which hits the factory in this file and constructs an `IndexedDB` instance with `re-memory` functions injected.

- [ ] **Step 3: Commit**

```bash
git add src/cljs/c3kit/bucket/re_indexeddb.cljs
git commit -m "replace ReIndexedDB deftype with thin factory over shared IndexedDB"
```
