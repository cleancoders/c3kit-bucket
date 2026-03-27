# Synchronous IndexedDB Test Redesign

## Problem

Speclj is purely synchronous. The IndexedDB test specs put `should=` assertions inside
`.then` Promise callbacks, but speclj's `*assertions*` dynamic binding is restored to
`undefined` before those callbacks fire. This means:

1. Assertions inside `.then` throw `No protocol method ISwap.-swap!` errors
2. Tests report "pass" before async assertions execute (false positives)
3. Assertion failures inside Promises become uncaught browser errors, not test failures

## Approach

**Approach C: Extract Pure Functions + Test Orchestration via Atoms**

- Keep production code async with real Promises (no changes for testability)
- Test synchronous memory effects directly in speclj (using `idb-atom = nil` to skip IDB writes)
- Add a small `cljs.test` integration test suite for end-to-end IDB verification

## Synchronous Unit Tests (speclj)

### Key Insight

`idb-tx` and `idb-tx*` update the in-memory store synchronously, then persist to IDB
only `when-let [idb @(.-idb-atom db)]`. With a nil `idb-atom`, they are purely synchronous.
Similarly, `refresh!` and `sync-complete!` perform their memory operations synchronously.

### idb_common_spec.cljs

| Current Test | Action |
|---|---|
| 4 dirty set tests (io/open, read/write/accumulate/remove) | **Delete** — covered by integration tests |
| sync! with nil idb-atom | **Restructure** — assert callback atom state directly, no `.then` |
| 4 ensure-offline-id tests | **Keep as-is** (already synchronous) |
| 2 offline-ensure-id tests | **Keep as-is** (already synchronous) |

### indexeddb_spec.cljs

| Current Test | Action |
|---|---|
| 6 persistence tests (init/tx/rehydrate) | **Delete** — covered by integration tests |
| 2 online-fn tests | **Keep as-is** (already synchronous) |
| 7 offline tx tests | **Restructure** — nil idb-atom, assert memory state: negative ID assignment, entity in memory, tombstone creation, batch IDs |
| 3 sync lifecycle tests | **Split** — memory effects (soft-delete neg IDs, tx server entities) tested synchronously with nil idb-atom; IDB dirty-set verification moves to integration |
| 2 refresh! tests | **Restructure** — nil idb-atom makes refresh! fully synchronous |
| 2 rollback tests | **Delete** — inherently async, covered by integration tests |

### re_indexeddb_spec.cljs

| Current Test | Action |
|---|---|
| reagent atom test | **Keep as-is** |
| 4 persistence tests | **Delete** — covered by integration tests |
| 2 online-fn tests | **Keep as-is** (already synchronous) |
| 2 offline tx tests | **Restructure** — nil idb-atom, assert memory state |
| 2 rollback tests | **Delete** — covered by integration tests |

### idb_io_spec.cljs

| Current Test | Action |
|---|---|
| 2 schema-hash tests | **Keep as-is** (already synchronous) |
| 3 idb-version tests | **Keep as-is** (already synchronous) |
| 4 serialization tests | **Keep as-is** (already synchronous) |
| 2 read-entity tests | **Delete** — covered by integration tests |

### Summary

- ~15 async tests deleted (moved to integration)
- ~8 async tests restructured to synchronous (nil idb-atom pattern)
- ~10 tests unchanged (already synchronous)

## Integration Tests (cljs.test)

### Location

`spec/cljs/c3kit/bucket/idb_integration_test.cljs`

### Runner Setup

Separate alias and build configuration, fully independent from the speclj suite.

**deps.edn alias:**
```clojure
:idb-integration {:main-opts ["-m" "c3kit.scaffold.cljs"]
                  :extra-paths ["spec/cljs"]}
```

**Build config:** `dev/config/cljs-integration.edn` — a dedicated CLJS build config
that compiles only what's needed for the integration tests. Uses a separate output
file and a custom HTML page that runs `cljs.test/run-tests` instead of speclj.

**Custom HTML page:** A hand-written HTML page (e.g., `dev/idb-integration.html`)
that loads the compiled JS and calls the `cljs.test` runner for the integration
test namespace. This page is loaded by Playwright via scaffold.

**Command:** `clj -M:test:idb-integration`

**CI:** Add as a separate step in `.github/workflows/test.yml`.

Use `cljs.test/async` with a `done` callback for Promise-based tests. Each test
creates a unique IndexedDB database and deletes it at the end.

### Test Suite (~5 tests)

**1. Persistence round-trip**
- Init DB, tx an entity, clear memory, rehydrate, verify entity present
- Covers: `init!`, `idb-tx`, `rehydrate!`, full IO read/write pipeline

**2. Dirty set round-trip**
- Open IDB, add to dirty set, read back, remove entries, read again
- Covers: `add-to-dirty-set!`, `remove-from-dirty-set!`, `read-dirty-set`, `dirty-chain` serialization

**3. Offline tx + sync lifecycle**
- Create DB offline, tx entities (negative IDs), verify dirty set populated
- Call `sync!` to get dirty entities via callback
- Call `sync-complete!` with server replacements
- Verify dirty set cleared and server entities persisted in IDB
- Covers: full offline-to-online sync flow

**4. Refresh**
- Create DB, tx offline + server entities, call `refresh!` with new server data
- Rehydrate from IDB, verify negative-ID entities gone and server data replaced
- Covers: `refresh!`, `purge-neg-entities-from-memory!`, `sync-idb-after-refresh!`

**5. Rollback on IDB failure**
- Tx an entity, point `idb-atom` at broken mock, tx another entity
- Wait briefly, verify memory rolled back to pre-failure state
- Covers: `with-rollback`, optimistic update recovery

### Test Pattern

```clojure
(deftest persistence-round-trip
  (async done
    (let [db (api/create-db {:impl :indexeddb :db-name "integration-1"} [bibelot])]
      (-> (idb/init! db)
          (.then (fn [db] (api/-tx db {:kind :bibelot :name "widget" :size 5})))
          (.then (fn [_]
                   (reset! (.-store db) {:all {}})
                   (idb/rehydrate! db)))
          (.then (fn [db]
                   (is (= 1 (count (api/find-by- db :bibelot :name "widget"))))
                   (api/close db)
                   (.deleteDatabase js/indexedDB "integration-1")))
          (.then (fn [_] (done)))
          (.catch (fn [e] (is (nil? e) (str "Unexpected error: " e)) (done)))))))
```

Each test uses a unique DB name and deletes it at the end. A `.catch` at the end
ensures `done` always fires so the runner does not hang.

## Production Code Changes

None. All production code remains async with real Promises. The synchronous tests
exploit the existing `when-let [idb @(.-idb-atom db)]` guard — with nil `idb-atom`,
the async IDB operations are simply skipped.

## Coverage Analysis

| Concern | Unit Tests (speclj) | Integration Tests (cljs.test) |
|---|---|---|
| Offline ID generation | Yes | Yes (indirectly) |
| Optimistic memory update | Yes | Yes (indirectly) |
| Entity coercion / soft-delete | Yes | No |
| Tombstone creation | Yes | Yes (sync lifecycle) |
| Rollback on IDB failure | No | Yes |
| Dirty set logic | No | Yes |
| Persistence round-trip | No | Yes |
| Sync lifecycle (full) | Partial (memory effects) | Yes |
| Refresh (full) | Yes (memory effects) | Yes (with IDB) |
| IO serialization | Yes (in idb-io-spec) | Yes (indirectly) |
| ReIndexedDB reagent atom | Yes | No (same IDB plumbing) |
