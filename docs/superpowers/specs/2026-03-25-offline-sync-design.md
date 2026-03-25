# Offline Sync for c3kit-bucket IndexedDB

## Overview

Adds offline sync capabilities to bucket's existing `:indexeddb` and `:re-indexeddb` implementations. When an app provides an `:online?` callback in the config, bucket automatically tracks dirty entities (creates, updates, deletes made while offline) and provides an API for syncing them to the server. A lightweight service-worker-compatible reader module enables background sync from the SW context.

This is a configuration option on the existing impls, not new impls. Apps that don't provide `:online?` get today's pure-cache behavior unchanged.

## Config

```clojure
{:impl     :re-indexeddb
 :online?  #(and (online/navigator-online?) @ws/open?)  ;; optional
 :store    (reagent/atom nil)
 :db-name  "wilson"}
```

- `:online?` — A zero-arg callback returning truthy/falsy. Bucket calls it to determine online/offline status. When absent, defaults to `(constantly true)` (pure cache, no sync).

## Dirty Tracking

### Dirty Set

The single source of truth for which entities need sync. Stored in the `_meta` IDB object store as:

```clojure
{:id "dirty" :data #{-1 -2 12345 67890}}
```

Serialized identically to regular entities (`{id: "dirty", data: "(pr-str #{...})"}`).

An entity is dirty if and only if its ID is in the dirty set. There are no other dirty signals — negative IDs, tombstones, and metadata flags are not checked independently.

### Dirty Set Operations

- **Add ID** — Read current set, `conj` the ID, write back. Done in the same IDB transaction as the entity write for atomicity.
- **Remove ID** — Read current set, `disj` the ID, write back.
- **Read all** — Fetch the `"dirty"` entry from `_meta`, deserialize.
- **Clear specific IDs** — Read, `disj` all provided IDs, write back. Not a full clear, since new entities may become dirty during an in-flight sync.

## Negative ID Generation

When offline and an entity is saved without an `:id`, bucket assigns a negative ID from a decrementing counter (atom starting at -1). Sequential negatives eliminate collision risk (unlike Wilson's current random negatives).

When online (or no `:online?` callback), `memory/ensure-id` assigns positive IDs as today.

Negative IDs serve as a convention that the entity was created offline and has no server-side counterpart, but they are NOT the mechanism for dirty tracking. The dirty set in `_meta` is the sole source of truth.

## Offline-Aware Transaction Behavior

### When Online (or no `:online?` callback)

Unchanged from current behavior:
- `db/tx` — Optimistic memory write, background IDB persist, rollback on failure.
- `db/delete` — Remove from memory, remove from IDB.
- Dirty set is not touched.

### When Offline

**Create** (entity has no `:id`):
1. Assign negative ID from decrementing counter
2. Write to in-memory store
3. Persist to IDB
4. Add ID to dirty set (same IDB transaction)

**Update** (entity has positive `:id`):
1. Write to in-memory store
2. Persist to IDB (overwrites cached version with modified version)
3. Add ID to dirty set (same IDB transaction)

**Delete** of a server-known entity (positive `:id`):
1. Remove from in-memory store (UI updates immediately)
2. Persist tombstone to IDB: `{:kind k :id i :db/delete? true}`
3. Add ID to dirty set (same IDB transaction)

**Delete** of an offline-created entity (negative `:id`):
1. Remove from in-memory store
2. Remove from IDB
3. Remove ID from dirty set

The last case is an optimization: no point syncing a create+delete the server never knew about.

## Sync Lifecycle

### Triggering Sync

Bucket does not decide when to sync. The app triggers sync (on reconnect, on SW sync event, etc.) by calling:

```clojure
(idb/sync! db callback)
```

### `sync!` Behavior

1. Read dirty set from `_meta`
2. Fetch each dirty entity from IDB
3. Pass the vector of dirty entities to `callback`

Entities are a mix of:
- Normal entities (creates and updates) — distinguishable by positive or negative ID
- Tombstones — distinguishable via `(api/delete? entity)` which checks `:db/delete? true`

**Dirty data is NOT cleared from IDB until `sync-complete!` is called.** If the sync fails, the data remains in IDB for the next attempt. The server must be idempotent since data could be sent more than once if the app crashes between a successful POST and calling `sync-complete!`.

### `sync-complete!`

Called by the app's success handler after the server accepts the sync payload:

```clojure
(idb/sync-complete! db dirty-ids server-entities)
```

1. Remove `dirty-ids` from the dirty set in `_meta`
2. Remove entities with those IDs from IDB (tombstones and negative-ID entities)
3. Purge negative-ID entities from the in-memory store (for the kinds present in `server-entities`)
4. `tx*` the `server-entities` into both memory and IDB as clean data (not added to dirty set)

### Typical App Flow

```clojure
;; On reconnect or periodic check:
(idb/sync! db
  (fn [dirty-entities]
    (let [updates (remove api/delete? dirty-entities)
          deletes (filter api/delete? dirty-entities)]
      (ajax/post! "/ajax/sync"
        {:updates updates :deletions deletes}
        (fn [server-response]
          (idb/sync-complete! db
            (map :id dirty-entities)
            server-response))))))
```

## Service Worker Reader

### `c3kit.bucket.idb-reader`

A minimal namespace for reading and clearing dirty data from IDB in a service worker context. No dependencies on reagent, memory, api, or the bucket DB protocol.

**Dependencies:** `c3kit.bucket.idb-common` + `cljs.reader` only.

### API

- `(common/open db-name legend)` — Opens an IDB connection using shared version/schema logic. (From `idb-common`, not `idb-reader`.)
- `(idb-reader/dirty-entities idb)` — Reads dirty set from `_meta`, fetches those entities from IDB, deserializes. Returns a promise resolving to a vector.
- `(idb-reader/clear-dirty! idb ids)` — Removes specific IDs from the dirty set and deletes their entities from IDB. Returns a promise.

### Service Worker Flow

```clojure
(ns my-app.service-worker
  (:require [c3kit.bucket.idb-common :as common]
            [c3kit.bucket.idb-reader :as reader]))

(defn on-sync [legend]
  (-> (common/open "my-app" legend)
      (.then (fn [idb]
               (-> (reader/dirty-entities idb)
                   (.then (fn [entities]
                            (-> (post-to-server! entities)
                                (.then #(reader/clear-dirty! idb (set (map :id entities))))))))))))
```

## New/Modified Namespaces

| Namespace | Role | Key Dependencies |
|---|---|---|
| `c3kit.bucket.idb-common` | Serialization, version management, IDB open. Extracted from current `idb.cljs`. | `cljs.reader` |
| `c3kit.bucket.idb` | Offline-aware `idb-tx`/`idb-tx*`, dirty set management, `sync!`, `sync-complete!`, negative ID generation. Uses `idb-common`. | `idb-common`, `memory`, `api` |
| `c3kit.bucket.idb-reader` | SW-compatible dirty entity reader. | `idb-common` |
| `c3kit.bucket.indexeddb` | Existing deftype, gains `online-fn` field from config. | `idb`, `memory`, `api` |
| `c3kit.bucket.re-indexeddb` | Existing deftype, gains `online-fn` field from config. | `idb`, `re-memory`, `api` |

### Extraction to `idb-common`

The following move from `idb.cljs` to `idb-common` so that `idb-reader` can use them without pulling in memory/api:

- `schema-hash`
- `idb-version`
- `open` (IDB database open with version management)
- `ensure-object-stores`
- `clj->js-entity` / `js->clj-entity`
- `read-store`

## Deftype Changes

Both `IndexedDB` and `ReIndexedDB` gain an `online-fn` field:

```clojure
(deftype IndexedDB [legend store idb-atom db-name online-fn]
  ...)

(defmethod api/-create-impl :indexeddb [config schemas]
  (let [online-fn (or (:online? config) (constantly true))
        ...]
    (IndexedDB. legend store idb-atom db-name online-fn)))
```

The `online-fn` is passed through to `idb-tx` and `idb-tx*` to gate offline behavior.

## Key Design Decisions

1. **Dirty set in `_meta` is the single source of truth.** Not negative IDs, not tombstones, not metadata flags. One place to check.
2. **Negative IDs are a convention, not a mechanism.** They signal "created offline" for app-level logic (e.g., server strips them to let the DB assign real IDs), but dirty tracking doesn't depend on them.
3. **Tombstones use existing `api/delete?` / `:db/delete? true`.** No new deletion mechanism. Tombstones are persisted to IDB and included in the dirty set like any other dirty entity.
4. **Sync is app-triggered, not automatic.** Bucket provides the plumbing (dirty tracking, reading, clearing). The app decides when to sync and handles transport.
5. **Dirty data persists until `sync-complete!`.** No optimistic clearing. Server idempotency is required, but dirty data is never lost to crashes.
6. **`:online?` is a callback, not an atom or reagent track.** Keeps bucket decoupled from any state management library. The app can close over whatever state it wants.
7. **`idb-reader` has minimal dependencies.** Safe to import in a service worker build without pulling in reagent or bucket's memory layer.
8. **No new impl keywords.** Offline sync is a configuration option (`:online?`) on existing `:indexeddb` / `:re-indexeddb`, not a separate impl. Apps that don't provide `:online?` get unchanged pure-cache behavior.
