# c3kit.bucket.idbc — Cross-Platform IDB Sync Utilities

## Problem

Apps using bucket's IndexedDB offline sync need to guard against offline-generated negative IDs leaking to server-side handlers, process synced entities (stripping negative IDs), and prevent duplicate sync requests. These patterns are generic but currently reimplemented per-app.

## Solution

New `c3kit.bucket.idbc` (cljc) namespace with three concerns:

### 1. `offline-id?` predicate

```clojure
(defn offline-id? [id] (and (number? id) (neg? id)))
```

Cross-platform check for offline-generated IDs. Used by client guards (don't send offline entities through live-save paths) and server guards (no-op when offline entity reaches a handler).

### 2. `sync-tx` / `sync-tx*`

```clojure
(defn sync-tx [entity]
  (if (offline-id? (:id entity))
    (db/tx (dissoc entity :id))
    (db/tx entity)))
```

Transacts an entity from offline sync. Strips negative IDs so the database assigns real ones. Uses `db/tx` (not Datomic directly) for backend-agnostic operation and testability with the memory impl.

`sync-tx*` is the batch version. Returns `{:entities [...] :id-map {old-neg-id new-real-id}}` so callers can remap cross-references between synced entities.

### 3. `claim-sync!` mutex

```clojure
(defn claim-sync! [sync-id]
  ...)
```

Server-side idempotency for sync requests. Returns true if the sync-id hasn't been processed before. Uses a bounded atom set that auto-trims when it exceeds `max-processed-syncs` (default 100).

## What stays where it is

- `idb-common.cljs` — CLJS-only: `ensure-offline-id`, dirty tracking, `sync!`, `sync-complete!`
- `idb-io.cljs` — CLJS-only: low-level IDB operations
- `idbc.cljc` — cross-platform sync utilities only

## Tests

`spec/cljc/c3kit/bucket/idbc_spec.cljc` using memory impl:

- `offline-id?` — true for negative numbers, false for positive/nil/zero/non-numbers
- `sync-tx` — strips negative ID and creates, passes through positive ID as update
- `sync-tx*` — batch, returns correct entities and id-map
- `claim-sync!` — first call true, duplicate false, trims after max
