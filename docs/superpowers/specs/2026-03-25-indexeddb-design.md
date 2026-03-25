# IndexedDB Implementation for c3kit-bucket

## Overview

An IndexedDB persistence layer for c3kit-bucket that makes IDB the source of truth for client-side data, with an in-memory store (atom or reagent/atom) as a synchronous read cache. This enables local-first architecture where data survives page refreshes and browser restarts.

Two implementation keywords mirror the existing memory/re-memory pattern:

- **`:indexeddb`** — IDB + plain atom. Synchronous reads, no reactivity.
- **`:re-indexeddb`** — IDB + ReMemory. Synchronous reads with Reagent reactivity.

## Architecture

```
[Component] --read--> [In-Memory Store] (synchronous, optionally reactive)
[Component] --write--> [IndexedDB] --on success--> [In-Memory Store]
[Startup]   --rehydrate--> [IndexedDB] --> [In-Memory Store]
```

- **Reads** go through the in-memory store (Memory or ReMemory). Never query IDB at read time.
- **Writes** go to IDB first. On success, the entity is synced into the in-memory store. On failure, the in-memory store is untouched — the UI never shows unpersisted data.
- **Deletes** follow the same write-through pattern.

## Protocols

Both `:indexeddb` and `:re-indexeddb` implement `api/DB` and `migrator/Migrator`.

### api/DB

- `(-tx [this entity])` — Writes to IDB, syncs to in-memory store on success. Returns a `js/Promise` resolving to the transacted entity.
- `(-tx* [this entities])` — Single IDB transaction for the batch. Syncs all entities to in-memory store on success. Returns a `js/Promise`.
- `(-entity [this kind id])` — Reads from in-memory store. Synchronous.
- `(-find [this kind options])` — Reads from in-memory store. Synchronous.
- `(-count [this kind options])` — Reads from in-memory store. Synchronous.
- `(-reduce [this kind f init options])` — Reads from in-memory store. Synchronous.
- `(-clear [this])` — Clears both IDB and in-memory store.
- `(-delete-all [this kind])` — Deletes from IDB, then clears kind from in-memory store.
- `(close [this])` — Closes the IDB connection.

### migrator/Migrator

Schema operations update the legend atom and reconcile IDB object stores via version upgrades.

## Data Storage

### Object Store Mapping

One IDB object store per entity kind. A `:user` kind maps to a `"user"` object store. Entities are stored as JavaScript objects, keyed by `:id`.

A `"_meta"` object store holds internal metadata (schema hash).

### Schema Versioning

IDB requires an integer version to trigger `onupgradeneeded`:

1. On `create-db`, hash the full legend deterministically (kinds + attributes, sorted).
2. Compare to the stored hash in localStorage (keyed by `"{db-name}-schema-hash"`).
3. If different, increment the version counter in localStorage (`"{db-name}-schema-ver"`) and open IDB with the new version.
4. If unchanged, open IDB with the current version.
5. `onupgradeneeded` reconciles object stores — creates new ones for new kinds, removes stale ones, ensures `"_meta"` exists.

The localStorage dependency is an implementation detail hidden behind the bucket API. Consumers never interact with it.

## Rehydration

On startup, data is loaded from IDB into the in-memory store before the app renders.

```clojure
;; Load all kinds
(-> (idb/rehydrate! db)
    (.then (fn [] (mount-app!))))

;; Load specific kinds
(-> (idb/rehydrate! db :user :settings)
    (.then (fn [] (mount-app!))))
```

- `rehydrate!` is variadic: no args loads all kinds, specific kind keywords load only those.
- Returns a `js/Promise` that resolves when all data is loaded into the in-memory store.
- Internally reads all entities from the specified IDB object stores and `tx*`s them into Memory/ReMemory.

## API Examples

### Configuration

```clojure
;; Without reactivity
(def db (api/create-db {:impl :indexeddb :db-name "myapp"} schemas))

;; With Reagent reactivity
(def db (api/create-db {:impl :re-indexeddb :db-name "myapp"} schemas))
```

### Writes

```clojure
;; Common case - don't need the return value
(let [user {:kind :user :name "Alice"}]
  (db/tx user)
  (do-something-with user))

;; When you need confirmation
(-> (db/tx {:kind :user :name "Alice"})
    (.then (fn [saved-user] (prn "Saved:" saved-user)))
    (.catch (fn [err] (prn "Failed:" err))))

;; Batch
(db/tx* [{:kind :user :name "Alice"}
         {:kind :user :name "Bob"}])
```

### Reads

```clojure
;; Synchronous, same as Memory/ReMemory
(db/find-by :user :active true)
(db/entity :user 42)
(db/find :user :where [[:age ['> 18]]] :order-by {:name :asc})
```

### Startup

```clojure
(-> (idb/rehydrate! db)
    (.then (fn [] (mount-app!))))
```

## Implementation Structure

### Files

- `src/cljs/c3kit/bucket/indexeddb.cljs` — Core IDB operations (open, read, write, delete, versioning). `IndexedDB` deftype implementing `api/DB` and `migrator/Migrator` with a plain atom store.
- `src/cljs/c3kit/bucket/re_indexeddb.cljs` — `ReIndexedDB` deftype wrapping IDB operations with a ReMemory store for reactivity.

### Shared Logic

Both implementations share the same IDB read/write logic. The only difference is the atom type backing the in-memory store:

- `:indexeddb` — `atom` (from `memory.cljc`)
- `:re-indexeddb` — `reagent/atom` (from `re_memory.cljs`)

### Registration

```clojure
(defmethod api/-create-impl :indexeddb [config schemas] ...)
(defmethod api/-create-impl :re-indexeddb [config schemas] ...)
```

## Performance

IDB writes are typically sub-millisecond to low single-digit milliseconds for individual entities. No perceptible UX difference from synchronous in-memory writes. `db/tx*` uses a single IDB transaction for batches, keeping bulk operations fast.

## Key Design Decisions

1. **IDB is the source of truth.** ReMemory/Memory is a read cache. The UI never shows data that isn't persisted.
2. **Writes are async (promises), reads are synchronous.** This preserves the existing synchronous read API while being honest about the async persistence boundary.
3. **One object store per kind.** Maps naturally to the legend and enables efficient per-kind operations.
4. **Schema versioning via legend hashing.** Attribute-level granularity — any schema change (new kind, new attribute, removed attribute) triggers an IDB upgrade.
5. **Rehydration is explicit.** The app controls when IDB data loads into memory, typically at startup before mounting.
6. **Two impls mirror memory/re-memory.** `:indexeddb` for non-reactive use, `:re-indexeddb` for Reagent apps.
