# c3kit.bucket IndexedDB Developer Guide

A guide to building offline-capable ClojureScript applications using c3kit.bucket's IndexedDB implementation.

## Overview

c3kit.bucket provides an IndexedDB-backed database that mirrors the same `api/DB` protocol used by Datomic, JDBC, and in-memory backends. This means your application code uses the same `db/tx`, `db/find-by`, `db/entity` functions regardless of whether data lives in Datomic on the server or IndexedDB in the browser.

The IndexedDB implementation adds:
- **Optimistic transactions** -- writes update memory immediately, persist to IDB asynchronously
- **Offline ID generation** -- negative IDs for entities created without a server
- **Dirty tracking** -- knows which entities need syncing to the server
- **Automatic rollback** -- reverts memory if IDB persistence fails
- **Schema-based versioning** -- IDB object stores auto-migrate when your schema changes

## Configuration

Create a database instance with `api/create-db`:

```clojure
(ns my-app.config
  (:require [c3kit.bucket.api :as db]
            [c3kit.bucket.indexeddb]   ;; registers :indexeddb impl
            [reagent.core :as reagent]))

(def bucket-config
  {:impl         :indexeddb       ;; or :re-indexeddb for Reagent reactivity
   :db-name      "my-app"         ;; IndexedDB database name
   :store        (reagent/atom nil)
   :online?      #(.-onLine js/navigator)
   :idb-strategy :cache})         ;; or :primary (default)

(defn init! []
  (db/set-impl! (db/create-db bucket-config my-schema/full-schema)))
```

### Config Options

| Key | Type | Default                                                                      | Description                                                                                                                                                                                                                        |
|-----|------|------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `:impl` | keyword | required                                                                     | `:indexeddb` (plain atoms) or `:re-indexeddb` (Reagent atoms - uses :re-memory look ups for fine-grained reactivity)                                                                                                               |
| `:db-name` | string | `"c3kit-bucket"`                                                             | IndexedDB database name. Also used as localStorage key prefix for version tracking.                                                                                                                                                |
| `:store` | atom | :indexeddb:<br/> `(atom {})`<br/><br/> :re-indexeddb <br/>`(reagent/atom {})` | In-memory store.                                                                                                                                                                                                                   |
| `:online?` | `(fn [] bool)` | `(constantly true)`                                                          | Called to determine online/offline status. Controls ID generation, dirty tracking, and cache clearing. Intended for when using IndexedDB solely for offline persistence in an application with a traditional server-side database. |
| `:idb-strategy` | keyword | `:primary`                                                                   | `:primary` or `:cache`. See [IDB Strategy](#idb-strategy-cache-vs-primary).                                                                                                                                                        |

## IDB Strategy: Cache vs Primary

The `:idb-strategy` config controls how IndexedDB behaves on initialization.

### `:primary` (default)

IDB is the source of truth. On `init!`, all data is rehydrated from IDB into memory. Data persists across page reloads indefinitely.

Use this when your app has **no server-side database** -- IndexedDB is the only persistent store.

```clojure
{:impl :indexeddb :idb-strategy :primary}
```

### `:cache`

IDB supplements a server-side database (e.g., Datomic, Postgres). On `init!`, if `:online?` returns true, IDB and the memory store are cleared. The app then fetches fresh data from the server.

When offline, IDB retains data normally -- it becomes the temporary source of truth until connectivity returns and the app syncs.

Use this when your app has a **server-side database** that is authoritative when online.

```clojure
{:impl :indexeddb :idb-strategy :cache :online? #(.-onLine js/navigator)}
```

**Why this matters:** Without `:cache`, entities deleted server-side would reappear on page reload because IDB still has them. With `:cache`, the IDB is cleared on every online page load, and server data repopulates it through normal fetch/sync operations.

## The Online Function

The `:online?` function is called in several contexts:

- **`init!`** -- determines whether to clear IDB (`:cache` strategy) or rehydrate
- **`tx` / `tx*`** -- determines whether to assign negative IDs and track dirty entities
- **`idb-tx` / `idb-tx*`** -- determines whether to persist as dirty (offline) or clean (online)

Keep this function simple. Use `#(.-onLine js/navigator)` for browser connectivity. Avoid checks that may not be ready at init time (e.g., WebSocket connection state) -- if the check returns false at init time, `:cache` strategy won't clear the IDB.

```clojure
;; Good -- always available at init time
:online? #(.-onLine js/navigator)

;; Bad -- WS won't be connected when init! runs
:online? #(and (.-onLine js/navigator) @ws/open?)
```

## Initialization

After creating the database, call `init!` to open the IDB connection and load data:

```clojure
(require '[c3kit.bucket.idb-common :as idb])

;; Initialize -- opens IDB, rehydrates or clears based on strategy
(-> (idb/init! @db/impl)
    (.then (fn [_] (log/info "IndexedDB initialized"))))
```

`init!` returns a `js/Promise`. It:
1. Opens the IndexedDB database (creating/migrating object stores based on schema)
2. If `:cache` + online: clears IDB and memory store
3. If `:primary` or offline: rehydrates memory from IDB

You can pass specific entity kinds to rehydrate only a subset:

```clojure
(idb/init! @db/impl :user :activity)  ;; only rehydrate these kinds
```

## Using the Database

Once initialized, use the standard `c3kit.bucket.api` functions:

```clojure
(require '[c3kit.bucket.api :as db])

;; Create
(db/tx {:kind :activity :description "Loading dock" :warehouse "Phoenix"})

;; Read
(db/entity :activity 123)
(db/find-by :activity :warehouse "Phoenix")
(db/ffind-by :activity :warehouse "Phoenix")  ;; first match

;; Update
(db/tx (assoc activity :description "Updated"))

;; Delete
(db/delete activity)

;; Count
(db/count-by :activity :warehouse "Phoenix")
```

When offline, `tx` automatically:
- Assigns a negative ID (e.g., -1, -2) to new entities
- Tracks the entity in the dirty set for later syncing
- Persists to IDB for durability across page refreshes

When online, `tx` assigns positive IDs via the in-memory ID generator and persists to IDB as clean (non-dirty) data.

## Offline Sync Lifecycle

The sync lifecycle handles getting offline-created data to the server:

### 1. Offline: Data Accumulates

User creates/edits/deletes entities while offline. Each operation:
- Updates the in-memory store immediately (optimistic)
- Persists to IDB asynchronously
- Adds the entity's `{id kind}` to the dirty set in IDB's `_meta` store

### 2. Online: Sync Triggers

When connectivity returns, your app calls `sync!`:

```clojure
(require '[c3kit.bucket.idb-common :as idb])

(defn sync-callback [dirty-entities]
  (when (seq dirty-entities)
    (send-to-server dirty-entities
      (fn [server-response]
        (idb/sync-complete! @db/impl
          (set (map :id dirty-entities))
          (:payload server-response))))))

(idb/sync! @db/impl sync-callback)
```

`sync!` reads the dirty set from IDB, fetches the actual entity data, and passes them to your callback.

### 3. Server Processes

New entities created while offline are given negative IDs as temp IDs. Ideally, these IDs are stripped when sent to the backend, but for an extra layer of protection, use`idbc/sync-tx*` to handle them:

```clojure
(require '[c3kit.bucket.idbc :as idbc])

(defn handle-sync [{:keys [updates deletions]}]
  (let [{:keys [entities id-map]} (idbc/sync-tx* updates)]
    (run! db/delete deletions)
    entities))
```

`sync-tx*` strips negative IDs so the database assigns real ones, and returns an `id-map` of `{old-negative-id new-real-id}` for remapping cross-references.

### 4. Cleanup

`sync-complete!` handles post-sync cleanup:

```clojure
(idb/sync-complete! @db/impl dirty-id-set server-entities)
```

This:
- Soft-deletes negative-ID entities from memory
- Transacts server-returned entities (with real IDs) into both memory and IDB
- Removes synced entries from the dirty set

### 5. Refresh

When receiving fresh server data (e.g., after navigating to a page), use `refresh!` to replace stale offline entities:

```clojure
(idb/refresh! @db/impl server-entities)
```

This purges negative-ID entities for the relevant kinds and loads the server data as clean entities.

## Server-Side Idempotency

Network retries, background sync, and service workers can all send the same offline data multiple times. Use `idbc/claim-sync!` to deduplicate:

```clojure
(require '[c3kit.bucket.idbc :as idbc])

(defn sync-from-offline [{:keys [body]}]
  (let [sync-id (:sync-id body)
        claimed (idbc/claim-sync! sync-id)]
    (if claimed
      (ajax/ok (process-sync body))  ;; first time -- process
      (ajax/ok []))))                ;; duplicate -- skip
```

The client generates a deterministic sync ID from the entity data. `claim-sync!` returns `true` only on the first call for a given ID, preventing duplicate processing. It maintains an in-memory set of up to 100 recent sync IDs.

### Helper Functions (c3kit.bucket.idbc)

| Function | Description |
|----------|-------------|
| `offline-id?` | Returns true if an ID is negative (offline-generated) |
| `sync-tx` | Transacts a single entity, stripping negative IDs |
| `sync-tx*` | Batch version -- returns `{:entities [...] :id-map {neg-id real-id}}` |
| `claim-sync!` | Returns true if sync-id hasn't been processed; prevents duplicates |
| `reset-sync-state!` | Clears processed sync set (for test isolation) |

## Reagent Integration (`:re-indexeddb`)

For ClojureScript apps using Reagent, use `:re-indexeddb` instead of `:indexeddb`. The only difference is the store uses `reagent/atom` and the entity/find functions are Reagent-aware, triggering reactive re-renders when data changes.

```clojure
(ns my-app.config
  (:require [c3kit.bucket.re-indexeddb]  ;; registers :re-indexeddb impl
            [reagent.core :as reagent]))

(def bucket-config
  {:impl         :re-indexeddb
   :store        (reagent/atom nil)
   :db-name      "my-app"
   :online?      #(.-onLine js/navigator)
   :idb-strategy :cache})
```

With `:re-indexeddb`, Reagent components that call `db/find-by` or `db/entity` automatically re-render when the underlying data changes via `db/tx`.

## Service Worker Integration

Service workers enable **background sync** -- syncing offline data to the server even when the user has closed the tab. Without a service worker, offline data only syncs when the user has the page open and goes back online. Bucket does not provide a service worker; it supports a bring-your-own approach.

### Why Use a Service Worker

1. **Tab-closed sync** -- User makes changes offline, closes the browser, and leaves. When the device regains connectivity, the service worker wakes up and syncs the data.
2. **Offline caching** -- Service workers can cache static assets (HTML, CSS, JS, images) so the app loads instantly even without network.
3. **Resilience** -- If the main app's sync fails (e.g., page crashes), the service worker provides a fallback sync path.

### How It Works

The service worker and the main app **share the same IndexedDB database**. The service worker reads dirty entities from IDB and POSTs them to the server, just like the main app does.

```
Main App (page open)                Service Worker (background)
    |                                      |
    |-- db/tx (offline) ----------------> IDB (shared)
    |                                      |
    |-- [goes online] --> sync! ----+      |
    |                               |      |
    |   OR (if page closed)         |      |
    |                               +----> sync event --> read IDB --> POST to server
```

### Coordinating Main App and Service Worker

When the page is open, **both** the main app and the service worker could try to sync simultaneously. This causes race conditions on the shared IDB connection. The solution: the service worker checks if any pages are open and defers to the main app:

```clojure
;; In the service worker
(defn sync-event [event]
  (when (= (tag event) background-sync-tag)
    (-> (match-all-clients)
        (.then (fn [clients]
                 (if (pos? (.-length clients))
                   (log/info "Page is open, deferring sync to main app.")
                   (when @idb-atom
                     (-> (reader/dirty-entities @idb-atom)
                         (.then (fn [entities]
                                  (when (seq entities)
                                    (send-sync-fetch entities))))))))))))
```

- **Page open** -- Main app handles sync (it has the live IDB connection and can update the UI)
- **Page closed** -- Service worker handles sync (the only process that can)

### Service Worker IDB Access

The service worker opens the same IDB database using `idb-io/open`. However, service workers cannot access `localStorage`, so the schema versioning mechanism (which uses localStorage) returns `nil` and the database is opened at its current version without triggering a schema upgrade. The main app is responsible for schema migrations.

### Service Worker Deployment

Service workers are aggressively cached by browsers (up to 24 hours). To ensure users get updated service worker code promptly:

1. **Server cache headers** -- Serve `service-worker.js` with `Cache-Control: no-store` so the browser always fetches the latest version.
2. **`skipWaiting` + `clientsClaim`** -- Call these in the service worker's install/activate handlers so the new version takes over immediately without waiting for all tabs to close.
3. **`updatefound` listener** -- Add this on the client side when registering the service worker to detect and log when a new version is installed.

## Schema and Versioning

IDB object stores are created automatically from your entity schemas. Each entity kind becomes an object store (e.g., `:activity` -> `"activity"` store), plus a `_meta` store for dirty tracking.

When your schema changes (new kinds, new fields), c3kit.bucket detects the change via a hash of the legend and increments the IDB version. This triggers `onupgradeneeded`, which:
- Creates new object stores for new kinds
- Deletes object stores for removed kinds
- Existing stores with new fields work automatically (IDB is schemaless within a store)

Version tracking uses localStorage (`<db-name>-schema-hash` and `<db-name>-schema-ver`). 

## Complete Example

```clojure
;; config.cljs
(ns my-app.config
  (:require [c3kit.bucket.re-indexeddb]
            [reagent.core :as reagent]))

(def bucket {:impl         :re-indexeddb
             :store        (reagent/atom nil)
             :db-name      "my-app"
             :online?      #(.-onLine js/navigator)
             :idb-strategy :cache})

;; schema.cljc
(ns my-app.schema
  (:require [c3kit.apron.schema :as s]))

(def task
  {:kind        (s/kind :task)
   :id          {:type :long}
   :title       {:type :string}
   :completed?  {:type :boolean}})

(def full-schema [task])

;; init.cljc
(ns my-app.init
  (:require [c3kit.bucket.api :as db]
            [my-app.config :as config]
            [my-app.schema :as schema]))

(defn install-db! []
  (db/set-impl! (db/create-db config/bucket schema/full-schema)))

;; main.cljs
(ns my-app.main
  (:require [c3kit.bucket.api :as db]
            [c3kit.bucket.idb-common :as idb]
            [my-app.init :as init]))

(defn ^:export main []
  (init/install-db!)
  (-> (idb/init! @db/impl)
      (.then (fn [_] (render-app))))
  (.addEventListener js/window "online"
    (fn [] (sync-offline-data!))))

;; sync.cljs
(ns my-app.sync
  (:require [c3kit.bucket.api :as db]
            [c3kit.bucket.idb-common :as idb]))

(defn sync-callback [dirty-entities]
  (when (seq dirty-entities)
    (let [updates (remove :db/delete? dirty-entities)
          deletes (filter :db/delete? dirty-entities)]
      (send-to-server {:updates updates :deletions deletes}
        (fn [response]
          (idb/sync-complete! @db/impl
            (set (map :id dirty-entities))
            (:payload response)))))))

(defn sync-offline-data! []
  (idb/sync! @db/impl sync-callback))

;; server_sync.clj
(ns my-app.server-sync
  (:require [c3kit.bucket.api :as db]
            [c3kit.bucket.idbc :as idbc]
            [c3kit.wire.ajax :as ajax]))

(defn handle-sync [{:keys [body]}]
  (let [sync-id (:sync-id body)
        claimed (idbc/claim-sync! sync-id)]
    (if claimed
      (let [{:keys [entities]} (idbc/sync-tx* (:updates body))]
        (run! db/delete (:deletions body))
        (ajax/ok entities))
      (ajax/ok []))))
```
