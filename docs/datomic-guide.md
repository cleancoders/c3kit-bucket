# c3kit.bucket Datomic Developer Guide

A guide to the Datomic-specific features available beyond the unified `c3kit.bucket.api`.

## Overview

Both Datomic backends implement the standard `api/DB` protocol, so `db/tx`, `db/find-by`, `db/entity`, and friends work identically across all backends. The Datomic implementations add a substantial layer on top: entity history, time-travel reads, raw datalog queries, aggregate finders, and timestamp helpers. The two backends — on-prem peer (`:datomic`) and Cloud client (`:datomic-cloud`) — share most of this surface but differ in connection config, schema indexing, query result format, and a handful of functions that exist on only one side.

## Choosing On-Prem vs Cloud

| | `:datomic` (on-prem peer) | `:datomic-cloud` |
|---|---|---|
| Library | `com.datomic/datomic-pro` or `datomic-free` | `com.datomic/client-cloud` |
| Storage | In-memory, SQL, DynamoDB, Cassandra | DynamoDB via Datomic Cloud service |
| Local dev | `datomic:mem://` | `:datomic-local` with `:storage-dir :mem` |
| Aggregate queries | `find-min*` / `find-max*` available | Not available |
| `squuid` | Built-in via `datomic.api/squuid` | Not available |
| `tx-ids` | Not available | Available |
| `:db/index` | Respected | Ignored |

Pick `:datomic` when you self-host Datomic Pro/Free, need aggregate helpers, or want `squuid`. Pick `:datomic-cloud` when you run Datomic Cloud on AWS and want managed infrastructure.

## Connection Setup: On-Prem (`:datomic`)

```clojure
(ns my-app.db
  (:require [c3kit.bucket.api :as db]
            [c3kit.bucket.datomic]       ;; registers :datomic impl
            [my-app.schema :as schema]))

(def config
  {:impl      :datomic
   :uri       "datomic:mem://my-app"   ;; or datomic:ddb://... / datomic:sql://...
   :partition :my-app})                ;; optional; defaults to :db.part/user

(defn install-db! []
  (db/set-impl! (db/create-db config schema/all-schemas)))
```

### URI Formats

| URI | Storage |
|-----|---------|
| `datomic:mem://my-app` | In-memory (dev/test) |
| `datomic:ddb://us-east-1/my-table/my-db` | DynamoDB |
| `datomic:sql://my-db?jdbc:postgresql://host/db` | SQL (Postgres, MySQL, etc.) |

The peer library connects directly to storage. `create-db` calls `datomic.api/create-database` followed by `datomic.api/connect`, so the database is created if it does not already exist.

## Connection Setup: Cloud (`:datomic-cloud`)

```clojure
(ns my-app.db
  (:require [c3kit.bucket.api :as db]
            [c3kit.bucket.datomic-cloud]  ;; registers :datomic-cloud impl
            [my-app.schema :as schema]))

(def config
  {:impl          :datomic-cloud
   :server-type   :datomic-local    ;; or :ion for deployed Cloud
   :storage-dir   :mem              ;; :mem for local; path for file-backed local
   :creds-profile "sandbox"
   :system        "my-system"
   :db-name       "my-app"
   :endpoint      "https://<api-id>.execute-api.us-east-1.amazonaws.com/"
   :region        "us-east-1"})

(defn install-db! []
  (db/set-impl! (db/create-db config schema/all-schemas)))
```

For production Cloud, set `:server-type :ion` and supply real `:endpoint`, `:region`, `:system`, and `:creds-profile` values. For local development, use `:server-type :datomic-local` with `:storage-dir :mem` (ephemeral) or `:storage-dir "/path/to/dir"` (durable). The spec suite config above is a real example straight from the test namespace.

`create-db` calls `datomic.client.api/client` to build a client, then `datomic.client.api/create-database` and `datomic.client.api/connect`.

## Schema Mapping

Apron schema specs translate to Datomic attributes via the optional `:db` option vector on each attribute spec.

```clojure
(def order
  {:kind    (s/kind :order)
   :id      s/id
   :number  {:type :string  :db [:unique-value]}
   :email   {:type :string  :db [:unique-identity :index]}
   :notes   {:type :string  :db [:fulltext]}
   :items   {:type [:ref]   :db [:component]}
   :total   {:type :bigdec  :db [:no-history]}})
```

### `:db` Options

| Option | Datomic attribute | Effect |
|--------|-------------------|--------|
| `:index` | `:db/index true` | Secondary index (on-prem only — ignored on Cloud) |
| `:unique-value` | `:db/unique :db.unique/value` | Value unique across all entities; upsert-by-value not allowed |
| `:unique-identity` | `:db/unique :db.unique/identity` | Value unique; supports upsert by this attribute |
| `:component` | `:db/isComponent true` | Cascades retraction to referenced entity |
| `:no-history` | `:db/noHistory true` | Skips history storage for this attribute |
| `:fulltext` | `:db/fulltext true` | Full-text search index |

**Cloud ignores `:db/index`.** The `spec->attribute` function accepts an `index-allowed?` flag; on-prem passes `true` and Cloud passes `false`, so `:db/index` is never included in Cloud schema transactions.

### Enum Schemas

Enums install Datomic ident datoms for use as keyword references:

```clojure
(def order-status
  {:enum   :order.status
   :values [:pending :processing :shipped :cancelled]})

;; Usage in an entity schema:
(def order
  {:kind   (s/kind :order)
   :id     s/id
   :status {:type :kw-ref}})
```

Pass both schemas in the `schemas` vector to `create-db`.

## Entity History and Time-Travel

Both backends share the history API, exposed as namespace-level functions (not on the `api/DB` protocol).

### `history`

Returns every version of an entity from creation to current state. Each entry carries `:db/tx` (transaction id) and `:db/instant` (java.util.Date).

```clojure
(require '[c3kit.bucket.datomic :as datomic])   ;; or datomic-cloud

(let [history (datomic/history order)]
  (doseq [version history]
    (println (:db/instant version) (:status version))))
```

### `created-at` / `updated-at` / `with-timestamps`

```clojure
(datomic/created-at order)     ;; => #inst "2024-01-15T..."
(datomic/updated-at order)     ;; => #inst "2024-03-20T..."

;; Attach both to the entity map
(datomic/with-timestamps order)
;; => {:kind :order ... :db/created-at #inst "..." :db/updated-at #inst "..."}
```

Both functions accept either an entity map or a raw entity id (long).

### `db-as-of`

Read the database as it existed at a point in time. Pass a transaction id or a `java.util.Date`:

```clojure
(require '[c3kit.bucket.datomic :as datomic])

(let [snapshot (datomic/db-as-of some-tx-id)]
  ;; snapshot is a Datomic db value at that transaction
  ...)
```

### `excise!`

Removes an entity from history entirely:

```clojure
(datomic/excise! order)
;; or
(datomic/excise! (:id order))
```

**Caveat:** Datomic excision is asynchronous at the index level. The entity disappears from queries only after the next indexing job completes. Do not rely on the entity being absent immediately after `excise!` returns.

## Datomic-Specific Query Functions

### `q` — Raw Datalog

Runs a raw datalog query against the current database value:

```clojure
(require '[c3kit.bucket.datomic :as datomic])

;; on-prem: finds entity ids matching the clause
(datomic/q '[:find ?e
             :where [?e :order/status :order.status/pending]])
```

Additional args after the query are passed as datalog `:in` bindings:

```clojure
(datomic/q '[:find ?e
             :in $ ?status
             :where [?e :order/status ?status]]
           :order.status/pending)
```

### `find-datalog` — Entity-Returning Queries

Runs a datalog query and returns results as hydrated entity maps. **The query syntax differs between backends:**

**On-prem** — use `?e` in `:find`; the library resolves entity ids to maps:

```clojure
;; c3kit.bucket.datomic
(datomic/find-datalog '[:find ?e
                        :in $
                        :where [?e :order/number]])
;; => ({:kind :order :id 123 :number "ORD-001" ...} ...)
```

**Cloud** — use `(pull ?e [*])` in `:find`; the library extracts the pulled maps:

```clojure
;; c3kit.bucket.datomic-cloud
(datomic-cloud/find-datalog '[:find (pull ?e [*])
                               :in $
                               :where [?e :order/number]])
;; => ({:kind :order :id 123 :number "ORD-001" ...} ...)
```

The return value is the same shape — entity maps with `:kind` and unqualified keys. Only the query syntax differs.

### `squuid` — Time-Based UUIDs (On-Prem Only)

Generates a UUID whose high bits encode the current time, making UUIDs sortable by creation time:

```clojure
(require '[c3kit.bucket.datomic :as datomic])

(datomic/squuid)  ;; => #uuid "5ebe2294-1234-5678-..."
```

`squuid` is a direct alias for `datomic.api/squuid`. It is **not available on Cloud**.

## Aggregate Queries: find-min / find-max (On-Prem Only)

These functions find the entity with the extreme value of an attribute across all entities of a kind.

```clojure
(require '[c3kit.bucket.datomic :as datomic])

;; Entity with the highest :total
(datomic/find-max-of-all :order :total)
;; => {:kind :order :id 456 :total 9999.99M ...}

;; Just the max value
(datomic/find-max-val-of-all :order :total)
;; => 9999.99M

;; Entity with the lowest :total
(datomic/find-min-of-all :order :total)
;; => {:kind :order :id 789 :total 0.99M ...}

;; Just the min value
(datomic/find-min-val-of-all :order :total)
;; => 0.99M
```

All four functions have `-` variants that accept an explicit db instance as first arg (e.g., `find-max-of-all-`), mirroring the pattern used throughout the library.

**These functions do not exist on `c3kit.bucket.datomic-cloud`.**

## Partitions (On-Prem Only)

On-prem Datomic organizes entities into partitions. c3kit.bucket defaults to `:db.part/user`. To use a custom partition, add `:partition` to the config:

```clojure
{:impl      :datomic
 :uri       "datomic:mem://my-app"
 :partition :my-app}
```

The partition must be installed before entities are written. You can transact the partition schema directly:

```clojure
(require '[c3kit.bucket.datomic-common :as datomic-common])

(datomic-common/transact! (datomic-common/partition-schema :my-app))
```

Partition config is ignored on Cloud — Cloud uses a single default partition.

## API Surface Differences

| Feature | `:datomic` | `:datomic-cloud` |
|---------|-----------|-----------------|
| `history` | `c3kit.bucket.datomic/history` | `c3kit.bucket.datomic-cloud/history` |
| `created-at` | `c3kit.bucket.datomic/created-at` | `c3kit.bucket.datomic-cloud/created-at` |
| `updated-at` | `c3kit.bucket.datomic/updated-at` | `c3kit.bucket.datomic-cloud/updated-at` |
| `with-timestamps` | `c3kit.bucket.datomic/with-timestamps` | `c3kit.bucket.datomic-cloud/with-timestamps` |
| `excise!` | `c3kit.bucket.datomic/excise!` | `c3kit.bucket.datomic-cloud/excise!` |
| `q` | `c3kit.bucket.datomic/q` | `c3kit.bucket.datomic-cloud/q` |
| `find-datalog` | `c3kit.bucket.datomic/find-datalog` | `c3kit.bucket.datomic-cloud/find-datalog` |
| `db-as-of` | `c3kit.bucket.datomic/db-as-of` | `c3kit.bucket.datomic-cloud/db-as-of` |
| `squuid` | `c3kit.bucket.datomic/squuid` | **not available** |
| `find-max-of-all` | `c3kit.bucket.datomic/find-max-of-all` | **not available** |
| `find-max-val-of-all` | `c3kit.bucket.datomic/find-max-val-of-all` | **not available** |
| `find-min-of-all` | `c3kit.bucket.datomic/find-min-of-all` | **not available** |
| `find-min-val-of-all` | `c3kit.bucket.datomic/find-min-val-of-all` | **not available** |
| `tx-ids` | **not available** | `c3kit.bucket.datomic-cloud/tx-ids` |
| `:partition` config | Supported | Ignored |
| `:db/index` schema option | Respected | Ignored |
| `transact` return | future (deref'd internally) | synchronous |
| `tempid` type | `datomic.db.DbId` | negative integer |
| `find-datalog` query | `[:find ?e ...]` | `[:find (pull ?e [*]) ...]` |
