# Bucket History Guide

Impl-agnostic entity history: timelines, point-in-time reads, timestamps, and excision.

## Motivation

Applications often need Datomic-style history (entity timelines, `as-of` reads, created/updated
timestamps) while tests run against `:memory`, which records no history by default. The
`c3kit.bucket.history` API works the same way across backends that implement `HistoryDB`.

## Quick start

```clojure
(require '[c3kit.bucket.api :as db]
         '[c3kit.bucket.history :as history])

;; Tests: wrap memory (or re-memory) with the history decorator
(db/set-impl! (db/create-db {:impl :memory-history :storage {:impl :memory}} [schema]))

;; Production Datomic: HistoryDB is native — no wrapper needed
(db/set-impl! (db/create-db {:impl :datomic :uri "datomic:mem://app"} [schema]))

(def e (db/tx :kind :thing :name "v1"))
(def e (db/tx e :name "v2"))

(history/history e)
;; => [{:id .. :name "v1" :db/tx .. :db/instant ..}
;;     {:id .. :name "v2" :db/tx .. :db/instant ..}]

(def t (:db/instant (first (history/history e))))
(db/entity- (history/as-of t) :thing (:id e))  ; point-in-time read
```

## Supported?

```clojure
(history/supported?)     ; current @api/impl
(history/supported? db)  ; explicit db
```

Returns true when the db implements `HistoryDB` (`:datomic`, `:datomic-cloud`,
`:memory-history`). Plain `:memory` returns false.

## API

Every operation has a default-db form and an explicit-db form suffixed with `-`
(e.g. `history` / `history-`), matching `c3kit.bucket.api`.

| Function | Description |
|---|---|
| `history` | Vector of every version, oldest → newest. Each version has `:db/tx` and `:db/instant`. Deletions appear as `{:db/tx .. :db/instant .. :db/deleted? true}`. Unknown entity → `[]`. Requires `(:id entity)`. |
| `created-at` | Instant of the first tx, or nil. Accepts entity or bare id. |
| `updated-at` | Instant of the most recent tx (deletions count), or nil. |
| `with-timestamps` | Entity plus `:db/created-at` and `:db/updated-at`. |
| `as-of` | Read-only `api/DB` view as of time `t` (instant **or** tx id, inclusive). Composes with `db/find-`, `db/entity-`, etc. Writes throw. |
| `entity-as-of` | Sugar: `(db/entity- (as-of t) kind id)` |
| `find-as-of` | Sugar: `(apply db/find- (as-of t) kind opts)` |
| `ffind-as-of` | First match of `find-as-of` |
| `excise!` | Erase the entity **and** all history of it |

### Deletion semantics

The new API uses a **rich deletion marker** (`:db/deleted? true` plus tx metadata).
Legacy `c3kit.bucket.datomic/history` still returns trailing `nil` for deletions (back-compat).

### `as-of` notes

- `t` may be a wall-clock instant or a tx id.
- Datomic may stamp multiple transactions with the same millisecond. Prefer tx ids when you need
  exact version boundaries; when using instants, capture wall-clock time *between* transactions.
- The view is read-only: `-tx`, `-tx*`, `-clear`, and `-delete-all` throw `ex-info`.

## Memory history decorator

```clojure
{:impl :memory-history
 :storage {:impl :memory}}       ; default if :storage omitted

{:impl :memory-history
 :storage {:impl :re-memory}}    ; reagent-reactive + history
```

Or wrap any existing db:

```clojure
(require '[c3kit.bucket.memory-history :as mh])
(def db (mh/decorate some-db))
```

### Caveats

- Version log lives in process memory (unbounded for long-lived stores).
- History begins at wrap time — entities that already exist in a shared store have no versions.
- Ideal for tests; use judgment in production. Over Datomic you never wrap — Datomic implements
  `HistoryDB` natively.
- No-op txs (entity unchanged) record nothing (Datomic parity).
- Failed CAS throws in the delegate before recording — nothing is logged.

## Datomic

Both `:datomic` and `:datomic-cloud` implement `HistoryDB` natively using the peer/client history APIs.
Legacy namespace helpers (`c3kit.bucket.datomic/history`, `created-at`, `excise!`, …) remain available
and unchanged in behavior.

## Composition model

Facets compose by nesting config — not by multiplying impl keywords:

```clojure
;; history over memory
{:impl :memory-history :storage {:impl :memory}}

;; history over reactive memory (cljs)
{:impl :memory-history :storage {:impl :re-memory}}
```

There is no `:history?` config flag inside backends, and no `:re-memory-history` keyword product.
