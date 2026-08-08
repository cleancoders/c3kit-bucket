# Bucket History — Design & Implementation Plan

Status: **implemented** (see `docs/history-guide.md`)

## Motivation

Applications use datomic's history capabilities (entity timelines, point-in-time reads, created/updated
timestamps), but tests run against the `:memory` impl, which records no history. Additionally, the only
history API today is datomic-specific (`c3kit.bucket.datomic/history`, `created-at`, ...), so application
code that uses it cannot run against any other impl.

This plan introduces:

1. **`c3kit.bucket.history`** — an impl-agnostic history API, styled after `c3kit.bucket.api`.
2. **`MemoryHistoryDecorator`** — a decorator that adds history to *any* `api/DB` impl (used to give the
   memory bucket history in tests).
3. **A datomic-layer refactor** — collapses the adapter deftypes and removes the `DatomicApi`/`api/DB`
   overlap so the new protocol fits cleanly.

### Design principles agreed

- **No config flags that branch inside impls.** `(if (:history? config) ...)` scattered through
  `memory.cljc` is an OCP violation and every future facet would add more. History is a *decorator*,
  composed at construction time.
- **No impl-keyword multiplication.** Facets (reagent-reactivity, history, indexeddb persistence) are
  orthogonal; encoding combinations as impl keywords (`:re-memory-history`, ...) multiplies. The impl
  keyword names the storage substrate; facets compose by nesting config.
- **One deftype per role, protocols per concern.** `DatomicDB` implements `api/DB`, `history/HistoryDB`,
  `migrator/Migrator`, and `DatomicApi` directly; the separate adapter deftypes are deleted.
- **Protocol methods follow the `-method` convention** (as `api/DB` and `migrator/Migrator` already do).

## Staging

Implement as two commits:

1. **Commit 1 — datomic-layer refactor.** Merge adapter deftypes into the DB deftypes, slim and rename
   `DatomicApi`, add `-legend` to `api/DB`. Zero behavior change; all existing specs stay green.
2. **Commit 2 — history feature.** `c3kit.bucket.history` namespace, native datomic implementations,
   `MemoryHistoryDecorator`, re-memory `select-*` decoupling, shared history specs, docs/CHANGES.

---

## Commit 1: Datomic-layer refactor

### 1.1 Problem being fixed

Today there are two deftypes per datomic product (`src/clj/c3kit/bucket/datomic.clj`,
`src/clj/c3kit/bucket/datomic_cloud.clj`):

- `DatomicDB [db-schema legend config api]` implements `api/DB` + `migrator/Migrator`.
- `DatomicOnPremApi [config conn]` implements `common-api/DatomicApi` (same split for cloud).

`DatomicApi` (in `src/clj/c3kit/bucket/datomic_common.clj`) mixes two abstraction levels:

- Driver primitives (`connect`, `db`, `transact`, `delete-database`, `as-of`, `q`, `history`, `d-entity`)
  — the genuine peer-vs-client variance.
- Dependency-inversion callbacks (`tx`, `tx*`, `do-find`) — these exist only so `datomic-common` (which
  cannot require `datomic.clj`/`datomic_cloud.clj` without a cycle) can call driver-specific code through
  the `(.-api db)` field. They are pure delegation with identical semantics to `api/DB`'s `-tx`/`-tx*`/`-find`.

Method names don't follow the `-method` convention, and `common-api/history` (raw datomic history db)
collides conceptually with `datomic/history` (entity versions).

### 1.2 New `DatomicApi` protocol

In `datomic_common.clj`, replace the protocol with a pure driver port:

```clojure
(defprotocol DatomicApi
  "Driver port: the variance between datomic on-prem (datomic.api) and cloud (datomic.client.api)."
  (-connect [impl])
  (-db [impl])
  (-transact [impl transaction])
  (-delete-database [impl])
  (-q [impl query] [impl query db args])
  (-d-entity [impl ddb eid])
  (-history-db [impl])          ;; raw (datomic/history (d/db conn)) view
  (-as-of-db [impl t]))         ;; raw (datomic/as-of (d/db conn) t) view
```

Notes:

- `tx`, `tx*`, `do-find` are **removed**. `datomic-common` call sites use `api/-tx*` / `api/-find` on the
  impl directly (the impl now implements `api/DB` itself — see 1.3). Affected call sites:
  - `delete-all` (datomic_common.clj:312): `(do-find (.-api db) db kind {})` → `(api/-find db kind {})`;
    `(tx* (.-api db) db (map api/soft-delete batch))` → `(api/-tx* db (map api/soft-delete batch))`.
- `-history-db` / `-as-of-db` are the renamed raw-view accessors. They CANNOT be named `-history`/`-as-of`:
  the new `history/HistoryDB` protocol uses those names with the same arity, and same-name same-arity
  methods on two protocols compile to colliding JVM interface methods — one deftype cannot give them
  different bodies. The `-db` suffix also makes "raw datomic view" explicit.

### 1.3 Merge the deftypes

`datomic.clj` — delete `DatomicOnPremApi`; `DatomicDB` absorbs its fields and methods:

```clojure
(deftype DatomicDB [db-schema legend config conn]
  api/DB
  ;; existing methods, but common-api calls pass `this` where they passed `api`
  common-api/DatomicApi
  (-connect [_] (reset! conn (connect (:uri config))))
  (-db [_] (datomic/db @conn))
  (-transact [_ transaction] (datomic/transact @conn transaction))
  (-delete-database [_] (datomic/delete-database (:uri config)))
  (-q [_ query] (datomic/q query (datomic/db @conn)))
  (-q [_ query db args] (apply datomic/q query db args))
  (-d-entity [_ ddb eid] (datomic/entity ddb eid))
  (-history-db [_] (datomic/history (datomic/db @conn)))
  (-as-of-db [_ t] (datomic/as-of (datomic/db @conn) t))
  migrator/Migrator
  ;; unchanged
  )

(defmethod api/-create-impl :datomic [config schemas]
  (let [legend     (atom (legend/build schemas))
        db-schemas (->> (flatten schemas) (mapcat #(common-api/->db-schema % true)))
        db         (DatomicDB. db-schemas legend config (atom nil))]
    (common-api/-connect db)
    db))
```

Same treatment in `datomic_cloud.clj`: delete `DatomicCloudApi`; `DatomicCloudDB` gets fields
`[db-schema legend config client conn]` and absorbs the cloud driver methods
(`-transact` wraps `{:tx-data transaction}`, `-d-entity` is `pull-entity`, `-delete-database` takes
`client config`, etc. — copy bodies from the current `DatomicCloudApi`).

### 1.4 `datomic-common` call-site updates

Every fn that reaches through `(.-api impl)` now calls the protocol on `impl` directly. Concretely
(all in `datomic_common.clj` unless noted):

| Current | Becomes |
|---|---|
| `(db (.-api impl))` / `datomic-db` | `(-db impl)` (keep `datomic-db` as a thin public wrapper if desired) |
| `(q (.-api db) query ...)` | `(-q db query ...)` |
| `(transact api transaction)` in `transact!` | `(-transact impl transaction)` |
| `(delete-database api)` / `(connect api)` in `clear` | `(-delete-database impl)` / `(-connect impl)` |
| `(d-entity (.-api impl) ddb eid)` | `(-d-entity impl ddb eid)` |
| `(history api)` in `tx-ids-`, `created-at-`, `updated-at-` | `(-history-db impl)` |
| `(as-of (.-api impl) txid)` in `entity-as-of-tx` | `(-as-of-db impl txid)` |
| `do-find`/`tx`/`tx*` callbacks in `delete-all` | `api/-find` / `api/-tx*` |

Also update the callers in `datomic.clj` / `datomic_cloud.clj` (`db-as-of`, `find-max-of-all-`,
`find-min-of-all-`, `installed-schema-legend`, `q`, `find-datalog`, `tx`/`tx*`/`do-find` local fns,
`update-form`, `id->entity`, `tx-entity-form`) — mechanical: `(.-api db)` disappears; where a fn passed
`(.-api db)` and `db` separately it now passes `db` once.

`api/DB` method bodies on the merged deftypes change accordingly, e.g.
`(-find [this kind options] (do-find this kind options))` (the ns-local `do-find`), and
`(-tx [this entity] (tx this entity))` — the former protocol round-trip through the adapter is gone.

### 1.5 Add `-legend` to `api/DB`

In `src/cljc/c3kit/bucket/api.cljc`:

```clojure
(defprotocol DB
  ...
  (-legend [this]))   ;; returns the legend ATOM (not its value)

(defn legend
  "Returns the legend (map of :kind -> schema) of the database implementation."
  [db] (deref (-legend db)))
```

It returns the **atom** because `c3kit.bucket.migration` (src/clj/c3kit/bucket/migration.clj:20) swaps it:
`(swap! (.-legend -db) ...)` → `(swap! (api/-legend -db) ...)`.

Implement `(-legend [_] legend)` on all deftypes implementing `api/DB`:
`MemoryDB` (memory.cljc), `ReMemoryDB` (re_memory.cljs), `IndexedDB` (indexeddb.cljs — shared by
`:re-indexeddb`), the jdbc deftype (jdbc.clj:666), `DatomicDB`, `DatomicCloudDB`.

Then replace **cross-namespace** field access `(.-legend db)` with `(api/-legend db)` in
`datomic_common.clj` (`where-all-of-kind`, `build-where-datalog` path) and `migration.clj`.
Same-namespace internal uses (e.g. `memory.cljc` reading its own field) may stay.

### 1.6 Commit-1 verification

- No public fn signatures change except protocol internals; `datomic-common` renames are
  internal-but-technically-public — note them in CHANGES.md.
- Run: `clojure -M:test:spec` (whole clj/cljc suite; datomic on-prem specs run against
  `datomic:mem://`, cloud specs against datomic-local `:mem` storage, both in-process).
- Run cljs suite: `clojure -M:test:cljs once`.
- Grep for stragglers: `grep -rn "\.-api\b\|(\.api " src spec` and `grep -rn "(\.-legend" src | grep -v memory.cljc`.

---

## Commit 2: History feature

### 2.1 New namespace `c3kit.bucket.history` (src/cljc/c3kit/bucket/history.cljc)

```clojure
(ns c3kit.bucket.history
  "Impl-agnostic API for entity history: timelines, point-in-time reads, timestamps, excision.
  Follows c3kit.bucket.api conventions: each operation has a default-db version (history, as-of, ...)
  and an explicit-db version suffixed with a dash (history-, as-of-, ...)."
  (:require [c3kit.bucket.api :as api]))

(defprotocol HistoryDB
  "API for entity history operations"
  (-history [db entity])
  (-as-of [db t])
  (-created-at [db id-or-entity])
  (-updated-at [db id-or-entity])
  (-excise! [db id-or-entity]))
```

Public surface (every fn below also gets a `-`-suffixed explicit-db twin, e.g. `history-`,
`as-of-`, `entity-as-of-`, implemented against a passed `db`; the plain versions deref `api/impl`):

```clojure
(supported?)               ;; => true when (satisfies? HistoryDB db)

(history entity)           ;; => vector of every version of the entity, oldest → newest.
                           ;;    Each version is the full entity stamped with :db/tx and :db/instant.
                           ;;    A deletion appears as {:db/tx <id> :db/instant <inst> :db/deleted? true}.
                           ;;    Unknown entity (or no recorded history) => [].
                           ;;    Requires (:id entity); asserts otherwise.

(created-at id-or-entity)  ;; => instant (java.util.Date / js/Date) of the first tx, or nil
(updated-at id-or-entity)  ;; => instant of the most recent tx (deletion counts), or nil
(with-timestamps entity)   ;; => entity + :db/created-at + :db/updated-at

(as-of t)                  ;; => a READ-ONLY api/DB view of the database as it was at time t.
                           ;;    Composes with all bucket.api read fns:
                           ;;      (api/find- (as-of t) :bibelot :where {:color "blue"})
                           ;;      (api/entity- (as-of t) :bibelot id)
                           ;;    Write operations (-tx, -tx*, -clear, -delete-all) throw.
                           ;;    t is an instant OR a tx id, inclusive.

(entity-as-of t kind id)   ;; sugar: (api/entity- (as-of- db t) kind id)
(find-as-of t kind & opts) ;; sugar: (apply api/find- (as-of- db t) kind opts)
(ffind-as-of t kind & opts);; sugar: first match

(excise! id-or-entity)     ;; erase the entity AND all trace of it from history
```

Helpers in the namespace:

```clojure
(defn ->id [id-or-entity] (if (map? id-or-entity) (:id id-or-entity) id-or-entity))

(deftype ReadOnlyDB [db]   ;; generic wrapper used by as-of implementations
  api/DB
  (close [_] nil)
  (-legend [_] (api/-legend db))
  (-entity [_ kind id] (api/-entity db kind id))
  (-find [_ kind options] (api/-find db kind options))
  (-count [_ kind options] (api/-count db kind options))
  (-reduce [_ kind f init options] (api/-reduce db kind f init options))
  (-tx [_ _] (throw (ex-info "as-of view is read-only" {})))
  (-tx* [_ _] (throw (ex-info "as-of view is read-only" {})))
  (-clear [_] (throw (ex-info "as-of view is read-only" {})))
  (-delete-all [_ _] (throw (ex-info "as-of view is read-only" {}))))
```

Deletion semantics decision: the new API returns a **rich deletion marker**
`{:db/tx .. :db/instant .. :db/deleted? true}` (the deletion's tx id and instant are useful).
The legacy `c3kit.bucket.datomic/history` fn keeps its current behavior (trailing `nil`) for
back-compat; only the new namespace uses the marker.

Explicitly **not** included (deferred, see Future work): tx-centric queries. A standalone `tx-ids`
fn is skipped on purpose — it's `(map :db/tx (history e))`.

### 2.2 Native datomic implementations

Both `DatomicDB` and `DatomicCloudDB` add (using each product's own `attributes->entity`):

```clojure
history/HistoryDB
(-history [this entity] (common-api/history-versions- this entity attributes->entity))
(-as-of [this t] (history/->ReadOnlyDB (->as-of-view this t)))
(-created-at [this id-or-entity] (common-api/created-at- this id-or-entity))
(-updated-at [this id-or-entity] (common-api/updated-at- this id-or-entity))
(-excise! [this id-or-entity] (common-api/excise!- this id-or-entity))
```

New fn in `datomic_common.clj` — like the existing `history-` but emits the deletion marker instead
of `nil` when `entity-as-of-tx` finds no attributes at a tx:

```clojure
(defn history-versions- [impl entity attributes->entity]
  ;; for each tx id in (tx-ids- impl (:id entity)):
  ;;   (or (entity-as-of-tx impl id kind txid attributes->entity)
  ;;       {:db/tx txid :db/instant (:db/txInstant (-d-entity impl (-db impl) txid)) :db/deleted? true})
  )
```

(`entity-as-of-tx` already stamps `:db/tx`/`:db/instant` on live versions. Keep legacy `history-`
delegating to the old behavior so `datomic/history` and `datomic-cloud/history` are unchanged.)

`->as-of-view`: a small per-product deftype whose `api/DB` read methods run against the pinned raw
view. Recommended shape — implement `common-api/DatomicApi` delegating everything to the real impl,
overriding only the methods that must answer from the pinned db:

```clojure
(deftype DatomicAsOfView [impl aodb]
  common-api/DatomicApi
  (-db [_] aodb)
  (-q [_ query] (-q impl query aodb []))          ;; 1-arg q must hit aodb
  (-q [_ query db args] (-q impl query db args))
  (-d-entity [_ ddb eid] (-d-entity impl ddb eid))
  ;; -connect/-transact/-delete-database: throw (read-only); -history-db/-as-of-db: delegate
  api/DB
  (-legend [_] (api/-legend impl))
  (-entity [this kind id] ...)   ;; same bodies as DatomicDB's read methods, with `this` as impl
  (-find [this kind options] (do-find this kind options))
  (-count [this kind options] (common-api/do-count this kind options))
  (-reduce [this kind f init options] (reduce f init (do-find this kind options)))
  ...)
```

where `aodb` = `(common-api/-as-of-db impl t)`. `t` may be an instant or tx id — datomic's `as-of`
accepts both natively. Wrap the view in `history/->ReadOnlyDB` so writes throw uniformly.

Keep the legacy public fns (`datomic/history`, `created-at`, `updated-at`, `with-timestamps`,
`excise!`, `db-as-of`, and the cloud equivalents) working unchanged.

### 2.3 `MemoryHistoryDecorator` (new ns `c3kit.bucket.memory-history`, src/cljc/c3kit/bucket/memory_history.cljc)

The ns name matters: `api/create-db` dynamically requires `c3kit.bucket.<impl-name>` on the JVM, so
`{:impl :memory-history}` resolves to this file. (cljs callers require it explicitly, as with all impls.)

A decorator over ANY `api/DB`. It keeps its version log in its own atom — the delegate is untouched.

```clojure
(deftype MemoryHistoryDecorator [db versions tx-counter]
  ;; db         - the wrapped api/DB
  ;; versions   - atom {id [version ...]}  (version = entity snapshot + :db/tx + :db/instant,
  ;;                                        or deletion marker {:db/tx .. :db/instant .. :db/deleted? true})
  ;; tx-counter - atom long (start 1000, monotonically increasing)
  api/DB
  (close [_] (api/close db))
  (-legend [_] (api/-legend db))
  (-entity [_ kind id] (api/-entity db kind id))
  (-find [_ kind options] (api/-find db kind options))
  (-count [_ kind options] (api/-count db kind options))
  (-reduce [_ kind f init options] (api/-reduce db kind f init options))
  (-tx [_ e] (let [result (api/-tx db e)] (record! versions tx-counter [result]) result))
  (-tx* [_ es] (let [results (api/-tx* db es)] (record! versions tx-counter results) results))
  (-clear [_] (api/-clear db) (reset! versions {}))
  (-delete-all [_ kind] (let [doomed (api/-find db kind {})]
                          (api/-delete-all db kind)
                          (record-deletions! versions tx-counter doomed)))
  history/HistoryDB
  (-history [_ entity] ...)      ;; (get @versions (:id entity)) => vector (or []); assert (:id entity)
  (-as-of [this t] ...)          ;; see below
  (-created-at [_ id-or-entity] ...) ;; first version's :db/instant
  (-updated-at [_ id-or-entity] ...) ;; last version's :db/instant (deletion markers count)
  (-excise! [_ id-or-entity]     ;; delete from delegate (if present) + (swap! versions dissoc id)
  migrator/Migrator
  ;; delegate all methods to db (migrator/-install-schema! db schema), etc.
  )
```

`record!` semantics (single fn used by both tx paths):

- One tx id per call: `(swap! tx-counter inc)` once, `(c3kit.apron.time/now)` once — all entities in a
  `tx*` batch share `:db/tx` and `:db/instant` (datomic parity).
- For each result:
  - Soft-delete result (`api/delete?` true) → append deletion marker under that id, but only if the id
    has a live previous version (deleting a nonexistent entity records nothing — datomic parity).
  - Normal result → compare to the previous version (`(dissoc prev :db/tx :db/instant)` vs result);
    if equal, record nothing (datomic records no datoms for a no-op tx); else append
    `(assoc result :db/tx txid :db/instant instant)`.
- Results are post-coercion entities (that's what impls return), so snapshots match stored data exactly.
- Use `c3kit.apron.corec/conjv` for vector appends.

`-as-of` implementation (generic — no knowledge of the delegate):

1. Choose the comparison key: number `t` → `:db/tx`; otherwise instant → `:db/instant` compared via
   millis (`.getTime` on both platforms; cljc via a small helper or `c3kit.apron.time` utilities).
   Inclusive: version qualifies when `key(version) <= t`.
2. For each id in `@versions`, take the LAST qualifying version; drop ids whose last qualifying version
   is a deletion marker or that have none.
3. Strip `:db/tx`/`:db/instant` from each survivor; build a memory-store-shaped map
   `{:all {id e}, <kind> {id e}, ...}`.
4. Serve reads via `(c3kit.bucket.memory/->MemoryDB (api/-legend this) (atom snapshot))` wrapped in
   `history/->ReadOnlyDB`. (Reuses memory's full `where`/`order-by` query engine for free.)

Construction:

```clojure
(defn decorate
  "Wrap any api/DB with in-memory history recording. History begins at wrap time."
  [db] (MemoryHistoryDecorator. db (atom {}) (atom 1000)))

(defmethod api/-create-impl :memory-history [config schemas]
  (decorate (api/create-db (or (:storage config) {:impl :memory}) schemas)))
```

Config composes by nesting — no flags, no keyword products:

```clojure
{:impl :memory-history :storage {:impl :memory}}      ;; test db with history (the default :storage)
{:impl :memory-history :storage {:impl :re-memory}}   ;; reagent-reactive with history
```

Documented caveats: the version log lives in process memory (unbounded for long-lived stores) and
history begins at wrap time — ideal for tests, use judgment elsewhere. Over `:datomic` you never wrap;
datomic implements `HistoryDB` natively.

Note on cas: a failed cas throws inside the delegate's `-tx`, so nothing is recorded — correct.

### 2.4 re-memory `select-*` decoupling

`re_memory.cljs`'s `slice-by-kind`/`slice-by-ids`/`->keyseq`(and `ensure-full-entity-and-meta` via
`select-tx-`) reach into `(.-store @api/impl)` / `(.-legend @api/impl)` by field. Under a decorator,
`@api/impl` is not a `ReMemoryDB` and field access breaks.

Fix: a namespace-level var holding the active store, set at creation (the create-impl already does
per-creation setup via `clear-slice-db-cache!`):

```clojure
(def ^:private active-store (atom nil))

(defmethod api/-create-impl :re-memory [config schemas]
  (let [store (or (:store config) (r/atom {}))]
    (clear-slice-db-cache!)
    (reset! active-store store)
    (ReMemoryDB. (atom (legend/build schemas)) store)))
```

`slice-by-kind`/`slice-by-ids` read `@@active-store`; legend lookups go through `(api/legend @api/impl)`
(protocol-based after commit 1, so it delegates through decorators correctly). `select-tx-`/`select-tx*-`
already take the db argument explicitly — they keep working when handed the inner db, but note in
docstrings that with a decorated impl, `select-tx` (the `@api/impl` flavor) must go through `api/-tx`
to be recorded; simplest is to route `select-tx-`'s swap through `api/-tx` on the passed db rather than
`memory/tx-entity` directly. (Behavior for undecorated use is identical.)

`:re-indexeddb` needs the same `active-store` treatment in `re_indexeddb.cljs`'s create-impl if it is
to be decorated; minimum for this commit is `:re-memory`.

### 2.5 Shared specs

Add `history-specs` to `src/cljc/c3kit/bucket/impl_spec.cljc` (it ships in the jar like the other
shared spec fns), modeled on the existing history context in `spec/clj/c3kit/bucket/datomic_spec.clj:188`
but **cljc-safe**: no `Thread/sleep` (cljs can't), no strict instant ordering (same-millisecond txs are
fine); use `java.util.Date` vs `js/Date` via reader conditionals; `c3kit.apron.time` for comparisons.

Tests (run against decorated memory, `:datomic`, and `:datomic-cloud` — identical assertions):

- `supported?` is true.
- history of a new entity: 1 version, correct attrs, has `:db/tx` and `:db/instant`.
- history of an entity tx'd 4 times (the "Biby" sequence): 4 versions, oldest→newest, each stamped;
  versions already sorted by `:db/tx`.
- no-op tx records no new version.
- deleted entity: prior versions retained; final entry is the deletion marker
  (`:db/deleted?` true, has `:db/tx` and `:db/instant`).
- `created-at`: correct type; within the last few seconds; not after now+1s.
- `updated-at`: not before `created-at`; reflects latest tx.
- `with-timestamps`: equals `created-at`/`updated-at` values.
- `created-at`/`updated-at` accept a bare id as well as an entity.
- `tx*`: entities saved in one batch share `:db/tx`.
- `as-of` by instant: capture `t` between two txs (`time/now` between them; memory/datomic both stamp
  real clock instants) — entity at `t` shows the earlier version; `(api/find- (as-of- db t) ...)` sees
  the old value; current db sees the new one.
- `as-of` by tx id: `(as-of (:db/tx (first (history e))))` shows the first version.
- `entity-as-of` / `find-as-of` / `ffind-as-of` sugar.
- as-of view is read-only: `-tx`/`-tx*`/`-clear`/`-delete-all` throw.
- `excise!`: entity gone from db, `history` returns `[]`.
  - CAVEAT: datomic excision is asynchronous (needs `sync-index`; see the commented-out spec at
    datomic_spec.clj:233). If it cannot be asserted reliably on datomic, keep the excise! test in a
    separate `excise-specs` fn run only by the memory spec, and leave datomic's commented spec as is.

Wire-up:

- `spec/cljc/c3kit/bucket/memory_spec.cljc`: add a history context using
  `{:impl :memory-history :storage {:impl :memory}}`, running `(spec/history-specs config)` +
  decorator-specific tests:
  - history starts at wrap time (pre-existing entities in a supplied `:storage` store have no versions),
  - `clear` resets the log,
  - `delete-all` records deletion markers,
  - plain `{:impl :memory}` still reports `(supported?)` false.
- `spec/clj/c3kit/bucket/datomic_spec.clj` and `datomic_cloud_spec.clj`: replace the bespoke history
  contexts with `(spec/history-specs config)`; keep one test each asserting the legacy fns
  (`sut/history` etc.) still behave as before (trailing `nil` on deletion).

### 2.6 Docs & housekeeping

- `api.cljc` `create-db` docstring: document `:memory-history` and its `:storage` key.
- CHANGES.md: entries for both commits (datomic-common protocol renames are technically breaking for
  anyone implementing `DatomicApi` outside this repo — call that out).
- After implementation, convert this design doc into (or supplement it with) a user-facing
  `docs/history-guide.md` following the style of `docs/indexeddb-guide.md`.

### 2.7 Commit-2 verification

- `clojure -M:test:spec` — full clj/cljc suite (memory, datomic mem, datomic-local cloud, jdbc/h2/sqlite).
- `clojure -M:test:cljs once` — cljs suite (memory, re-memory, decorator under cljs).
- Manual smoke in a REPL:

```clojure
(require '[c3kit.bucket.api :as db] '[c3kit.bucket.history :as history])
(db/set-impl! (db/create-db {:impl :memory-history :storage {:impl :memory}} [schema]))  ;; cljs
;; tx a few versions, then:
(history/history e)
(db/find- (history/as-of t) :thing :where {...})
```

---

## Future work (explicitly deferred)

- **Tx-centric queries** — `(tx-entities tx-id)` ("what else changed in this save") and
  `(changes-since tx-or-instant)` (audit trails, sync watermarks; tx ids are the correct monotonic
  cursor where timestamps skew). The decorator's log and datomic's log API can both answer these;
  add when an application needs them.
- Decorating `:indexeddb`/`:re-indexeddb` officially (needs the `active-store` treatment + async tx
  consideration).
- Respecting `:db/noHistory` schema options in the decorator (datomic drops history for such attrs;
  the decorator currently snapshots everything).
- The `datomic-common` `tx-form`/`insert-form`/`update-form` duplication between the two products —
  further cleanup now that the adapter split is gone.
