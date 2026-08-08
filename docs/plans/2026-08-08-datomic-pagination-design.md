# Datomic Pagination & :order-by — Design & Implementation Plan

Status: **implemented**

Builds on the refactored datomic layer (merged deftypes, `DatomicApi` driver port — see
`docs/plans/2026-08-08-history-design.md`). All code references below are to the post-history-refactor sources.

## Motivation

Bucket's `find` supports `:drop`/`:take` (pagination) and `:order-by`. Behavior today by impl:

- **SQL (jdbc)** — translated into the query; the database pages efficiently. Works well.
- **Memory** — sorts/pages in memory over the store. Fine.
- **Datomic on-prem** (`datomic.clj` `do-find`, ~line 133) — queries bare eids (`:find ?e`),
  drops/takes on the id tuples, hydrates only the survivors via lazy `d/entity`. Cheap — BUT `d/q`
  returns an **unordered set**, so page boundaries are nondeterministic: page 2 of one call can
  overlap or skip entities relative to page 1 of the previous call.
- **Datomic cloud** (`datomic_cloud.clj` `do-find`, ~line 127) — **disastrous**: the query is
  `[:find (pull ?e [*]) ...]`, so every matching entity is fully hydrated on the query group and
  shipped to the client *before* `:drop`/`:take` runs. A `:take 20` over a 1M-entity kind pulls
  1M full entities across the wire.
- **`:order-by` on datomic** — unsupported (the `api/find` docstring says so).

## Design

One pipeline for both datomic impls:

```
1. query ids (+ sort-key columns when :order-by)   — tuples only, no entity hydration
2. sort tuples in memory                            — by sort keys, eid as tie-break
3. drop/take                                        — page of [eid ...] tuples
4. hydrate ONLY the page                            — lazy d/entity (on-prem)
                                                      one batch pull (cloud)
```

Datalog cannot push sort into the query engine (no ORDER BY), but a query can return any values
bound in its `:where` clauses straight off the index datoms. So the sort key rides along with the
eid as a tuple column — the matching set is realized as small `[eid val]` tuples, never as entities.

### Example

`(find :bibelot :where {:color "blue"} :order-by {:size :asc} :drop 40 :take 20)` becomes:

```clojure
;; step 1 — ids + sort keys
[:find ?e ?v0
 :where [?e :bibelot/color "blue"]
        [(get-else $ ?e :bibelot/size ::nil) ?v0]]
;; => #{[17592186045418 3] [17592186045421 1] ...}

;; steps 2-3 — in memory
(->> tuples (sort-by ...) (drop 40) (take 20))

;; step 4 — hydrate 20
;; on-prem: (d/entity ddb eid) per survivor (lazy, local)
;; cloud:   ONE query: [:find (pull ?e [*]) :in $ [?e ...]]  bound to the page ids
```

## Implementation

### Shared helpers (in `datomic_common.clj`)

The two impls differ only in hydration, so steps 1–3 are shared:

```clojure
(defn order-by->extra
  "Returns {:syms [?v0 ...] :clauses [[(get-else $ ?e <attr> ::nil) ?v0] ...] :dirs [:asc ...]}
  for the :order-by option. :id sorts by ?e directly (no extra clause/column)."
  [kind order-by] ...)

(defn sort-and-page-tuples
  "Sorts result tuples by the order-by columns (tie-break: eid), then applies :drop/:take.
  With no :order-by but with :drop/:take, sorts by eid for deterministic pages.
  With neither, returns tuples untouched (no cost added to plain finds)."
  [options dirs tuples] ...)
```

Details:

- **Query construction.** Base stays `[:find ?e :in $ :where <where>]`. With `:order-by`, append
  one find var and one clause per sort key. Use `get-else` with a sentinel default (e.g. the
  namespaced keyword `::nil`) — a plain `[?e :bibelot/size ?v]` clause would act as a join and
  silently *exclude* entities missing the attribute, which plain `find` must not do.
- **Sentinel/nil sorting.** Before comparing, map the sentinel back to `nil` and use clojure's
  `compare` — `nil` compares lowest, which matches the memory impl's `sort-by field` behavior
  exactly (nils first ascending, last descending). No custom null-placement semantics.
- **`:desc`** — reversed comparator (`#(compare %2 %1)`), same as memory (memory.cljc ~line 121).
- **`:id` as sort field** — sort by `?e` directly; skip the get-else column.
- **Tie-break by `?e`** always, so equal sort keys still page deterministically.
- **Determinism without `:order-by`.** When `:drop`/`:take` is present, sort tuples by eid before
  paging. When neither pagination nor order-by is requested, do NOT sort — plain finds keep their
  current cost.
- **Multi-key `:order-by`.** `:order-by` is a map `{field dir}`; support each entry in map order as
  successive sort columns (compare column 0, then 1, ...). NOTE: the memory impl currently applies
  only `(first order-by)` (memory.cljc `apply-order-by`) — acceptable to match datomic to
  single-key initially, but if multi-key is implemented, fix memory to match and spec it shared.
  Insertion order is only reliable for literal maps ≤8 entries (array-map); document that.
- **Cardinality-many sort attrs** — one tuple per value would duplicate entities across pages.
  Detect via the legend (`:type` is a vector / `:seq`) and throw ex-info; same check should
  eventually apply to memory for parity.
- **Vector-distance ops** (`'<->` etc., supported by memory/postgres) — NOT supported on datomic;
  throw ex-info with a clear message.

### On-prem `do-find` (`datomic.clj`)

```clojure
(defn do-find [db kind options]
  (if-let [where (seq (common-api/build-where-datalog db kind (:where options)))]
    (let [{:keys [syms clauses dirs]} (common-api/order-by->extra kind (:order-by options))
          query (concat [:find '?e] syms '[:in $ :where] where clauses)]
      (->> (common-api/-q db query)
           (common-api/sort-and-page-tuples options dirs)
           (q->entities db)))                       ;; unchanged: lazy d/entity per page id
    []))
```

`q->entities` maps over tuples taking `(first %)` — works unchanged for wider tuples and preserves
the sorted order.

### Cloud `do-find` (`datomic_cloud.clj`)

```clojure
(defn do-find [db kind options]
  (if-let [where (seq (common-api/build-where-datalog db kind (:where options)))]
    (let [{:keys [syms clauses dirs]} (common-api/order-by->extra kind (:order-by options))
          query    (concat [:find '?e] syms '[:in $ :where] where clauses)
          page-ids (->> (common-api/-q db query)
                        (common-api/sort-and-page-tuples options dirs)
                        (map first))]
      (hydrate-page db page-ids)))
    []))

(defn- hydrate-page
  "ONE round trip for the whole page, then restore page order."
  [db page-ids]
  (let [results  (common-api/-q db
                   '[:find (pull ?e [*]) :in $ [?e ...]]
                   (common-api/-db db) [page-ids])
        by-id    (into {} (map (fn [[e]] [(:db/id e) e]) results))]
    (->> page-ids (map by-id) (map attributes->entity))))
```

Critical detail: query results are a set — **not** in binding order — so the pull results must be
re-ordered by `page-ids` (index by `:db/id`, map the ids). This replaces today's
`(pull ?e [*])`-inside-the-main-query, fixing the full-hydration disaster while keeping a single
round trip (the client API has no lazy `d/entity`; per-id pulls would be N+1 remote calls).

`DatomicAsOfView` / `DatomicCloudAsOfView` call the same `do-find`, so as-of reads get the fix for
free. `do-count` is unchanged (already a count aggregate). `-reduce` routes through `do-find`.

### `api.cljc` docstring

Update `find`'s docstring: remove "(not supported by datomic)" from `:order-by`; note that the
vector-distance operators remain memory/postgres-only; note datomic pagination/order-by cost is
O(matching set) per call (see Performance).

## Performance characteristics (document in datomic-guide.md too)

Be honest with users:

- Datalog is set-semantics: any paged/ordered find realizes **all matching `[eid sort-key]`
  tuples** in peer (on-prem) or client (cloud) memory, then sorts — O(n log n) per page request,
  repeated every page. Tuples are cheap (two scalars), entities are never bulk-hydrated.
- Practical guidance: unremarkable up to ~100k matches (tens of ms); at ~1M matches expect
  seconds per page and ~100MB of transient allocation — SQL with a covering index does the same
  page in milliseconds at O(offset+take). Datomic's mitigation is that the sort runs on *your*
  peer, not a shared server; but the right tool at that scale is a hand-written AVET index walk
  (`d/datoms :avet` / `d/index-range`): index-ordered, lazy, O(offset+take) — per indexed
  attribute, with filters applied during the walk. Bucket's generic `find` intentionally does not
  abstract that.
- Cloud additionally ships the tuple set over the wire from the query group.

## Specs

- **Wire the existing shared `order-by-specs`** (impl_spec.cljc) into `datomic_spec.clj` and
  `datomic_cloud_spec.clj` (NOT `order-by-vector-specs` — vector ops stay unsupported). Adjust any
  assertions that assume memory-only semantics; nil ordering should already agree (both use
  `compare` with nil-lowest).
- **New shared `pagination-specs`** in impl_spec.cljc, run by memory, jdbc/h2, datomic, cloud:
  - seed ~10 entities; page through with `:order-by {:size :asc} :take 3 :drop n`; concatenated
    pages equal the full sorted set — no overlaps, no gaps.
  - same with `:desc`.
  - `:drop`/`:take` WITHOUT `:order-by`: two identical calls return identical pages, and
    concatenated pages cover the full set exactly (determinism).
  - entities missing the order-by attribute still appear (nil-first asc / nil-last desc).
  - `:order-by` on `:id`.
  - cardinality-many order-by attr throws (datomic; memory if parity check added).
- **Cloud-specific**: page order preserved after batch pull (seed with attribute values that sort
  differently than eids, assert order).

## Files changed

| File | Change |
|---|---|
| `src/clj/c3kit/bucket/datomic_common.clj` | add `order-by->extra`, `sort-and-page-tuples` |
| `src/clj/c3kit/bucket/datomic.clj` | `do-find` gains order-by columns + shared sort/page |
| `src/clj/c3kit/bucket/datomic_cloud.clj` | `do-find` → ids query + `hydrate-page` batch pull |
| `src/cljc/c3kit/bucket/api.cljc` | `find` docstring |
| `src/cljc/c3kit/bucket/impl_spec.cljc` | `pagination-specs`; possibly extend `order-by-specs` |
| `spec/clj/c3kit/bucket/datomic_spec.clj` | run `order-by-specs` + `pagination-specs` |
| `spec/clj/c3kit/bucket/datomic_cloud_spec.clj` | run `order-by-specs` + `pagination-specs` |
| `spec/cljc/c3kit/bucket/memory_spec.cljc` | run `pagination-specs` |
| `docs/datomic-guide.md` | performance characteristics section |
| `CHANGES.md` | entry: datomic order-by support; cloud pagination fix; deterministic pages |

## Verification

- `clojure -M:test:spec` — full clj/cljc suite (datomic on-prem via `datomic:mem://`, cloud via
  datomic-local `:mem`, jdbc/h2/sqlite, memory).
- `clojure -M:test:cljs once` — memory/re-memory under cljs.
- REPL sanity on a seeded db: identical page sequences across repeated calls; cloud query count
  per find is 2 (ids + batch pull), not 1-per-entity.

## Future work (deferred)

- Multi-key `:order-by` across all impls (memory currently honors only the first entry).
- Keyset/cursor pagination (`:after {field value}`) — turns deep pages into range predicates;
  still O(matching set) in datalog, but the natural API step if apps need stable infinite scroll.
- Per-view AVET index-walk escape hatch for genuinely large ordered scans.
