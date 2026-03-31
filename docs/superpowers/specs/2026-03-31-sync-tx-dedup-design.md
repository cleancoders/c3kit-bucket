# sync-tx Dedup Keys Design

## Problem

`sync-tx*` processes offline entities one at a time. If the server crashes mid-batch (e.g., after persisting 5 of 10 entities), the client retries the entire batch. The already-persisted entities have their offline IDs stripped and `db/tx` creates duplicates.

`claim-sync!` provides first-line dedup via an in-memory set of processed sync IDs, but this state is lost on server restart — it cannot protect against the crash-and-retry scenario.

## Solution

Add optional dedup keys to `sync-tx` and `sync-tx*`. When provided, offline-ID entities are checked against the database before creating — if a matching entity already exists, it is updated (upsert) rather than duplicated.

## API

### sync-tx

Gains an optional second argument — a collection of attribute keywords:

```clojure
(sync-tx entity)                                       ;; no dedup (existing behavior)
(sync-tx entity [:employee-status :date :operation])   ;; dedup by these attrs
```

### sync-tx*

Gains an optional second argument — a map of `{kind [attrs]}`:

```clojure
(sync-tx* entities)                                    ;; no dedup (existing behavior)
(sync-tx* entities {:activity [:employee-status :date :operation]
                    :timecard [:employee :date]})
```

Kinds not present in the map fall through to existing behavior (no dedup).

## Dedup Logic

Applies only to entities with offline (negative) IDs. Positive-ID entities are treated as updates and behave exactly as today.

When dedup keys are provided for an entity's kind:

1. Extract the dedup key values from the entity (e.g., `{:employee-status 5 :date "2026-03-31"}`)
2. `db/ffind-by` the entity's kind with those key-value pairs
3. **Match found** → merge the incoming entity onto the existing one (preserving the existing real ID), then `db/tx`
4. **No match** → existing behavior: `db/tx (dissoc entity :id)`

## id-map

`sync-tx*` returns `{:entities [...] :id-map {neg-id real-id}}`. When dedup finds an existing entity, `real-id` is the existing entity's ID. The mapping is still correct for callers that need to remap cross-references.

## Backwards Compatibility

- Both arguments are optional. Existing callers are unaffected.
- Kinds without entries in the dedup map get existing behavior.
- `claim-sync!` is unchanged — it remains a fast-path dedup for the common case.
- Deletions are not affected — `db/delete` on a nonexistent entity is already a no-op.

## Files to Modify

- `src/cljc/c3kit/bucket/idbc.cljc` — add dedup key support to `sync-tx` and `sync-tx*`
- `spec/cljc/c3kit/bucket/idbc_spec.cljc` — tests for dedup behavior
- `docs/indexeddb-guide.md` — document dedup keys in the sync lifecycle section
