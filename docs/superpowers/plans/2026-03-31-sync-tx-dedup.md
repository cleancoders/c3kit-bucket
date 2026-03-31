# sync-tx Dedup Keys Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add optional per-kind dedup keys to `sync-tx` and `sync-tx*` so retried offline syncs upsert instead of creating duplicates.

**Architecture:** `sync-tx` gains an optional dedup-keys argument (a vector of attribute keywords). When present and the entity has a negative ID, it checks `db/ffind-by` before creating. `sync-tx*` accepts a `{kind [attrs]}` map and passes the appropriate keys per entity. Existing callers without dedup keys are unaffected.

**Tech Stack:** Clojure/ClojureScript (cljc), speclj

---

### Task 1: sync-tx with dedup keys — entity not yet in DB

**Files:**
- Modify: `spec/cljc/c3kit/bucket/idbc_spec.cljc`
- Modify: `src/cljc/c3kit/bucket/idbc.cljc`

- [ ] **Step 1: Write the failing test**

Add a new context inside the existing `"sync-tx"` context in `spec/cljc/c3kit/bucket/idbc_spec.cljc`:

```clojure
    (it "strips negative id and creates new entity with dedup keys when no match exists"
      (let [result (sut/sync-tx {:kind :bibelot :id -1 :name "offline" :size 5} [:name :size])]
        (should (pos? (:id result)))
        (should= "offline" (:name result))
        (should= 5 (:size result))
        (should= result (db/entity (:id result)))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clj -M:test:spec -f focus`

After adding `focus-` prefix to the new `it` block. Expected: FAIL — wrong arity for `sync-tx`.

- [ ] **Step 3: Write minimal implementation**

In `src/cljc/c3kit/bucket/idbc.cljc`, update `sync-tx`:

```clojure
(defn sync-tx
  "Transacts an entity from offline sync. Strips offline IDs so the
   database assigns real ones. When dedup-keys are provided and the entity
   has an offline ID, checks for an existing entity matching those attributes
   and upserts instead of creating a duplicate."
  ([entity] (sync-tx entity nil))
  ([entity dedup-keys]
   (if (offline-id? (:id entity))
     (if-let [existing (when (seq dedup-keys)
                         (apply db/ffind-by (:kind entity) (mapcat (fn [k] [k (get entity k)]) dedup-keys)))]
       (db/tx (merge (dissoc entity :id) {:id (:id existing)}))
       (db/tx (dissoc entity :id)))
     (db/tx entity))))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `clj -M:test:spec -f focus`

Expected: PASS

- [ ] **Step 5: Remove focus prefix and run all tests**

Run: `clj -M:test:spec`

Expected: All tests pass (existing `sync-tx` tests still green because 1-arity calls the 2-arity with `nil`).

---

### Task 2: sync-tx with dedup keys — entity already exists (upsert)

**Files:**
- Modify: `spec/cljc/c3kit/bucket/idbc_spec.cljc`

- [ ] **Step 1: Write the failing test**

Add after the previous test in the same context:

```clojure
    (it "upserts when dedup keys match an existing entity"
      (let [existing (db/tx :kind :bibelot :name "offline" :size 5 :color "red")
            result   (sut/sync-tx {:kind :bibelot :id -1 :name "offline" :size 5 :color "blue"} [:name :size])]
        (should= (:id existing) (:id result))
        (should= "blue" (:color result))
        (should= "blue" (:color (db/entity (:id existing))))))
```

- [ ] **Step 2: Run test to verify it passes**

This should already pass with the implementation from Task 1. Run: `clj -M:test:spec`

Expected: PASS — the `merge` logic handles the upsert.

---

### Task 3: sync-tx with dedup keys — positive ID bypasses dedup

**Files:**
- Modify: `spec/cljc/c3kit/bucket/idbc_spec.cljc`

- [ ] **Step 1: Write the failing test**

```clojure
    (it "positive id bypasses dedup keys and updates directly"
      (let [existing (db/tx :kind :bibelot :name "original" :size 5)
            result   (sut/sync-tx (assoc existing :name "updated") [:name :size])]
        (should= (:id existing) (:id result))
        (should= "updated" (:name result))))
```

- [ ] **Step 2: Run test to verify it passes**

Run: `clj -M:test:spec`

Expected: PASS — the `offline-id?` check short-circuits to `db/tx` for positive IDs.

---

### Task 4: sync-tx* with dedup-keys-by-kind map

**Files:**
- Modify: `spec/cljc/c3kit/bucket/idbc_spec.cljc`
- Modify: `src/cljc/c3kit/bucket/idbc.cljc`

- [ ] **Step 1: Write the failing test**

Add a new context inside the existing `"sync-tx*"` context:

```clojure
    (it "deduplicates offline entities using per-kind dedup keys"
      (let [existing (db/tx :kind :bibelot :name "w1" :size 1 :color "red")
            entities [{:kind :bibelot :id -1 :name "w1" :size 1 :color "blue"}
                      {:kind :bibelot :id -2 :name "w2" :size 2}]
            {:keys [entities id-map]} (sut/sync-tx* entities {:bibelot [:name :size]})]
        (should= 2 (count entities))
        (should= (:id existing) (:id (first entities)))
        (should= "blue" (:color (first entities)))
        (should (pos? (:id (second entities))))
        (should= (:id existing) (get id-map -1))
        (should= (:id (second entities)) (get id-map -2))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clj -M:test:spec -f focus`

Expected: FAIL — `sync-tx*` doesn't accept a second argument yet.

- [ ] **Step 3: Write minimal implementation**

In `src/cljc/c3kit/bucket/idbc.cljc`, update `sync-tx*`:

```clojure
(defn sync-tx*
  "Batch sync-tx. Returns {:entities [...] :id-map {old-neg-id new-real-id}}
   so callers can remap cross-references between synced entities.
   When dedup-keys-by-kind is provided (a map of {kind [attr-keys]}),
   offline entities are checked for duplicates before creating."
  ([entities] (sync-tx* entities nil))
  ([entities dedup-keys-by-kind]
   (reduce (fn [{:keys [entities id-map]} entity]
             (let [old-id     (:id entity)
                   dedup-keys (get dedup-keys-by-kind (:kind entity))
                   result     (sync-tx entity dedup-keys)]
               {:entities (conj entities result)
                :id-map   (if (offline-id? old-id)
                            (assoc id-map old-id (:id result))
                            id-map)}))
           {:entities [] :id-map {}}
           entities)))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `clj -M:test:spec -f focus`

Expected: PASS

- [ ] **Step 5: Remove focus prefix and run all tests**

Run: `clj -M:test:spec`

Expected: All tests pass (existing `sync-tx*` tests still green).

- [ ] **Step 6: Commit**

```bash
git add src/cljc/c3kit/bucket/idbc.cljc spec/cljc/c3kit/bucket/idbc_spec.cljc
git commit -m "add optional dedup keys to sync-tx and sync-tx*"
```

---

### Task 5: sync-tx* with kind not in dedup map falls through

**Files:**
- Modify: `spec/cljc/c3kit/bucket/idbc_spec.cljc`

- [ ] **Step 1: Write the failing test**

```clojure
    (it "kinds not in dedup map use existing behavior"
      (let [entities [{:kind :bibelot :id -1 :name "w1" :size 1}]
            {:keys [entities id-map]} (sut/sync-tx* entities {:thingy [:name]})]
        (should= 1 (count entities))
        (should (pos? (:id (first entities))))
        (should= (:id (first entities)) (get id-map -1))))
```

- [ ] **Step 2: Run test to verify it passes**

Run: `clj -M:test:spec`

Expected: PASS — `(get dedup-keys-by-kind :bibelot)` returns `nil`, which causes `sync-tx` to use existing behavior.

---

### Task 6: Run ClojureScript tests

**Files:** None (verification only)

- [ ] **Step 1: Run ClojureScript unit tests**

Run: `clj -M:test:cljs once`

Expected: All tests pass. The `idbc.cljc` file is cross-compiled and tests run in both Clojure and ClojureScript.

- [ ] **Step 2: Commit if any fixes were needed**

---

### Task 7: Update indexeddb-guide.md

**Files:**
- Modify: `docs/indexeddb-guide.md`

- [ ] **Step 1: Update the "Server Processes" section (section 3 of the sync lifecycle)**

After the existing `sync-tx*` explanation, add a paragraph about dedup keys:

```markdown
#### Handling Retries with Dedup Keys

If the server crashes mid-sync, the client will retry the entire batch. Without protection, `sync-tx*` would create duplicates for entities already persisted. Pass a dedup-keys-by-kind map to handle this:

\`\`\`clojure
(defn handle-sync [{:keys [updates deletions]}]
  (let [{:keys [entities id-map]} (idbc/sync-tx* updates
                                    {:activity [:employee-status :date :operation]
                                     :timecard [:employee :date]})]
    (run! db/delete deletions)
    entities))
\`\`\`

For each offline entity whose kind appears in the map, `sync-tx*` checks whether an entity with matching attribute values already exists. If so, it updates the existing entity instead of creating a duplicate. Kinds not in the map use the default create behavior.
```

- [ ] **Step 2: Update the Helper Functions table**

Update the `sync-tx` and `sync-tx*` rows:

| Function | Description |
|----------|-------------|
| `sync-tx` | Transacts a single entity, stripping negative IDs. Optional dedup-keys arg (vector of attrs) checks for existing matches before creating. |
| `sync-tx*` | Batch version — returns `{:entities [...] :id-map {neg-id real-id}}`. Optional dedup-keys-by-kind arg (`{kind [attrs]}`) prevents duplicates on retry. |

- [ ] **Step 3: Commit**

```bash
git add docs/indexeddb-guide.md
git commit -m "document sync-tx dedup keys in indexeddb guide"
```
