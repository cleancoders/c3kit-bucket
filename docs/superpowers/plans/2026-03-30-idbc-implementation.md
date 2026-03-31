# c3kit.bucket.idbc Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add cross-platform IDB sync utilities (`offline-id?`, `sync-tx`, `sync-tx*`, `claim-sync!`) to c3kit-bucket.

**Architecture:** New `c3kit.bucket.idbc` (cljc) namespace using `db/tx` for backend-agnostic sync transaction processing. Tests use the memory impl via `spec-helperc/with-schemas`.

**Tech Stack:** Clojure/ClojureScript (cljc), speclj, c3kit.bucket.api

---

### Task 1: `offline-id?` predicate

**Files:**
- Create: `src/cljc/c3kit/bucket/idbc.cljc`
- Create: `spec/cljc/c3kit/bucket/idbc_spec.cljc`

- [ ] **Step 1: Write failing tests**

```clojure
(ns c3kit.bucket.idbc-spec
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [describe context it should= should should-not]]
            [c3kit.bucket.idbc :as sut]))

(describe "idbc"

  (context "offline-id?"

    (it "true for negative integers"
      (should (sut/offline-id? -1))
      (should (sut/offline-id? -100)))

    (it "false for positive integers"
      (should-not (sut/offline-id? 1))
      (should-not (sut/offline-id? 12345)))

    (it "false for zero"
      (should-not (sut/offline-id? 0)))

    (it "false for nil"
      (should-not (sut/offline-id? nil)))

    (it "false for non-numbers"
      (should-not (sut/offline-id? "hello"))
      (should-not (sut/offline-id? :keyword))))
  )
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket && clj -M:test:spec`
Expected: FAIL — namespace `c3kit.bucket.idbc` not found

- [ ] **Step 3: Write minimal implementation**

```clojure
(ns c3kit.bucket.idbc)

(defn offline-id?
  "Returns true if id is a negative number (offline-generated ID)."
  [id]
  (and (number? id) (neg? id)))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket && clj -M:test:spec`
Expected: All pass

- [ ] **Step 5: Commit**

```bash
cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket
git add src/cljc/c3kit/bucket/idbc.cljc spec/cljc/c3kit/bucket/idbc_spec.cljc
git commit -m "add offline-id? predicate to c3kit.bucket.idbc"
```

---

### Task 2: `sync-tx`

**Files:**
- Modify: `src/cljc/c3kit/bucket/idbc.cljc`
- Modify: `spec/cljc/c3kit/bucket/idbc_spec.cljc`

- [ ] **Step 1: Write failing tests**

Add to `idbc_spec.cljc` requires:
```clojure
[c3kit.bucket.api :as db]
[c3kit.bucket.impl-spec :as impl-spec]
[c3kit.bucket.spec-helperc :as helper]
```

Add after the `offline-id?` context:

```clojure
(context "sync-tx"
  (helper/with-schemas [impl-spec/bibelot])

  (it "strips negative id and creates new entity"
    (let [result (sut/sync-tx {:kind :bibelot :id -1 :name "offline"})]
      (should (pos? (:id result)))
      (should= "offline" (:name result))
      (should= result (db/entity (:id result)))))

  (it "passes through positive id as update"
    (let [existing (db/tx :kind :bibelot :name "original")
          result   (sut/sync-tx (assoc existing :name "updated"))]
      (should= (:id existing) (:id result))
      (should= "updated" (:name result))
      (should= "updated" (:name (db/entity (:id existing))))))

  (it "creates entity when id is nil"
    (let [result (sut/sync-tx {:kind :bibelot :name "no-id"})]
      (should (pos? (:id result)))
      (should= "no-id" (:name result)))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket && clj -M:test:spec`
Expected: FAIL — `sync-tx` not defined

- [ ] **Step 3: Write minimal implementation**

Add to `idbc.cljc`:

```clojure
(ns c3kit.bucket.idbc
  (:require [c3kit.bucket.api :as db]))

(defn sync-tx
  "Transacts an entity from offline sync. Strips offline IDs so the
   database assigns real ones. Returns the persisted entity."
  [entity]
  (if (offline-id? (:id entity))
    (db/tx (dissoc entity :id))
    (db/tx entity)))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket && clj -M:test:spec`
Expected: All pass

- [ ] **Step 5: Commit**

```bash
cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket
git add src/cljc/c3kit/bucket/idbc.cljc spec/cljc/c3kit/bucket/idbc_spec.cljc
git commit -m "add sync-tx to c3kit.bucket.idbc"
```

---

### Task 3: `sync-tx*`

**Files:**
- Modify: `src/cljc/c3kit/bucket/idbc.cljc`
- Modify: `spec/cljc/c3kit/bucket/idbc_spec.cljc`

- [ ] **Step 1: Write failing tests**

Add after `sync-tx` context:

```clojure
(context "sync-tx*"
  (helper/with-schemas [impl-spec/bibelot])

  (it "processes mixed positive and negative ids"
    (let [existing (db/tx :kind :bibelot :name "existing")
          entities [{:kind :bibelot :id -1 :name "offline-1"}
                    {:kind :bibelot :id -2 :name "offline-2"}
                    (assoc existing :name "updated")]
          {:keys [entities id-map]} (sut/sync-tx* entities)]
      (should= 3 (count entities))
      (should= "offline-1" (:name (first entities)))
      (should= "offline-2" (:name (second entities)))
      (should= "updated" (:name (nth entities 2)))
      (should (pos? (:id (first entities))))
      (should (pos? (:id (second entities))))
      (should= (:id existing) (:id (nth entities 2)))
      (should= (:id (first entities)) (get id-map -1))
      (should= (:id (second entities)) (get id-map -2))
      (should-not (contains? id-map (:id existing)))))

  (it "returns empty id-map when no negative ids"
    (let [existing (db/tx :kind :bibelot :name "existing")
          {:keys [entities id-map]} (sut/sync-tx* [(assoc existing :name "updated")])]
      (should= 1 (count entities))
      (should= {} id-map)))

  (it "handles empty input"
    (let [{:keys [entities id-map]} (sut/sync-tx* [])]
      (should= [] entities)
      (should= {} id-map))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket && clj -M:test:spec`
Expected: FAIL — `sync-tx*` not defined

- [ ] **Step 3: Write minimal implementation**

Add to `idbc.cljc`:

```clojure
(defn sync-tx*
  "Batch sync-tx. Returns {:entities [...] :id-map {old-neg-id new-real-id}}
   so callers can remap cross-references between synced entities."
  [entities]
  (reduce (fn [{:keys [entities id-map]} entity]
            (let [old-id (:id entity)
                  result (sync-tx entity)]
              {:entities (conj entities result)
               :id-map   (if (offline-id? old-id)
                           (assoc id-map old-id (:id result))
                           id-map)}))
          {:entities [] :id-map {}}
          entities))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket && clj -M:test:spec`
Expected: All pass

- [ ] **Step 5: Commit**

```bash
cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket
git add src/cljc/c3kit/bucket/idbc.cljc spec/cljc/c3kit/bucket/idbc_spec.cljc
git commit -m "add sync-tx* to c3kit.bucket.idbc"
```

---

### Task 4: `claim-sync!`

**Files:**
- Modify: `src/cljc/c3kit/bucket/idbc.cljc`
- Modify: `spec/cljc/c3kit/bucket/idbc_spec.cljc`

- [ ] **Step 1: Write failing tests**

Add after `sync-tx*` context:

```clojure
(context "claim-sync!"

  (it "returns true for new sync-id"
    (should (sut/claim-sync! "sync-1")))

  (it "returns false for duplicate sync-id"
    (sut/claim-sync! "sync-2")
    (should-not (sut/claim-sync! "sync-2")))

  (it "returns true for nil sync-id (always processes)"
    (should (sut/claim-sync! nil))
    (should (sut/claim-sync! nil)))

  (it "trims processed set when exceeding max"
    (doseq [i (range 105)]
      (sut/claim-sync! (str "trim-" i)))
    (should (sut/claim-sync! "trim-0"))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket && clj -M:test:spec`
Expected: FAIL — `claim-sync!` not defined

- [ ] **Step 3: Write minimal implementation**

Add to `idbc.cljc`:

```clojure
(def ^:private processed-syncs (atom #{}))
(def max-processed-syncs 100)

(defn- trim-processed-syncs! []
  (when (> (count @processed-syncs) max-processed-syncs)
    (reset! processed-syncs #{})))

(defn claim-sync!
  "Returns true if this sync-id hasn't been processed yet. Thread-safe.
   Prevents duplicate sync requests from creating duplicate entities.
   Always returns true for nil sync-id."
  [sync-id]
  (if-not sync-id
    true
    (let [claimed? (atom false)]
      (swap! processed-syncs
        (fn [syncs]
          (if (contains? syncs sync-id)
            (do (reset! claimed? false) syncs)
            (do (reset! claimed? true) (conj syncs sync-id)))))
      (trim-processed-syncs!)
      @claimed?)))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket && clj -M:test:spec`
Expected: All pass

- [ ] **Step 5: Commit**

```bash
cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket
git add src/cljc/c3kit/bucket/idbc.cljc spec/cljc/c3kit/bucket/idbc_spec.cljc
git commit -m "add claim-sync! to c3kit.bucket.idbc"
```

---

### Task 5: Run CLJS tests

**Files:** None — verification only

- [ ] **Step 1: Run CLJS test suite**

Run: `cd /Users/AlexRoot-Roatch/current-projects/c3kit-bucket && clj -M:test:cljs once`
Expected: All pass (cljc specs run in both JVM and CLJS)

- [ ] **Step 2: Commit if any fixes needed, otherwise done**

---

### Task 6: Update Wilson warehouse `bucket-idb` branch

**Files:**
- Modify: `wilson-warehouse/deps.edn` — point bucket to local
- Modify: `wilson-warehouse/src/clj/wilson/service_point.clj` — use `idbc/sync-tx`, `idbc/claim-sync!`, `idbc/offline-id?`
- Modify: `wilson-warehouse/src/clj/wilson/activity.clj` — use `idbc/offline-id?`
- Modify: `wilson-warehouse/src/cljs/wilson/core.cljs` — use `idbc/offline-id?`
- Modify: `wilson-warehouse/src/cljs/wilson/activities/common.cljs` — use `idbc/offline-id?`

- [ ] **Step 1: Point wilson-warehouse deps.edn to local bucket**

Uncomment the local override in `deps.edn`:
```clojure
com.cleancoders.c3kit/bucket {:local/root "../c3kit-bucket"}
```

- [ ] **Step 2: Update service_point.clj**

Replace inline negative-ID handling with `idbc` functions:

```clojure
(ns wilson.service-point
  (:require [c3kit.apron.log :as log]
            [c3kit.bucket.api :as db]
            [c3kit.bucket.idbc :as idbc]
            [c3kit.wire.apic :as apic]))

(defn -transact-ot-employee [{:keys [ot-request] :as ot-employee} id-map]
  (let [employee (dissoc ot-employee :id)]
    (if (idbc/offline-id? ot-request)
      (let [new-ot-id (get id-map ot-request)]
        (db/tx (assoc employee :ot-request new-ot-id)))
      (db/tx employee))))

(defn idb->datomic [{:keys [updates deletions]}]
  (let [grouped-entities                (group-by :kind updates)
        ot-requests                     (get grouped-entities :ot-request [])
        ot-employees                    (get grouped-entities :ot-request-employee [])
        timecards                       (get grouped-entities :timecard [])
        activities                      (get grouped-entities :activity [])
        {:keys [entities id-map]}       (idbc/sync-tx* (concat ot-requests activities timecards))
        updated-employees               (mapv #(-transact-ot-employee % id-map) ot-employees)]
    (run! db/delete deletions)
    (concat entities updated-employees)))

(defn sync-from-offline [{:keys [body]}]
  (let [sync-id (:sync-id body)
        claimed (idbc/claim-sync! sync-id)]
    (log/info "SYNC" sync-id "claimed:" claimed)
    (if claimed
      (apic/ok (idb->datomic body))
      (apic/ok []))))
```

- [ ] **Step 3: Update activity.clj**

Replace `(some-> (:id params) neg?)` with `(idbc/offline-id? (:id params))`:

Add require:
```clojure
[c3kit.bucket.idbc :as idbc]
```

Replace in `quick-save`:
```clojure
(if (some-> (:id params) neg?)
```
with:
```clojure
(if (idbc/offline-id? (:id params))
```

Replace in `ws-respond-activities`:
```clojure
(if (some-> (:id params) neg?)
```
with:
```clojure
(if (idbc/offline-id? (:id params))
```

- [ ] **Step 4: Update core.cljs**

Add require:
```clojure
[c3kit.bucket.idbc :as idbc]
```

Replace in `make-call!`:
```clojure
(and offline-handler (neg? (:id presented)))
```
with:
```clojure
(and offline-handler (idbc/offline-id? (:id presented)))
```

- [ ] **Step 5: Update common.cljs**

Add require:
```clojure
[c3kit.bucket.idbc :as idbc]
```

Replace in `send-beacon-when-hidden`:
```clojure
(not (neg? (:id @active-activity)))
```
with:
```clojure
(not (idbc/offline-id? (:id @active-activity)))
```

- [ ] **Step 6: Run all wilson-warehouse tests**

```bash
cd /Users/AlexRoot-Roatch/current-projects/wilson-warehouse
clj -M:test:spec && clj -M:test:cljs once
```

Expected: All pass

- [ ] **Step 7: Commit**

```bash
git add deps.edn src/clj/wilson/service_point.clj src/clj/wilson/activity.clj src/cljs/wilson/core.cljs src/cljs/wilson/activities/common.cljs
git commit -m "use c3kit.bucket.idbc for offline ID handling and sync"
```
