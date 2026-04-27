# c3kit-bucket Audit Response Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Address every applicable point from the c3kit-bucket open-source readiness audit (`~/Desktop/c3kit-bucket-audit.md`), shipping the work as two PRs / two releases (2.13.1 patch, then 2.14.0 minor).

**Architecture:** PR 1 combines repo hygiene cleanup with TDD-driven bug fixes and docstring additions; PR 2 is a documentation overhaul (README rewrite + new guides + CONTRIBUTING.md). PR 1 must merge and ship to Clojars before PR 2 starts so the README installation snippet can reference 2.14.0.

**Tech Stack:** Clojure / ClojureScript, Speclj for tests, `c3kit.apron.log` for logging, `clojure.tools.build.api` for builds, Clojars for distribution.

**Spec:** `docs/superpowers/specs/2026-04-27-audit-response-design.md`

---

## Conventions used in this plan

- Every code fix is TDD: failing test first, then minimal implementation, then verify.
- Test runner: `clj -M:test:spec` (Clojure), `clj -M:test:cljs once` (ClojureScript).
- Project uses Speclj. Log capture in tests is `(c3kit.apron.log/capture-logs ...)` plus `(c3kit.apron.log/captured-logs-str)` — see `spec/cljc/c3kit/bucket/memory_spec.cljc:66` and `spec/clj/c3kit/bucket/datomic_spec.clj:46` for reference.
- Commit messages follow the existing repo style (short imperative, no Co-Authored-By trailer unless the maintainer adds it).

---

# PR 1 — Cleanup + code fixes (ships as 2.13.1)

## Task 1: Branch setup and baseline test verification

**Files:** none modified

- [ ] **Step 1: Create feature branch from master**

```bash
git checkout master
git pull
git checkout -b audit-response-2.13.1
```

- [ ] **Step 2: Verify the baseline Clojure test suite passes before any changes**

Run: `clj -M:test:spec`
Expected: all specs PASS, no failures or errors.

- [ ] **Step 3: Verify the baseline ClojureScript test suite passes**

Run: `clj -M:test:cljs once`
Expected: all specs PASS.

If either suite is red on master, STOP — do not proceed with the audit response until master is green.

---

## Task 2: Untrack `c3kit-bucket.iml`

**Files:**
- Delete (untrack only): `c3kit-bucket.iml`

The audit notes (audit §1, "IDE/build cruft committed") that `*.iml` is in `.gitignore` but this file was committed before that rule. We remove it from tracking; `.gitignore` already prevents re-adding.

- [ ] **Step 1: Untrack the file (keep on disk)**

```bash
git rm --cached c3kit-bucket.iml
```

- [ ] **Step 2: Verify `.gitignore` already excludes `*.iml`**

Run: `grep -n '\*\.iml' .gitignore`
Expected output: `2:*.iml`

- [ ] **Step 3: Confirm git no longer tracks it**

Run: `git ls-files | grep iml || echo "untracked OK"`
Expected: `untracked OK`

---

## Task 3: Untrack `sqlite_test.db`

**Files:**
- Delete (untrack and remove from disk): `sqlite_test.db`

This is a 0-byte orphaned test artifact. `.gitignore` already lists it.

- [ ] **Step 1: Remove from tracking and disk**

```bash
git rm sqlite_test.db
```

- [ ] **Step 2: Verify `.gitignore` excludes it**

Run: `grep -n 'sqlite_test\.db' .gitignore`
Expected: `20:sqlite_test.db`

---

## Task 4: Untrack `.beads/` and update `.gitignore`

**Files:**
- Delete (untrack only, keep on disk): `.beads/` directory
- Modify: `.gitignore`

The `.beads/` directory is the project's internal issue tracker; we stop public mirroring. The directory stays on disk so the maintainer can keep using `bd` locally; `.gitignore` is updated so it never gets re-committed.

- [ ] **Step 1: Untrack the directory (keep files on disk)**

```bash
git rm -r --cached .beads
```

- [ ] **Step 2: Add `.beads/` to `.gitignore`**

Append a `.beads/` line to `.gitignore`. After the edit, the relevant section should look like:

```
sqlite_test.db
.beads/
```

- [ ] **Step 3: Verify**

Run: `git status .beads`
Expected: `.beads/` should not appear as tracked or modified (it's now ignored).

---

## Task 5: Move `AGENTS.md` to `.claude/AGENTS.md`

**Files:**
- Move: `AGENTS.md` → `.claude/AGENTS.md`

Beads-specific agent onboarding instructions; not for public consumers.

- [ ] **Step 1: Confirm `.claude/` exists**

Run: `ls -d .claude/`
Expected: directory present (it was already shown in the repo root).

- [ ] **Step 2: Move the file**

```bash
git mv AGENTS.md .claude/AGENTS.md
```

- [ ] **Step 3: Verify**

Run: `git status` and check the staged change is a rename (R).

---

## Task 6: Delete empty `sql.clj`

**Files:**
- Delete: `src/clj/c3kit/bucket/sql.clj`

The file contains only `(ns c3kit.bucket.sql)` with no other content. No callers — verify before deletion.

- [ ] **Step 1: Confirm no references**

Run: `grep -rn "c3kit.bucket.sql\b" src/ spec/ dev/`
Expected: no matches (or matches only in `sql.clj` itself).

- [ ] **Step 2: Delete**

```bash
git rm src/clj/c3kit/bucket/sql.clj
```

---

## Task 7: Clean up the `2.?.?` orphan entry in `CHANGES.md`

**Files:**
- Modify: `CHANGES.md` (entry at lines 129-131)

The entry reads:

```
### 2.?.?
 * fixes typo in migration usage 
 * Adds bucket.seed namespace for seeding databases with data
```

Both bullets describe changes that have long since shipped. Investigate which release actually contained them by running `git log` against the relevant files; fold the bullets into the correct version's section if found, otherwise delete the orphan.

- [ ] **Step 1: Investigate when these changes shipped**

Run: `git log --oneline -- src/cljc/c3kit/bucket/seed.cljc | tail -5`
Run: `git log --oneline --all -S "fixes typo in migration" -- CHANGES.md`

Use the commit dates and version-bump commits to identify the right release.

- [ ] **Step 2: Either fold into the correct version or delete**

If a version is identifiable, append the bullets under that version's `### X.Y.Z` header and remove lines 129-131. Otherwise, delete lines 129-131 entirely.

- [ ] **Step 3: Verify CHANGES.md is well-formed**

Run: `tail -20 CHANGES.md`
Expected: ends with a real version heading and bullet list, no `2.?.?`.

---

## Task 8: Commit cleanup batch and verify suite still green

**Files:** none modified in this task

- [ ] **Step 1: Run the test suites**

Run: `clj -M:test:spec`
Expected: PASS.

Run: `clj -M:test:cljs once`
Expected: PASS.

- [ ] **Step 2: Commit**

```bash
git add -A
git commit -m "remove repo cruft and orphan CHANGES entry

- untrack c3kit-bucket.iml, sqlite_test.db, and .beads/
- move AGENTS.md under .claude/
- delete empty src/clj/c3kit/bucket/sql.clj
- update .gitignore for .beads/
- clean up CHANGES.md 2.?.? orphan"
```

---

## Task 9: Fix malformed `rename-attribute!-` in `migrator.cljc` (TDD)

**Files:**
- Modify: `src/cljc/c3kit/bucket/migrator.cljc:67-70`
- Test: `spec/cljc/c3kit/bucket/migrator_spec.cljc` (or wherever migrator-specific tests live — locate first)

The current body has a stranded vector literal evaluated and discarded as the first body form, copy-pasted from the public arity above.

Current code (`migrator.cljc:67-70`):
```clojure
(defn rename-attribute!-
  "rename-attribute! with explicit db"
  [db kind attr new-kind new-attr]
  [kind attr new-kind new-attr] (-rename-attribute! db kind attr new-kind new-attr))
```

- [ ] **Step 1: Locate the migrator tests**

Run: `find spec -name 'migrator_spec*' -o -name '*migration_spec*'`

If a `migrator_spec` exists, use it. If migrator behavior is currently tested only inside `memory_spec.cljc` (see `:rename-attribute!` test at line 70), create a new file `spec/cljc/c3kit/bucket/migrator_spec.cljc` for the public-arity wrapper test.

- [ ] **Step 2: Write the failing test**

The test must exercise `migrator/rename-attribute!-` (the explicit-db wrapper) and confirm it forwards to the SPI without throwing.

```clojure
;; spec/cljc/c3kit/bucket/migrator_spec.cljc
(ns c3kit.bucket.migrator-spec
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [describe context it should= should-not-throw before]]
            [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as sut]))

(def bibelot
  {:kind  (s/kind :bibelot)
   :id    s/id
   :name  {:type :string}
   :color {:type :string}})

(describe "Migrator public API"
  (context "rename-attribute!- (explicit db)"
    (it "renames an attribute on the given db without throwing"
      (let [db (api/create-db {:impl :memory} [bibelot])]
        (sut/-install-schema! db bibelot)
        (should-not-throw (sut/rename-attribute!- db :bibelot :color :bibelot :hue))))))
```

- [ ] **Step 3: Run the test and verify it fails**

Run: `clj -M:test:spec`
Expected: at least the new `migrator-spec` test FAILs — calling `rename-attribute!-` either throws or yields incorrect behavior because of the stranded vector literal in the body.

- [ ] **Step 4: Fix the function**

Replace the broken definition in `migrator.cljc` with:

```clojure
(defn rename-attribute!-
  "rename-attribute! with explicit db"
  [db kind attr new-kind new-attr]
  (-rename-attribute! db kind attr new-kind new-attr))
```

- [ ] **Step 5: Run the test and verify it passes**

Run: `clj -M:test:spec`
Expected: PASS (including the new `migrator-spec` and no regressions in adjacent specs).

- [ ] **Step 6: Commit**

```bash
git add src/cljc/c3kit/bucket/migrator.cljc spec/cljc/c3kit/bucket/migrator_spec.cljc
git commit -m "fix stranded vector literal in migrator/rename-attribute!-"
```

---

## Task 10: Drop dead `core-file` rename map entry

**Files:**
- Modify: `src/cljc/c3kit/bucket/memory.cljc:2`
- Modify: `src/cljs/c3kit/bucket/re_memory.cljs:2`
- Modify: `src/clj/c3kit/bucket/jdbc.clj:2`

The audit (§3, smell 2) notes these three files contain `(:refer-clojure :rename {find core-file ...})` but `core-file` is referenced nowhere — confirmed by grep. The intended name was almost certainly `core-find` but the rename serves no purpose since no caller uses the renamed binding. Drop the `find core-file` entry from each rename map.

- [ ] **Step 1: Confirm `core-file` is dead across the codebase**

Run: `grep -rn "core-file" src/ spec/ dev/`
Expected: matches only in the three `:refer-clojure` lines themselves — nothing else.

- [ ] **Step 2: Remove `find core-file` from each rename map**

In each of the three files, change:
```clojure
(:refer-clojure :rename {find core-file count core-count reduce core-reduce})
```
to:
```clojure
(:refer-clojure :rename {count core-count reduce core-reduce})
```

- [ ] **Step 3: Run the full test matrix**

Run: `clj -M:test:spec`
Expected: PASS.

Run: `clj -M:test:cljs once`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add src/cljc/c3kit/bucket/memory.cljc src/cljs/c3kit/bucket/re_memory.cljs src/clj/c3kit/bucket/jdbc.clj
git commit -m "drop dead 'find core-file' rename map entry

The renamed binding was never referenced — propagated typo from
an earlier rename that didn't land."
```

---

## Task 11: Replace `println` calls in `seed.cljc` with `log/info` (TDD)

**Files:**
- Modify: `src/cljc/c3kit/bucket/seed.cljc:1-27`
- Modify: `spec/cljc/c3kit/bucket/seed_spec.cljc`

The audit (§3, smell 3) flags the unconditional `println` debug output. Match the rest of the codebase by routing through `c3kit.apron.log` (already imported in `bg.clj`, used everywhere). The existing `seed_spec.cljc` currently asserts on `with-out-str` capture — it must be migrated to `log/capture-logs`.

Current `seed.cljc:20-26`:
```clojure
(if (attrs-match? other-fields e)
  (do
    (println "EXISTS:   " (pr-str kind search-fields))
    (reset! atm e))
  (do
    (println "UPDATING: " (pr-str kind search-fields))
    (reset! atm (db/tx (merge e other-fields)))))
(let [entity (merge {:kind kind} search-fields other-fields)]
  (println "CREATING: " (pr-str kind search-fields))
  (reset! atm (db/tx entity)))
```

Current `seed_spec.cljc:20-38` asserts via `with-out-str`:
```clojure
(should= "CREATING:  :bibelot {:name \"Optimus\"}\n" output)
```

- [ ] **Step 1: Update tests to capture log output (failing test stage)**

Edit `spec/cljc/c3kit/bucket/seed_spec.cljc`. Add `[c3kit.apron.log :as log]` to the requires and rewrite the three `it` forms to use log capture instead of `with-out-str`:

```clojure
(it "creating"
  (let [optimus (sut/entity :bibelot {:name "Optimus"} {})]
    (log/capture-logs
      @optimus
      (should-contain "CREATING:" (log/captured-logs-str))
      (should-contain ":bibelot" (log/captured-logs-str))
      (should-contain "{:name \"Optimus\"}" (log/captured-logs-str)))
    (should-not-be-nil (:id @optimus))))

(it "updating"
  (let [_original (db/tx :kind :bibelot :name "Optimus" :happy? false)
        optimus   (sut/entity :bibelot {:name "Optimus"} {:happy? true})]
    (log/capture-logs
      @optimus
      (should-contain "UPDATING:" (log/captured-logs-str)))
    (should= true (:happy? @optimus))))

(it "exists"
  (let [_original (db/tx :kind :bibelot :name "Optimus" :happy? true)
        optimus   (sut/entity :bibelot {:name "Optimus"} {:happy? true})]
    (log/capture-logs
      @optimus
      (should-contain "EXISTS:" (log/captured-logs-str)))))
```

Add `should-contain` and `should-not-be-nil` to the speclj refer if not already there.

- [ ] **Step 2: Run the test and verify failure**

Run: `clj -M:test:spec`
Expected: the `seed-spec` "creating" / "updating" / "exists" tests FAIL — the production code still uses `println`, so the captured-logs-str will not contain "CREATING:" / "UPDATING:" / "EXISTS:".

- [ ] **Step 3: Update production code**

Edit `src/cljc/c3kit/bucket/seed.cljc`:

```clojure
(ns c3kit.bucket.seed
  (:require [c3kit.apron.log :as log]
            [c3kit.bucket.api :as db]))


(defn attr-matches? [fields entity attr]
  (= (get fields attr)
     (get entity attr)))

(defn attrs-match? [fields entity]
  (every? (partial attr-matches? fields entity) (keys fields)))

(deftype Entity [atm kind search-fields other-fields]
  #?(:clj clojure.lang.IDeref :cljs cljs.core/IDeref)
  (#?(:clj deref :cljs -deref) [_]
    (if-let [e (db/reload @atm)]
      e
      (if-let [e (apply db/ffind-by kind (flatten (seq search-fields)))]
        (if (attrs-match? other-fields e)
          (do
            (log/info "EXISTS:   " (pr-str kind search-fields))
            (reset! atm e))
          (do
            (log/info "UPDATING: " (pr-str kind search-fields))
            (reset! atm (db/tx (merge e other-fields)))))
        (let [entity (merge {:kind kind} search-fields other-fields)]
          (log/info "CREATING: " (pr-str kind search-fields))
          (reset! atm (db/tx entity)))))))

(defn entity
  "Creates a defer-able entity that will, when deref-ed, ensure the entity is in the database and returns the
  currently stored state of the entity.
  search-fields is the minimal set of attributes that identify the entity.
  other-fields is all the rest of the attributes that will be saved in this entity."
  ([kind search-fields] (entity kind search-fields {}))
  ([kind search-fields other-fields] (Entity. (atom nil) kind search-fields other-fields)))
```

- [ ] **Step 4: Run the Clojure suite and verify the seed tests pass**

Run: `clj -M:test:spec`
Expected: PASS (no regressions, including `seed-spec`).

- [ ] **Step 5: Run the ClojureScript suite**

Run: `clj -M:test:cljs once`
Expected: PASS — `seed.cljc` is `.cljc`, so it's exercised in both runtimes if there's CLJS coverage.

- [ ] **Step 6: Commit**

```bash
git add src/cljc/c3kit/bucket/seed.cljc spec/cljc/c3kit/bucket/seed_spec.cljc
git commit -m "route seed.cljc output through log/info

Was unconditional println; now uses c3kit.apron.log so callers can
silence or filter via standard log config."
```

---

## Task 12: Strip `;; TODO - MDM` notes

**Files:**
- Modify: `src/cljc/c3kit/bucket/api.cljc:320-324`
- Modify: `src/clj/c3kit/bucket/migration.clj:250`
- Modify: `src/clj/c3kit/bucket/jdbc.clj:612`

Three TODO notes referencing "MDM" (developer initials). The audit (§3) and §5 item 9 ask these to be removed; track them in issues if still relevant.

- [ ] **Step 1: Delete the api.cljc TODO block**

Remove lines 320-324 from `src/cljc/c3kit/bucket/api.cljc`:

```
;; TODO - MDM:
;;  2) middleware for saving and loading. timestamps is a saving middleware
;;  3) apply to test data.  Bring in entity and for-kind features
;;  4) seeding entity
;;  7) datomic specific features
```

- [ ] **Step 2: Delete the migration.clj TODO**

Remove line 250 from `src/clj/c3kit/bucket/migration.clj`:

```
;; TODO - MDM: Handle name translation here.
```

- [ ] **Step 3: Delete the jdbc.clj TODO**

Remove line 612 from `src/clj/c3kit/bucket/jdbc.clj`:

```
;; TODO - MDM: don't do name translation here, let migration handle it.
```

- [ ] **Step 4: Verify no other `TODO - MDM` references remain**

Run: `grep -rn "TODO - MDM\|TODO-MDM\|TODO MDM" src/`
Expected: no output.

- [ ] **Step 5: Run tests**

Run: `clj -M:test:spec` and `clj -M:test:cljs once`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add src/cljc/c3kit/bucket/api.cljc src/clj/c3kit/bucket/migration.clj src/clj/c3kit/bucket/jdbc.clj
git commit -m "strip TODO-MDM developer notes from public source"
```

---

## Task 13: Clean up `(close [_this] (comment "Nothing to do here"))`

**Files:**
- Modify: `src/cljc/c3kit/bucket/memory.cljc:217`
- Search and modify any analogues across other deftype-implementing files

The audit (§3, smell 4) notes `(close [_this] (comment "Nothing to do here"))` reads as a TODO. The `(comment …)` form returns nil, which is what's wanted, but a clean no-op is clearer.

- [ ] **Step 1: Find all `comment "Nothing to do"` analogues**

Run: `grep -rn 'comment "Nothing to do' src/`

Note all hits — there may be more than just `memory.cljc:217`.

- [ ] **Step 2: Replace each with a clean no-op**

For each hit, replace `(comment "Nothing to do here")` with `nil`. Example for `memory.cljc:217`:

```clojure
;; before
(close [_this] (comment "Nothing to do here"))
;; after
(close [_this] nil)
```

- [ ] **Step 3: Run tests**

Run: `clj -M:test:spec` and `clj -M:test:cljs once`
Expected: PASS — `close` semantics unchanged (still returns nil).

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "replace (comment \"Nothing to do\") with explicit nil in close bodies"
```

---

## Task 14: Add docstrings to `bg.clj` public API

**Files:**
- Modify: `src/clj/c3kit/bucket/bg.clj`

Functions to document (currently undocumented per audit §3 smell 6): `start`, `stop`, `schedule`, `task`, `cancel-task`, `start-scheduled-tasks`, `stop-scheduled-tasks`.

The `service` def is also public — leave it alone (it's a value, not a fn, and is self-explanatory).

- [ ] **Step 1: Read each function and write a docstring**

Read `src/clj/c3kit/bucket/bg.clj` end-to-end. For each function listed above, write a 1-2 line docstring matching the style used in `api.cljc` (see `find` and `tx` for reference). Cover:
- What the fn does
- Required arguments and their meaning
- Return value (only if non-obvious)

Example shape (do not copy verbatim — write fresh based on what each fn does):

```clojure
(defn schedule
  "Register a recurring background task under `key` that runs `task` every
  `period` milliseconds. Persists last-run-at in a :bg-task entity so a restart
  doesn't lose track. Returns the underlying ScheduledFuture."
  [key period ^Runnable task]
  ...)
```

Apply to: `start`, `stop`, `task`, `schedule`, `cancel-task`, `start-scheduled-tasks`, `stop-scheduled-tasks`.

- [ ] **Step 2: Run tests**

Run: `clj -M:test:spec`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add src/clj/c3kit/bucket/bg.clj
git commit -m "document bg.clj public API"
```

---

## Task 15: Add docstrings to `api.cljc` extension SPI

**Files:**
- Modify: `src/cljc/c3kit/bucket/api.cljc`

Functions to document (currently undocumented per audit §3, "Public API surface"): `-create-impl`, `-start-service`, `-stop-service`, `-check-cas!`. These form the contract for backend-plugin authors.

- [ ] **Step 1: Find each fn and write a docstring**

Locate the four functions in `api.cljc` (use `grep -n "defn -create-impl\|defn -start-service\|defn -stop-service\|defn -check-cas!" src/cljc/c3kit/bucket/api.cljc`).

For each, write a docstring covering:
- The fact that it's part of the extension contract (mention that backend authors override or call this)
- What `:impl` keyword dispatch looks like (for `-create-impl` specifically — it's a multimethod)
- Inputs and return shape

Example shape for `-create-impl` (do not copy verbatim — base on actual code):

```clojure
(defmulti -create-impl
  "Extension SPI. Backend authors implement this multimethod (dispatching on
  the `:impl` key of the config map) to construct a DB instance. Returns
  something satisfying the c3kit.bucket.api/DB protocol."
  (fn [config _schemas] (:impl config)))
```

- [ ] **Step 2: Run tests**

Run: `clj -M:test:spec` and `clj -M:test:cljs once`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add src/cljc/c3kit/bucket/api.cljc
git commit -m "document extension SPI in api.cljc"
```

---

## Task 16: Bump VERSION and write CHANGES entry

**Files:**
- Modify: `VERSION`
- Modify: `CHANGES.md`

- [ ] **Step 1: Read current VERSION**

Run: `cat VERSION`
Expected: `2.13.0`.

- [ ] **Step 2: Bump VERSION**

Update `VERSION` to:
```
2.13.1
```

(Ensure no trailing newline issues — match whatever the file already has.)

- [ ] **Step 3: Add CHANGES.md entry**

Insert a new section at the top of `CHANGES.md`, above `### 2.13.0`:

```markdown
### 2.13.1
* Fixed `migrator/rename-attribute!-` (explicit-db arity); previous body had a stranded vector literal that prevented the function from forwarding correctly to the SPI
* Replaced unconditional `println` calls in `c3kit.bucket.seed` with `log/info` so callers can silence them via standard log config
* Dropped the dead `find core-file` entry from `:refer-clojure :rename` in `memory.cljc`, `re_memory.cljs`, and `jdbc.clj`
* Added docstrings to the `c3kit.bucket.bg` public API and to the backend-plugin SPI in `c3kit.bucket.api` (`-create-impl`, `-start-service`, `-stop-service`, `-check-cas!`)
```

- [ ] **Step 4: Run full test suites one more time**

Run: `clj -M:test:spec` and `clj -M:test:cljs once`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add VERSION CHANGES.md
git commit -m "bump version to 2.13.1"
```

---

## Task 17: Open PR 1

**Files:** none modified

- [ ] **Step 1: Push branch**

```bash
git push -u origin audit-response-2.13.1
```

- [ ] **Step 2: Open PR via gh**

```bash
gh pr create --title "audit response 2.13.1: cleanup + code fixes" --body "$(cat <<'EOF'
## Summary

Addresses the Must-fix and Should-fix code/cleanup items from the OSS readiness audit. Documentation work follows in PR 2 as 2.14.0.

### Cleanup
- Untrack `c3kit-bucket.iml`, `sqlite_test.db`, `.beads/`
- Move `AGENTS.md` to `.claude/AGENTS.md`
- Delete empty `src/clj/c3kit/bucket/sql.clj`
- Clean up orphan `2.?.?` CHANGES entry
- Add `.beads/` to `.gitignore`

### Code fixes
- Fix stranded vector literal in `migrator/rename-attribute!-` body
- Drop dead `find core-file` `:refer-clojure :rename` entries
- Replace `println` in `seed.cljc` with `log/info`
- Strip `;; TODO - MDM` notes from `api.cljc`, `migration.clj`, `jdbc.clj`
- Replace `(comment "Nothing to do here")` close bodies with `nil`

### Docs (in-source)
- Docstrings on `bg.clj` public API (`start`, `stop`, `schedule`, `task`, `cancel-task`, `start-scheduled-tasks`, `stop-scheduled-tasks`)
- Docstrings on backend-plugin SPI (`-create-impl`, `-start-service`, `-stop-service`, `-check-cas!`)

## Test plan
- [ ] `clj -M:test:spec` green locally
- [ ] `clj -M:test:cljs once` green locally
- [ ] CI green
- [ ] After merge: tag and deploy 2.13.1 to Clojars
EOF
)"
```

- [ ] **Step 3: Wait for CI green, then merge and deploy**

After CI passes and the PR merges to master:

```bash
git checkout master
git pull
clj -T:build deploy
```

This runs `tag` (which pushes the `2.13.1` git tag) and `aether/deploy` (which uploads to Clojars). Requires `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` env vars (per current README).

---

# PR 2 — Documentation overhaul (ships as 2.14.0)

PR 2 only begins after PR 1 has merged AND `2.13.1` is on Clojars.

---

## Task 18: Branch setup for PR 2

**Files:** none modified

- [ ] **Step 1: Update master and create branch**

```bash
git checkout master
git pull
git checkout -b audit-response-2.14.0
```

- [ ] **Step 2: Verify tests still pass on the fresh branch**

Run: `clj -M:test:spec` and `clj -M:test:cljs once`
Expected: PASS.

---

## Task 19: Survey Datomic namespaces; decide on `docs/datomic-guide.md`

**Files:** none yet (decision-only)

The spec defers the decision: a Datomic guide is created **only** if the Datomic namespaces show unique behavior beyond the unified API worth documenting.

- [ ] **Step 1: Read the Datomic namespaces**

Read in full:
- `src/clj/c3kit/bucket/datomic.clj` (on-prem peer)
- `src/clj/c3kit/bucket/datomic_cloud.clj` (Cloud client)
- `src/clj/c3kit/bucket/datomic_common.clj` (shared)

Note any of: peer/client connection setup, schema attribute syntax that differs from JDBC, transaction semantics that diverge from the unified API, when-to-pick-on-prem-vs-cloud guidance, configuration knobs not exposed via the unified `db/*` API.

- [ ] **Step 2: Decide**

If the unique-behavior content adds up to **a meaningful page** (~300+ words across multiple non-trivial sections), proceed with Task 22 (write `docs/datomic-guide.md`).

If it's less, skip Task 22; the README's "Supported backends" table will note "On-prem peer; see `c3kit.bucket.datomic` namespace docstring" and that's enough.

Record the decision (in a comment on the branch, in the eventual PR description, or just in the commit message of Task 22 / its absence). The decision affects what links the README needs in Task 21.

---

## Task 20: Write `docs/migrations-guide.md`

**Files:**
- Create: `docs/migrations-guide.md`

The audit (§1, README) calls out that the migration system is undocumented except for a CLI usage string buried in `defn migrate`.

- [ ] **Step 1: Read the migration source**

Read `src/clj/c3kit/bucket/migration.clj` end-to-end. Note:
- The CLI usage string in `defn migrate`
- File-naming convention for migration scripts (look at `spec/clj/c3kit/bucket/migration_samples/` for examples — `20230101.clj`, `20230202.clj`)
- The `up` / `down` shape (`spec/clj/c3kit/bucket/migration_samples/20230101.clj` shows it: `(defn up [] ...)` and `(defn down [] ...)`)
- `MigrationApi` protocol (if any) and which backends implement it
- How rollback works (or whether `down` is invoked automatically)

- [ ] **Step 2: Draft the guide**

Suggested structure for `docs/migrations-guide.md`:

```markdown
# Migrations

Bucket includes a versioned migration runner in `c3kit.bucket.migration`. It works against any backend whose impl supports the migrator SPI.

## Writing a migration

Create a file under your migrations directory named with a sortable timestamp prefix, e.g. `20260427_add_widgets_kind.clj`:

(show example based on migration_samples/)

## Running migrations from the CLI

(lift the CLI usage string out of `defn migrate` and explain each option)

## Rollback

(describe what `down` does and how the runner invokes it, based on what migration.clj actually does)

## Programmatic use

(show a `c3kit.bucket.migration` REPL example for tests / dev seeds)
```

Length target: 100-300 lines, in the same voice as `docs/indexeddb-guide.md`.

- [ ] **Step 3: Commit**

```bash
git add docs/migrations-guide.md
git commit -m "add migrations guide"
```

---

## Task 21: Conditionally write `docs/datomic-guide.md`

**Skip this task entirely if Task 19 decided no guide is warranted.**

**Files:**
- Create: `docs/datomic-guide.md`

- [ ] **Step 1: Outline based on the survey from Task 19**

Likely sections (omit any that don't apply):
- When to pick on-prem peer vs Cloud client
- Connection configuration (connection string for on-prem; client config for Cloud)
- Schema attribute syntax differences from the unified `c3kit.apron.schema`
- Transaction semantics (entity IDs, datoms, retracts) where they leak through
- Datomic-specific search features (e.g., the audit's mention of "datomic q, find-datalog, squuid" in CHANGES 2.1.2)
- Limitations or gotchas

- [ ] **Step 2: Write the guide**

Match the voice and structure of `docs/indexeddb-guide.md`. Length target: as long as it needs to be — quality over volume; don't pad.

- [ ] **Step 3: Commit**

```bash
git add docs/datomic-guide.md
git commit -m "add datomic guide"
```

---

## Task 22: Write `CONTRIBUTING.md`

**Files:**
- Create: `CONTRIBUTING.md`

Light boilerplate per the spec.

- [ ] **Step 1: Draft `CONTRIBUTING.md`**

```markdown
# Contributing to c3kit-bucket

Thanks for your interest. This is a small library and contributions are welcome.

## Branching

Branch off `master`. PRs require green CI before merge.

## Running tests locally

```bash
# Clojure tests (most of the suite)
clj -M:test:spec

# Auto-runner — re-runs on save
clj -M:test:spec -a

# ClojureScript tests
clj -M:test:cljs once

# Auto-runner
clj -M:test:cljs

# IndexedDB integration tests (used in CI)
clojure -M:test:idb-integration
```

## Database infrastructure for the full SQL suite

The JDBC tests are exercised in CI against real databases. To run them locally you need:

- **PostgreSQL** — with a `test` database. See README "Development" for setup.
- **Microsoft SQL Server** — typically run via Docker (`mcr.microsoft.com/mssql/server`).
- **SQLite** — usually pre-installed on macOS/Linux.
- **sqlite-vec** — load extension manually; CI installs it via package manager.

Skipping these means parts of `jdbc_spec.clj` will be skipped or fail with connection errors. That's OK for most contributions; CI will exercise the full matrix.

## Updating CHANGES.md

Every consumer-visible change should add a bullet to `CHANGES.md` under the appropriate version header.

## Code style

The codebase aims for idiomatic Clojure with conventions visible in nearby code. There's no project-wide formatter configured.

## Releasing (maintainers only)

1. Acquire a Clojars deploy token at https://clojars.org/tokens with appropriate scope.
2. Set environment variables:
   ```
   CLOJARS_USERNAME=<your username>
   CLOJARS_PASSWORD=<your deploy token>
   ```
3. Bump `VERSION` and add the CHANGES.md entry on master.
4. Run `clj -T:build deploy` — this tags the release in git and uploads to Clojars.

You must be a member of the Clojars group `com.cleancoders.c3kit` to deploy.
```

- [ ] **Step 2: Commit**

```bash
git add CONTRIBUTING.md
git commit -m "add CONTRIBUTING.md"
```

---

## Task 23: Rewrite `README.md`

**Files:**
- Modify (full rewrite): `README.md`

Replaces the current 56-line README with the structure agreed in the spec.

- [ ] **Step 1: Replace the README contents**

Write the new README per the structure below. Keep the existing logo image, badge, and Alger quote.

````markdown
# Bucket

![Bucket](https://github.com/cleancoders/c3kit/blob/master/img/bucket_200.png?raw=true)

A library component of [c3kit - Clean Coders Clojure Kit](https://github.com/cleancoders/c3kit).

_"Most men give advice by the bucket, but take it by the grain."_ - William R. Alger

[![Bucket Build](https://github.com/cleancoders/c3kit-bucket/actions/workflows/test.yml/badge.svg)](https://github.com/cleancoders/c3kit-bucket/actions/workflows/test.yml)

## What is bucket?

Bucket is a unified entity-storage API for Clojure and ClojureScript. The same domain code can read and write against Datomic, JDBC (Postgres / H2 / MSSQL / SQLite, with optional `pgvector` and `sqlite-vec` for embeddings), an in-memory store on the JVM, a Reagent-aware in-memory store in the browser, or IndexedDB for browser-side persistence.

Define your schemas once with [c3kit-apron](https://github.com/cleancoders/c3kit-apron); transact, query, and reload entities through the same `c3kit.bucket.api` namespace regardless of backend.

## Installation

`deps.edn`:

```clojure
{:deps {com.cleancoders.c3kit/bucket {:mvn/version "2.14.0"}}}
```

Leiningen:

```clojure
[com.cleancoders.c3kit/bucket "2.14.0"]
```

## Hello World

```clojure
(require '[c3kit.apron.schema :as s]
         '[c3kit.bucket.api :as db])

;; Define a schema
(def widget
  {:kind  (s/kind :widget)
   :id    s/id
   :name  {:type :string}
   :color {:type :keyword}})

;; Create an in-memory db and install it as the default impl
(def my-db (db/create-db {:impl :memory} [widget]))
(reset! db/impl my-db)

;; Save an entity
(db/tx {:kind :widget :name "Sprocket" :color :red})
;; => {:kind :widget :id 1 :name "Sprocket" :color :red}

;; Find by attribute
(db/find-by :widget :name "Sprocket")
;; => [{:kind :widget :id 1 :name "Sprocket" :color :red}]

;; Look up by id
(db/entity :widget 1)
;; => {:kind :widget :id 1 :name "Sprocket" :color :red}
```

The same code shape works against any supported backend — only the `{:impl ...}` config in `create-db` changes.

## Supported backends

| Impl key        | Platform | Notes                                               | Guide |
|-----------------|----------|-----------------------------------------------------|-------|
| `:memory`       | CLJ/CLJS | In-process, ephemeral                               | —     |
| `:re-memory`    | CLJS     | Reagent-aware in-memory                             | —     |
| `:jdbc`         | CLJ      | Postgres, H2, MSSQL, SQLite (+ pgvector, sqlite-vec) | —     |
| `:datomic`      | CLJ      | Datomic on-prem (peer)                              | (link if datomic-guide.md exists) |
| `:datomic-cloud`| CLJ      | Datomic Cloud (client)                              | (link if datomic-guide.md exists) |
| `:indexeddb`    | CLJS     | Browser persistent storage                          | [docs/indexeddb-guide.md](docs/indexeddb-guide.md) |
| `:re-indexeddb` | CLJS     | Reagent-aware IndexedDB                             | [docs/indexeddb-guide.md](docs/indexeddb-guide.md) |

## Core concepts

### Schemas

Schemas are plain maps validated by [c3kit.apron.schema](https://github.com/cleancoders/c3kit-apron). Every schema has a `:kind` (the entity type), an `:id` field, and one or more attribute definitions:

```clojure
(def widget
  {:kind  (s/kind :widget)
   :id    s/id
   :name  {:type :string}
   :color {:type :keyword}})
```

### The `DB` protocol

Every backend implements `c3kit.bucket.api/DB`. You don't usually call protocol methods directly — the `db/*` functions (`db/tx`, `db/find-by`, `db/entity`, `db/reload`, etc.) wrap the protocol and provide a stable consumer API.

If you maintain multiple databases simultaneously (e.g., a primary store and an audit log), each `db/*` function has a `db/X-` "explicit-db" variant that takes the impl as its first argument.

## Migrations

Bucket ships a versioned migration runner. See [docs/migrations-guide.md](docs/migrations-guide.md).

## Background tasks

`c3kit.bucket.bg` provides scheduled-task management with persisted last-run-at tracking, useful for periodic jobs that survive restarts. See the docstrings in `src/clj/c3kit/bucket/bg.clj`.

## Development

```bash
# JVM tests
clj -M:test:spec
clj -M:test:spec -a            # auto runner

# ClojureScript tests
clj -M:test:cljs once
clj -M:test:cljs               # auto runner
```

The full SQL suite requires PostgreSQL, MSSQL, SQLite, and `sqlite-vec`. See [CONTRIBUTING.md](CONTRIBUTING.md#database-infrastructure-for-the-full-sql-suite) for setup.

PostgreSQL local setup:

```bash
sudo -u postgres createuser $(whoami)
sudo -u postgres createdb test
sudo -u postgres psql -d test -c "GRANT ALL ON SCHEMA public TO PUBLIC;"
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

MIT — see [LICENSE](LICENSE).
````

If Task 19 decided to create `docs/datomic-guide.md`, replace `(link if datomic-guide.md exists)` in the table with `[docs/datomic-guide.md](docs/datomic-guide.md)` for both Datomic rows. Otherwise, replace it with `—`.

- [ ] **Step 2: Verify the Hello World snippet actually runs**

Open a REPL: `clj`

Paste the Hello World code line by line. Confirm:
- The schema definition compiles
- `db/create-db` and `reset!` succeed
- `db/tx` returns a map with an `:id`
- `db/find-by` returns a vector containing that entity
- `db/entity` returns the same map

If any step fails, fix the snippet in the README to match the actual API. Common pitfalls:
- `s/id` vs `c3kit.apron.schema/id` — the snippet aliases `:as s`, so `s/id` should resolve.
- `db/impl` is an atom; `reset!` is correct usage. Confirm with `(:impl @db/impl)` or similar.
- `db/find-by` may return the result in a different shape than shown — adjust the comment to reality.

- [ ] **Step 3: Commit**

```bash
git add README.md
git commit -m "rewrite README for OSS consumers

- Add installation snippet (deps.edn + lein)
- Add Hello World example using the unified API
- Add supported-backends matrix table
- Add core concepts section (schemas, DB protocol)
- Link to docs/indexeddb-guide.md, docs/migrations-guide.md
- Move Clojars deploy instructions to CONTRIBUTING.md
- Drop stale namespace bullet list (referenced files that no longer exist)"
```

---

## Task 24: Bump VERSION and write CHANGES entry for 2.14.0

**Files:**
- Modify: `VERSION`
- Modify: `CHANGES.md`

- [ ] **Step 1: Bump VERSION**

Update `VERSION` to:
```
2.14.0
```

- [ ] **Step 2: Add CHANGES entry**

Insert a new section at the top of `CHANGES.md`, above `### 2.13.1`:

```markdown
### 2.14.0
* Documentation overhaul: rewritten README with installation snippet, "Hello World" example, supported-backends matrix, and core concepts
* New `docs/migrations-guide.md` covering the migration runner, file-naming convention, CLI usage, and rollback semantics
* New `CONTRIBUTING.md` covering local test setup, infra requirements for the full SQL suite, and maintainer release instructions
```

If a Datomic guide was created in Task 21, add a fourth bullet:

```markdown
* New `docs/datomic-guide.md` covering on-prem-vs-Cloud trade-offs, connection setup, and Datomic-specific schema and query features
```

- [ ] **Step 3: Run tests one more time**

Run: `clj -M:test:spec` and `clj -M:test:cljs once`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add VERSION CHANGES.md
git commit -m "bump version to 2.14.0"
```

---

## Task 25: Open PR 2

**Files:** none modified

- [ ] **Step 1: Push branch**

```bash
git push -u origin audit-response-2.14.0
```

- [ ] **Step 2: Open PR**

```bash
gh pr create --title "audit response 2.14.0: docs overhaul" --body "$(cat <<'EOF'
## Summary

Documentation overhaul addressing the README weaknesses identified in the OSS readiness audit. No code changes.

- README rewrite: installation, Hello World, supported-backends matrix, core concepts, links to all docs
- New `docs/migrations-guide.md`
- New `CONTRIBUTING.md` (boilerplate; absorbs Clojars deploy instructions previously in README)
- New `docs/datomic-guide.md` (only if the Datomic survey in Task 19 found enough unique behavior to document)

## Test plan
- [ ] CI green
- [ ] README Hello World snippet manually verified in a REPL
- [ ] All doc links resolve
- [ ] After merge: tag and deploy 2.14.0 to Clojars
EOF
)"
```

- [ ] **Step 3: After CI green and merge, deploy**

```bash
git checkout master
git pull
clj -T:build deploy
```

---

# Summary of audit items addressed

| Audit item | Tier | Task |
|---|---|---|
| 1. README rewrite | Must | Task 23 |
| 2. Remove `c3kit-bucket.iml`, `sqlite_test.db` from tracking | Must | Tasks 2, 3 |
| 3. Fix `migrator.cljc:67-70` malformed fn | Must | Task 9 |
| 4. Fix `core-file` rename map | Must | Task 10 |
| 5. Replace `println` in `seed.cljc` with `log/info` | Must | Task 11 |
| 6. Decide on `AGENTS.md` and `.beads/` | Must | Tasks 4, 5 |
| 7. Docstrings on `bg.clj` and SPI | Should | Tasks 14, 15 |
| 8. Per-backend usage docs | Should | Tasks 19, 21, 23 (matrix in README) |
| 9. Strip `;; TODO - MDM` notes | Should | Task 12 |
| 10. Clean up `2.?.?` CHANGES entry | Should | Task 7 |
| 11. Empty `sql.clj` — remove | Should | Task 6 |
| 13. CONTRIBUTING.md | Nice | Task 22 |
| Smell 4 — `(comment "Nothing to do here")` | — | Task 13 |
| Migration system docs (audit §1, README) | Must (subitem) | Task 20 |

Items deliberately not addressed (out of scope per spec):
- Item 12: clj-kondo / cljfmt in CI
- Item 14: coverage report and version-matrix table
- Architecture observation: `MemoryDB` / `ReMemoryDB` consolidation
- Region-comment style sweep across the codebase
