# c3kit-bucket Audit Response — Design

**Date:** 2026-04-27
**Source:** `~/Desktop/c3kit-bucket-audit.md` (audit of `master @ 58c28e7`, version 2.13.0)
**Goal:** Address every applicable point in the audit, moving the repo from "internal tool published to Clojars" to "library a stranger would feel comfortable adopting."

---

## Scope

In scope:

- All audit Must-fix items (1–6)
- All audit Should-fix items (7–11)
- Audit Nice-to-have item 13 (CONTRIBUTING.md)

Out of scope:

- Audit Nice-to-have item 12 (clj-kondo + cljfmt in CI)
- Audit Nice-to-have item 14 (coverage reporting; the version-matrix sub-item is also dropped)
- Architecture observation: `MemoryDB`/`ReMemoryDB` consolidation (audit explicitly calls this not urgent and excluded from punch list)

---

## Delivery shape

Two PRs, two release windows.

### PR 1 — Cleanup + code fixes (ships as 2.13.1 patch release)

Combines repo hygiene with the small bug fixes the audit identified. Every code change is TDD-driven (failing test first, per project policy).

### PR 2 — Documentation & contributor materials (ships as 2.14.0 minor release)

The README rewrite, supporting docs, and CONTRIBUTING.md. No code changes. Builds on PR 1 so the README's installation snippet can reference 2.14.0.

PR 1 must merge and ship before PR 2 is started.

---

## PR 1 — Cleanup + code fixes (2.13.1)

### Repo hygiene

| Action | File | Notes |
|---|---|---|
| Untrack | `c3kit-bucket.iml` | Already in `.gitignore` pattern (`*.iml`); was committed before that rule. |
| Untrack | `sqlite_test.db` | Already in `.gitignore`; orphaned 0-byte file. |
| Untrack | `.beads/` (entire directory) | Stop public mirroring of internal issue tracker. Add `.beads/` to `.gitignore`. |
| Move | `AGENTS.md` → `.claude/AGENTS.md` | Beads-onboarding instructions; not for public consumers. |
| Delete | `src/clj/c3kit/bucket/sql.clj` | Empty `ns` form, no placeholder reason. |
| Edit | `CHANGES.md` | Remove the orphaned `2.?.?` entry at the bottom (line 129). |
| Verify | `.gitignore` | Confirm it now excludes everything just untracked. |

### Code fixes (TDD)

Each item below: failing test first, then fix.

1. **`migrator.cljc:67-70` malformed `rename-attribute!-`.** The body has a stranded vector literal `[kind attr new-kind new-attr]` evaluated and discarded as the first form, copy-pasted from the public arity above. Test exercises `rename-attribute!-` with an explicit db. Home: existing `migrator-spec`.

2. **`core-file` → `core-find` rename in `:refer-clojure :rename` maps.** Affected files: `memory.cljc:2`, `re_memory.cljs:2`, `jdbc.clj:2`. The audit notes none of these files reference `core-file` anywhere — the rename is dead. Approach: grep for `core-file` across `src/` and `spec/` to confirm dead. If confirmed unused, drop the `find core-file` entry from the rename map entirely (don't rename to `core-find` if no caller needs it). If a caller exists, fix to `core-find`. No new test needed if the existing suite passes after the change.

3. **`seed.cljc:20-26` `println` → `log/info`.** The audit notes the rest of the codebase uses a `log` namespace; verify which one (likely `c3kit.apron.log`) and adopt it. Test: assert `log/info` is invoked for the EXISTS / UPDATING / CREATING paths. Check `seed_spec.cljc` for prior art on log capture; if no pattern exists, follow whatever pattern other namespaces use for log testing.

4. **Strip `;; TODO - MDM` notes.** Files: `api.cljc` (TODO block at lines 320-324), `jdbc.clj`, `migration.clj`. No tests; pure deletion.

5. **`memory.cljc:217` and analogues.** Replace `(close [_this] (comment "Nothing to do here"))` with a clean no-op (`nil` body or empty body, whichever the deftype protocol allows). Audit notes the `(comment …)` form reads as a TODO. No new test; existing `close` tests cover behavior.

### Docstrings

No tests required for docstring additions. Targets:

- **`bg.clj` public API:** `start`, `stop`, `schedule`, `cancel-task`, `task`, `start-scheduled-tasks`, `stop-scheduled-tasks`. Each gets one short docstring matching the conventions used elsewhere in the codebase (e.g., `api.cljc`'s `find` docstring style).
- **Extension API in `api.cljc`:** `-create-impl`, `-start-service`, `-stop-service`, `-check-cas!`. These are part of the contract for backend authors; document inputs, expected return, and how they're dispatched.

### Release

- Bump `VERSION` to `2.13.1`.
- Add `CHANGES.md` entry summarizing the bug fixes and docstring additions. Do not list cleanup items (untracking files isn't a consumer-visible change).
- Standard Clojars deploy after merge.

---

## PR 2 — Documentation & contributor materials (2.14.0)

### README.md rewrite

Replaces the current 56-line README. Structure:

1. **Title, logo, badge, tagline** — keep the William R. Alger quote.
2. **What is bucket** — one paragraph: a unified API for entity storage across multiple backends (Datomic, JDBC, in-memory, IndexedDB), so the same domain code runs on the JVM, in browser memory, or against a real database.
3. **Installation** — Clojars coordinates with both `deps.edn` and Leiningen blocks; reference the version this PR ships (`2.14.0`).
4. **Quick start ("Hello World")** — single working example using the unified API: define a kind via `c3kit.apron.schema`, `api/create-db` against `:memory`, `db/tx`, `db/find-by`, `db/entity`. The same code shape would work against any backend.
5. **Supported backends** — table:

   | Impl key | Platform | Notes | Guide |
   |---|---|---|---|
   | `:memory` | CLJ/CLJS | In-process, ephemeral | — |
   | `:re-memory` | CLJS | Reagent-aware in-memory | — |
   | `:jdbc` | CLJ | Postgres, H2, MSSQL, SQLite (+ pgvector, sqlite-vec) | — |
   | `:datomic` | CLJ | On-prem peer | (link if guide is created) |
   | `:datomic-cloud` | CLJ | Datomic Cloud | (link if guide is created) |
   | `:indexeddb` | CLJS | Browser persistent storage | [docs/indexeddb-guide.md](docs/indexeddb-guide.md) |
   | `:re-indexeddb` | CLJS | Reagent-aware IndexedDB | [docs/indexeddb-guide.md](docs/indexeddb-guide.md) |

6. **Core concepts** — short subsections:
   - Schemas (link to `c3kit.apron.schema`, brief `(s/kind :widget)` example)
   - Entity IDs and kinds
   - The `DB` protocol (one sentence: all backends implement this; consumers mostly use the `db/*` wrapper functions)
7. **Migrations** — short section, link to `docs/migrations-guide.md`.
8. **Background tasks** — short paragraph on `c3kit.bucket.bg`, references the docstrings added in PR 1.
9. **Development** — keep the Clojure/CLJS test command blocks; add the Postgres + MSSQL + sqlite-vec note for the full SQL suite; add the IDB integration runner. Move maintainer-only Clojars deploy instructions to `CONTRIBUTING.md`.
10. **Contributing** — one-line pointer to `CONTRIBUTING.md`.
11. **License** — one line: MIT, see LICENSE.

Explicitly excluded from the README:

- The current "stale list of namespaces" bullets (the audit notes three of the listed filenames don't exist in this repo).
- Tested-against-versions matrix (out of scope).
- Deep extension docs ("extending bucket"); the docstring pass in PR 1 is sufficient for now.

### docs/migrations-guide.md (new)

Short, focused. Sections:

- What it is (versioned migration scripts, CLI-driven)
- How to write a migration (file naming convention, expected shape)
- CLI usage — the string currently buried in `defn migrate` lifted out and explained
- What rollback support looks like — content TBD during write-up based on what `c3kit.bucket.migration` actually supports

### docs/datomic-guide.md (conditional)

Created **only** if a survey of `datomic.clj`, `datomic_cloud.clj`, `datomic_common.clj` during PR 2 work surfaces unique behavior beyond the unified API worth documenting (e.g., when to pick on-prem vs cloud, schema attribute syntax differences, peer/client connection setup, transaction semantics that diverge from JDBC).

If the unique-behavior content is less than a meaningful page, fold a short setup snippet into the README backend table instead and skip the guide. Decision deferred to during PR 2 work; not predetermined.

The guiding principle: the unified API is what users learn; per-backend guides exist only when there's genuinely unique behavior the unified API can't paper over (IndexedDB qualifies; SQLite-vs-Postgres connection config does not).

### CONTRIBUTING.md (new)

Light boilerplate:

- How to branch (off `master`)
- How to run the test suites locally (Clojure, ClojureScript, IDB integration)
- Expectation that PRs add a `CHANGES.md` entry
- Infra required for the full SQL suite (Postgres + MSSQL + SQLite + sqlite-vec)
- Maintainer-only Clojars release instructions (moved here from current README)

### Release

- Bump `VERSION` to `2.14.0`.
- Add `CHANGES.md` entry summarizing the documentation overhaul.
- Standard Clojars deploy after merge.

---

## Decisions captured

- **AGENTS.md / .beads/ disposition:** Remove `.beads/` entirely; move `AGENTS.md` to `.claude/`.
- **Per-backend doc structure:** Separate guide files only when a backend has genuinely unique behavior beyond the unified API. IndexedDB qualifies. JDBC dialects do not. Datomic is TBD during PR 2.
- **PR sequencing:** Two PRs (cleanup+fixes, then docs). Two releases (2.13.1, 2.14.0).
- **`seed.cljc` printlns:** Swap to `log/info` (matches rest of codebase).
- **Region comments style:** No project-wide adoption in this scope. The IDB files keep them; other files are not changed. Region-comment expansion is a separate refactor.
- **CONTRIBUTING.md scope:** Light boilerplate. Not a substantive contributor handbook.
- **`sql.clj` empty file:** Delete.
- **`MemoryDB`/`ReMemoryDB` consolidation:** Out of scope.

---

## Out-of-scope items (from the audit)

For the record, these audit observations are deliberately not addressed:

- clj-kondo / cljfmt in CI
- Coverage reporting and version-matrix table
- `MemoryDB`/`ReMemoryDB` deftype consolidation
- A full "extending bucket" guide (docstrings on the SPI are the minimum bar for this round)
- Region-comment style sweep across the codebase
