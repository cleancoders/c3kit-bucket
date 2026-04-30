# Contributing to c3kit-bucket

c3kit-bucket is a small Clojure/ClojureScript library providing a unified data API across Datomic, SQL databases, IndexedDB, and in-memory stores. Contributions are welcome — keep PRs focused and the scope small.

## Workflow

**All pull requests must be linked to an open issue.** PRs without a linked issue will be auto-closed without review by the [`require-linked-issue`](./.github/workflows/require-linked-issue.yml) workflow. Open (or find) an issue first, get a thumbs-up from a maintainer, then start work. This protects everyone's time — yours and ours.

1. Open or find an issue describing the bug or proposed change. Wait for maintainer acknowledgement before starting work on anything non-trivial.
2. Branch off `master`.
3. Use TDD — write a failing spec first, then the minimum code to make it pass, then refactor.
4. Keep commits small and focused.
5. Update `CHANGES.md` with a one-line entry under the relevant version header (skip for doc-only or repo cleanup).
6. Reference the issue with `Closes #N` (or `Fixes #N` / `Resolves #N`) in your PR description so the linked-issue check passes and the issue auto-closes on merge.
7. Ensure CI is green before requesting review.

## Running tests locally

### Clojure

```bash
clj -M:test:spec          # run once
clj -M:test:spec -a       # auto runner
```

CI runs with `:spec-ci` (no `-t ~slow` filter) — use that when you want the full suite:

```bash
clj -M:test:spec-ci
```

### ClojureScript

```bash
clj -M:test:cljs once     # run once
clj -M:test:cljs          # auto runner
```

### IndexedDB integration

```bash
clojure -M:test:idb-integration
```

This launches a headless browser runner. It requires Node/npm tooling on your PATH (handled automatically in CI).

## Database infrastructure for the full SQL suite

CI runs PostgreSQL, MSSQL, and SQLite. Without local instances the JDBC specs will be skipped or will fail.

**PostgreSQL**

```bash
sudo -u postgres createuser $(whoami)
sudo -u postgres createdb test
sudo -u postgres psql -d test -c "GRANT ALL ON SCHEMA public TO PUBLIC;"
```

**MSSQL**

Run via Docker:

```bash
docker run -e "ACCEPT_EULA=Y" -e "SA_PASSWORD=Pala2023" \
  -p 1433:1433 \
  mcr.microsoft.com/mssql/server:2019-CU27-ubuntu-20.04
```

Then create the database:

```bash
sqlcmd -S localhost -U sa -P Pala2023 -Q "CREATE DATABASE bucketTest" -C
```

**SQLite**

SQLite is usually pre-installed. The `sqlite-vec` extension is required for vector tests:

```bash
pip install sqlite-vec
export SQLITE_VEC_PATH=$(python3 -c 'import sqlite_vec; print(sqlite_vec.loadable_path())')
```

## Updating CHANGES.md

Every consumer-visible change — new features, bug fixes, behaviour changes — gets a bullet under the relevant version header in `CHANGES.md`. Pure repo cleanup or doc-only changes don't need an entry.

## Code style

No project-wide formatter. Aim for idiomatic Clojure consistent with the surrounding code. See the formatting notes in the project's internal style docs if in doubt.

## Releasing (maintainers only)

You must be a member of the Clojars group `com.cleancoders.c3kit`.

1. Go to <https://clojars.org/tokens> and create a deploy token scoped to the group.
2. Export credentials:

```bash
export CLOJARS_USERNAME=<your username>
export CLOJARS_PASSWORD=<your deploy token>
```

3. Update the `VERSION` file.
4. Deploy:

```bash
clj -T:build deploy
```

This tags the commit, builds the jar, and pushes to Clojars in one step.
