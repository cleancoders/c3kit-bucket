# Migrations

`c3kit.bucket.migration` runs versioned migration scripts against your database. Each migration is a Clojure namespace with `up` and `down` functions. The runner compares what's on disk against what has been applied (tracked as `:migration` entities in your DB) and runs whatever is needed to reach the target.

## Writing a Migration

### File Location and Naming

Migrations live in a dedicated namespace directory. Set `:migration-ns` in your bucket config to point to it:

```clojure
{:impl         :datomic
 :migration-ns "my.app.migrations"}
```

The runner scans `my/app/migrations/` on the classpath for files matching `[0-9]{8}.*\.clj` — eight leading digits followed by any suffix. Files are sorted lexicographically, so the digits act as a timestamp. The recommended convention is `YYYYMMDD` optionally followed by a short description.

When you need multiple migrations on the same date, append a letter suffix (`a`, `b`, `c`, …) directly after the digits so lex order matches the intended run order:

```
my/app/migrations/
  20230101.clj
  20230202-add-user-email.clj
  20230303a-drop-legacy-kind.clj
  20230303b-add-replacement-kind.clj
```

Files without eight leading digits don't match the pattern and aren't picked up by the runner.

### Migration Script Shape

Each migration file defines two zero-argument functions: `up` and `down`.

```clojure
(ns my.app.migrations.20230202-add-user-email
  (:require [c3kit.bucket.migrator :as migrator]
            [my.app.schema :as schema]))

(defn up []
  (migrator/install-schema! schema/user)
  (migrator/add-attribute! :user :email {:type :string}))

(defn down []
  (migrator/remove-attribute! :user :email))
```

- `up` is called when migrating forward to this version.
- `down` is called when rolling back past this version.
- Both functions receive no arguments and return nothing meaningful.
- Both are required by convention. The runner resolves `<migration-ns>.<name>/up` and `<migration-ns>.<name>/down` at runtime; a missing `down` will cause a runtime error if rollback is attempted.

Use the `c3kit.bucket.migrator` API inside migration functions:

| Function | Description |
|----------|-------------|
| `migrator/install-schema!` | Creates a new kind in the database schema |
| `migrator/add-attribute!` | Adds an attribute to an existing kind |
| `migrator/remove-attribute!` | Removes an attribute (and all its values) |
| `migrator/rename-attribute!` | Renames an attribute within a kind |

### Going Deeper

For more substantive migrations — multi-step data transforms, cross-kind moves, performance considerations on large tables — read [Facing Migrations With Respect](https://cleancoders.com/blog/2025-02-13-facing-migrations-with-respect). It covers the conventions and discipline this project's migrations follow.

If you're working in Claude Code, the [`writing-migrations` skill](https://github.com/cleancoders/agent-plugins/blob/master/plugins/clojure/skills/writing-migrations/SKILL.md) in the `clojure` plugin walks you through the same conventions step-by-step.

## Running Migrations

### From the CLI

Add a `:migrate` alias to your `deps.edn` that points to `c3kit.bucket.migration/-main` and starts your app services (including the bucket service):

```clojure
:migrate {:main-opts ["-m" "my.app.migrate"]}
```

Your entry point namespace starts the bucket service then delegates to `c3kit.bucket.migration/migrate`:

```clojure
(ns my.app.migrate
  (:require [c3kit.bucket.api :as db]
            [c3kit.bucket.migration :as migration]))

(defn -main [& args]
  (app/start! [db/service])
  (apply migration/migrate args))
```

The built-in usage string (from `migration/migrate`):

```
**** c3kit migration USAGE:

clj -Mmigrate:test [command | migration-name] [preview]

 Commands:
   help:  prints this message
   sync:  synchronize the database schema with :full-schema-var
   list:  show available and when they were applied
 migration-name - e.g. 20220806-first
   migrations will be applied UP or DOWN to make the named migration the latest applied
 no arguments will migrate UP to latest migration
 preview: migrations/syncing will not update the database
```

| Invocation | Behavior |
|------------|----------|
| `clj -M:migrate` | Migrate up to the latest available migration |
| `clj -M:migrate 20230202-add-user-email` | Migrate up or down so that `20230202-add-user-email` is the latest applied |
| `clj -M:migrate list` | Print all available migrations and when each was applied |
| `clj -M:migrate sync` | Run schema sync (see [Schema Sync](#schema-sync)) |
| `clj -M:migrate help` | Print the usage string |
| `clj -M:migrate <any command> preview` | Dry-run: log what would happen but do not write to the database |

### From the REPL or Tests

`migrate!` is the public programmatic entry point. It reads `:migration-ns` from the config you pass and requires the bucket DB impl to be present:

```clojure
(require '[c3kit.bucket.migration :as migration])

;; migrate up to latest
(migration/migrate! config)

;; migrate to a specific version
(migration/migrate! config "20230202-add-user-email")
```

`config` must contain:
- `:-db` — the bucket DB impl (e.g., `@db/impl`)
- `:migration-ns` — namespace string where migration files live

When using the app service system, `migrate!` can be called with no arguments and will pull config from `app/app`:

```clojure
(migration/migrate!)
```

`c3kit.bucket.migration/service` is a ready-made app service that calls `migrate!` on start. Add it to your service startup sequence to run migrations automatically at boot:

```clojure
(app/start! [db/service migration/service])
```

## Schema Sync

Schema sync (`sync-schemas!`) is a non-destructive operation that compares your in-code legend against what is actually installed in the database:

- **Missing kinds** are created.
- **Missing attributes** are added.
- **Type mismatches** are logged as warnings.
- **Extra attributes or kinds** in the DB are logged as warnings.
- Nothing is removed or modified.

```clojure
;; programmatic
(migration/sync-schemas! config schemas)

;; or via no-arg form when app services are running
(migration/sync-schemas!)
```

Sync runs at most once every 3 minutes. Subsequent calls within that window are skipped.

**When to use sync vs. migrations:**

Use **sync** during development or as a safety net on startup — it catches drift between your schema definitions and the live database. Use **migrations** for any structural change that needs to be applied in a controlled, ordered, reversible way in production.

## How Migrations Are Tracked

Applied migrations are stored as `:migration` entities in your configured database. Each entity has:

```clojure
{:kind :migration
 :name "20230202-add-user-email"
 :at   #inst "2023-02-15T..."}
```

Two sentinel records also live in the `:migration` kind:

- `"LOCK"` — used for distributed locking (see below)
- `"SYNC_SCHEMAS"` — tracks when schema sync last ran

The migration schema is bootstrapped automatically on first run if it does not exist.

## Locking

The runner uses optimistic locking (`db/cas`) to prevent concurrent migrations. Before running, it acquires a lock by setting `:at` on the `LOCK` record. If another process holds the lock, the runner waits up to 10 seconds (checking every 250 ms) and then retries. The lock is always released in a `finally` block.

In preview mode, locking is skipped.

## Rollback

Rolling back to an earlier migration (by passing a target name to `migrate!`) calls each migration's `down` function in reverse order — from the most recently applied migration down to (but not including) the target.

Example: if migrations `20230101`, `20230202`, and `20230303` are applied and you run `(migrate! config "20230101")`, the runner calls `20230303/down` then `20230202/down`, leaving `20230101` as the latest applied.

Each `down` call is executed sequentially. If a `down` function throws, the runner halts immediately — later `down` functions are not called. The migration record for the failed migration is **not** deleted, so the database tracking reflects what was actually completed. There is no automatic partial-rollback recovery; you must fix the failure and retry manually.

## Backend Support

The migration runner works against any backend that implements the `c3kit.bucket.migrator/Migrator` protocol:

- In-memory (`:memory`) — supported; used in tests
- JDBC (`:jdbc`, H2, Postgres, SQLite, SQL Server) — supported
- Datomic (`:datomic-free`, `:datomic-client`, `:datomic-local`) — supported

All major backends ship with `Migrator` implementations. The `migrator/migration-schema` multimethod defaults to `default-migration-schema` for all backends, so no backend-specific migration config is needed unless you override it.
