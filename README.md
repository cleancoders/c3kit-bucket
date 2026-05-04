# Bucket

![Bucket](https://github.com/cleancoders/c3kit/blob/master/img/bucket_200.png?raw=true)

A library component of [c3kit - Clean Coders Clojure Kit](https://github.com/cleancoders/c3kit).

_"Most men give advice by the bucket, but take it by the grain."_ - William R. Alger

[![Bucket Build](https://github.com/cleancoders/c3kit-bucket/actions/workflows/test.yml/badge.svg)](https://github.com/cleancoders/c3kit-bucket/actions/workflows/test.yml)
[![Clojars Project](https://img.shields.io/clojars/v/com.cleancoders.c3kit/bucket.svg)](https://clojars.org/com.cleancoders.c3kit/bucket)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## What is bucket?

Bucket is a unified entity-storage API for Clojure and ClojureScript. The same domain code runs against any supported backend — Datomic, JDBC (Postgres, H2, MSSQL, SQLite), in-memory, and IndexedDB — because every backend implements the same `c3kit.bucket.api/DB` protocol. Define your schemas once with [c3kit-apron](https://github.com/cleancoders/c3kit-apron), then transact, query, and look up entities through `c3kit.bucket.api` regardless of where the data lives.

## Installation

**deps.edn**

```clojure
{:deps {com.cleancoders.c3kit/bucket {:mvn/version "2.13.1"}}}
```

**Leiningen**

```clojure
[com.cleancoders.c3kit/bucket "2.13.1"]
```

## Hello World

The following example uses the explicit-db API (`tx-`, `find-by-`, `entity-`), which works without any global state setup and runs in a plain `clj` REPL once the dep is on the classpath.

```clojure
(require '[c3kit.apron.schema :as s]
         '[c3kit.bucket.api :as db]
         '[c3kit.bucket.memory])  ; loads the :memory backend

;; Define a schema
(def widget
  {:kind  (s/kind :widget)
   :id    s/id
   :name  {:type :string}
   :color {:type :keyword}})

;; Create an in-memory database
(def my-db (db/create-db {:impl :memory} [widget]))

;; Save an entity (explicit-db variant)
(db/tx- my-db {:kind :widget :name "Sprocket" :color :red})
;; => {:kind :widget, :id 1001, :name "Sprocket", :color :red}

;; Find by attribute (returns a lazy seq)
(db/find-by- my-db :widget :name "Sprocket")
;; => ({:kind :widget, :id 1001, :name "Sprocket", :color :red})

;; Look up by id
(db/entity- my-db :widget 1001)
;; => {:kind :widget, :id 1001, :name "Sprocket", :color :red}
```

The id value (`1001` above) is auto-generated and will vary. The same code shape works against any supported backend; only the `{:impl ...}` config map passed to `create-db` changes.

## Supported backends

| Impl key | Platform | Notes | Guide |
|---|---|---|---|
| `:memory` | CLJ / CLJS | In-process, ephemeral | — |
| `:re-memory` | CLJS | Reagent-aware in-memory | — |
| `:jdbc` | CLJ | Postgres, H2, MSSQL, SQLite (+ pgvector, sqlite-vec) | — |
| `:datomic` | CLJ | Datomic on-prem (peer) | [docs/datomic-guide.md](docs/datomic-guide.md) |
| `:datomic-cloud` | CLJ | Datomic Cloud (client) | [docs/datomic-guide.md](docs/datomic-guide.md) |
| `:indexeddb` | CLJS | Browser persistent storage | [docs/indexeddb-guide.md](docs/indexeddb-guide.md) |
| `:re-indexeddb` | CLJS | Reagent-aware IndexedDB | [docs/indexeddb-guide.md](docs/indexeddb-guide.md) |

## Core concepts

### Schemas

Schemas are plain Clojure maps validated by [c3kit-apron](https://github.com/cleancoders/c3kit-apron). Every entity schema has a `:kind` declaration (via `s/kind`), an `:id` field, and attribute definitions that describe the type of each field.

```clojure
(def order
  {:kind     (s/kind :order)
   :id       s/id
   :status   {:type :keyword}
   :subtotal {:type :bigdec}
   :placed-at {:type :instant}})
```

Attribute types include `:string`, `:keyword`, `:long`, `:int`, `:boolean`, `:instant`, `:bigdec`, `:ref`, and collection types like `[:string]` (a sequence of strings). See the apron docs for the full type reference.

### The DB protocol

Every backend implements `c3kit.bucket.api/DB`. Consumers typically call the wrapper functions in `c3kit.bucket.api` (`tx`, `find`, `find-by`, `ffind`, `ffind-by`, `entity`, `entity!`, `count`, `delete`, `reload`, etc.).

Each wrapper has an **explicit-db variant** suffixed with `-` (e.g. `tx-`, `find-by-`, `entity-`) that accepts a DB instance as its first argument. Use these variants when working with multiple databases in the same process, or to avoid touching the global `impl` atom.

## Migrations

Bucket includes a migration system for evolving your schema over time. Migrations are Clojure namespaces with numbered naming conventions; the migrator tracks which have run and executes new ones in order. See [docs/migrations-guide.md](docs/migrations-guide.md) for setup and usage.

## Background tasks

`c3kit.bucket.bg` provides a lightweight scheduled-task manager backed by a `ScheduledThreadPoolExecutor`. Tasks are registered by key and store a `:last-ran-at` timestamp in the database, allowing persistent scheduling across restarts. Refer to the docstrings in `src/clj/c3kit/bucket/bg.clj` for the full public API.

## Development

### Running tests

```bash
# JVM tests
clj -M:test:spec
clj -M:test:spec -a        # auto runner

# ClojureScript tests
clj -M:test:cljs once
clj -M:test:cljs           # auto runner
```

The full SQL suite requires Postgres, MSSQL, SQLite, and sqlite-vec to be available locally. See [CONTRIBUTING.md](CONTRIBUTING.md) for environment setup details.

### PostgreSQL local setup

```
$ sudo -u postgres createuser $(whoami)
$ sudo -u postgres createdb test
$ sudo -u postgres psql -d test -c "GRANT ALL ON SCHEMA public TO PUBLIC;"
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for development workflow, environment
setup, and the release process. This project follows the
[Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md). Security issues
should be reported privately — see [SECURITY.md](SECURITY.md).

## License

[MIT](LICENSE) © Clean Coders.
