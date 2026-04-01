### 2.12.1
* Fixed IDB keyPath migration: stores created by old bucket versions with `keyPath: "idxId"` are now detected and recreated with `keyPath: "id"` during `onupgradeneeded`
* Added `store-format-version` to `schema-hash` to force version bumps when store structure changes
* Fixed `sync-complete!` silently deleting dirty entities when server returns empty response (e.g., duplicate sync-id rejection); now no-ops to preserve dirty state for retry

### 2.12.0
* Fixed `:cache` strategy `init!` clearing dirty (unsynced) entities from IDB; dirty entities are now preserved across the clear
* Simplified `idb-common` public API: `init!`, `rehydrate!`, `refresh!`, `sync!`, and `sync-complete!` no longer require a `db` argument (they deref `api/impl` internally)
* Added optional dedup keys to `sync-tx` and `sync-tx*` for crash-recovery idempotency; prevents duplicate entity creation when offline syncs are retried after partial server failures

### 2.11.0
* Added IndexedDB implementations
  * :indexeddb for IndexedDB plus :memory
  * :re-indexeddb for IndexedDB plus :re-memory
* Includes `idbc.cljc` namespace for server-side helper functions to make sync endpoints idempotent
* Includes lightweight `idb-reader.cljs` for service workers
* Developer guide to using Bucket IndexedDB at docs/indexeddb-guide.md

### 2.10.0
* Updated ReMemory for compatibility with React 18 / Reagent 2 (backwards compatible with React 17 / Reagent 1.x)
    * Upgraded test dependencies to reagent 2.0.1, cljsjs/react 18.3.1-1, and cljsjs/react-dom 18.3.1-1
    * `ensure-full-entity-and-meta` now reads directly from the store instead of calling `entity`
    * Added `clear-slice-db-cache!` to clear the Reagent reaction cache when creating a new ReMemory impl

### 2.9.0
* Adds support for vector searching (embeddings) for postgres and memory implementations
* Formalizes :order-by option for find-by
* Formalizes json support for postgres and h2 

### 2.8.0
 * Adds Migration as a service

### 2.7.1
 * Normalizes input schemas when performing a migrate sync

### 2.7.0
 * Allow nested collections of schemas in `:full-schema`
 * Fix issue where conflicting types would throw during migrations
 * Suppress migration warnings with missing `:kind` attributes
 * `-installed-schema-legend` returns normalized schemas
   * Particularly for schemas of type `:seq`

### 2.6.0
 * ReMemory `db/tx` changed to use implicit nils 
   * `(db/tx (dissoc my-entity :some-attr))`
 * Added `rem/select-tx` and `rem/select-tx*` to ReMemory to use explicit nils
   * `(db/select-tx my-entity :some-attr nil)`

### 2.5.1
 * Reverts hashids dependency to the previously used version

### 2.5.0
 * Added ReMemory, a cljs reagent-focused bucket implementation
   * Optimizes frontend performance with state management
   * `find` will only deref parts of the database based on the query instead of being a deref for the whole database
   * `entity` will only deref the specific entity instead of being a deref for the whole database
   * `select-find` added to allow more precise control over what is part of a deref

### 2.4.0
 * Add support for enum types for Datomic in `c3kit.bucket.migration`
 * Upgrade dependencies
 * BREAKING: Remove deprecated namespaces

### 2.3.2
 * FIX: `nil` values in seqs passed in to Datomic's `find-by :id` now works

### 2.3.1
 * FIX: `nil` values passed in to Datomic's `find-by :id` now works

### 2.3.0
 * Datomic implementations may now search by the `:id` attribute
   * `(find-by :thing :id 123)`
   * `(find-by :thing :id [123 456])`
   * `(find-by :thing :id ['not= 123 456])`
 * `tx*` is now a no-op when provided with an empty seq
 * FIX: Updates `not` to `not=` in `c3kit.bucket.api/find` docstring
 * FIX: Providing an empty seq to "in" queries now yields no results
   * `(find-by :thing :name [])`
   * `(find-by :thing :name ['=])`
   * `(find-by :thing :size 2 :name [])`
   * `(find-by :thing :size [] :name [])`
* FIX: Providing an empty seq to "not in" queries now ignores those fields
    * `(find-by :thing :name ['not=])` - returns all results
    * `(find-by :thing :size ['not=] :name ['not=])` - returns all results
    * `(find-by :thing :size 2 :name ['not=])` - returns all results with `:size 2`

### 2.2.2
 * Fixes warning in datomic introduced in 2.2.1 when using "like" queries

### 2.2.1
 * :uuid types in JDBC can take strings
 * Implement case-insensitive "like" query for datomic and SQLite3

### 2.2.0
 * Apron and Scaffold 2.2.0

### 2.1.18
 * Adds `:cast` attribute to jdbc to override the db cast type when building queries

### 2.1.17
* Refactors `do-find-` `q->entities` in Datomic Cloud to not make separate pull request for each entity id it finds
* Refactors shared code between Datomic and Datomic Cloud into Datomic Common

### 2.1.15
 * Adds `reduce-sql-` to jdbc for added control over query reductions

### 2.1.7
 * Remove `log/warn!` side effect from cljs spec-helper

### 2.1.6
 * Bumps Apron

### 2.1.4
 * Upgrades dependencies
 * Supports empty "in" clauses for JDBC

### 2.1.3 
 * Adds LIKE queries to datomic implementation

### 2.1.2
 * uses apron 2.1.3, datomic uses new :seq spec format for cardinality
 * adds datomic q, find-datalog, squuid

### 2.?.?
 * fixes typo in migration usage 
 * Adds bucket.seed namespace for seeding databases with data
