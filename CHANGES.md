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
