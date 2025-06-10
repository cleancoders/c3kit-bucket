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
