# Consolidate IndexedDB / ReIndexedDB into a Single Deftype

**Date:** 2026-03-27

## Problem

`indexeddb.cljs` and `re_indexeddb.cljs` contain nearly identical deftypes (`IndexedDB` and `ReIndexedDB`). The only difference is two lines: `-entity` and `-find` delegate to `memory` vs `re-memory` respectively. This duplication means every change to IndexedDB behavior must be made in two places.

## Design

### Single Deftype with Injected Functions

Replace both deftypes with a single `IndexedDB` deftype that accepts `entity-fn` and `find-fn` as constructor fields. The branching between `memory` and `re-memory` happens once at construction time in the factory function, not at every read.

### Deftype (in `indexeddb.cljs`)

```clojure
(deftype IndexedDB [legend store idb-atom db-name online-fn entity-fn find-fn]
  api/DB
  (-clear [this]
    (memory/clear this)
    (when @idb-atom (idb/clear-all @idb-atom)))
  (close [_this] (io/close @idb-atom))
  (-count [this kind options] (core-count (api/-find this kind options)))
  (-delete-all [this kind]
    (memory/delete-all this kind)
    (when @idb-atom (idb/clear-store @idb-atom kind)))
  (-entity [this kind id] (entity-fn this kind id))
  (-find [this kind options] (find-fn this kind options))
  (-reduce [this kind f init options] (core-reduce f init (api/-find this kind options)))
  (-tx [this entity] (idb/idb-tx this entity))
  (-tx* [this entities] (idb/idb-tx* this entities))
  migrator/Migrator
  (-schema-exists? [_this schema] (contains? @legend (api/-schema-kind schema)))
  (-installed-schema-legend [_this _expected-legend] @legend)
  (-install-schema! [this schema] (memory/do-install-schema! this schema))
  (-add-attribute! [this schema attr] (migrator/-add-attribute! this (-> schema :kind :value) attr (get schema attr)))
  (-add-attribute! [_this kind attr spec] (swap! legend assoc-in [kind attr] spec))
  (-remove-attribute! [this kind attr] (memory/do-remove-attribute! this kind attr))
  (-rename-attribute! [this kind attr new-kind new-attr] (memory/do-rename-attribute! this kind attr new-kind new-attr)))
```

### Factory: `:indexeddb` (in `indexeddb.cljs`)

```clojure
(defmethod api/-create-impl :indexeddb [config schemas]
  (let [legend    (atom (legend/build schemas))
        store     (or (:store config) (atom {}))
        idb-atom  (atom nil)
        db-name   (or (:db-name config) "c3kit-bucket")
        online-fn (or (:online? config) (constantly true))]
    (IndexedDB. legend store idb-atom db-name online-fn memory/entity memory/do-find)))
```

### Factory: `:re-indexeddb` (in `re_indexeddb.cljs`)

`re_indexeddb.cljs` becomes a thin factory file (~10 lines). It requires `indexeddb` for the deftype and `re-memory` for the reactive functions:

```clojure
(ns c3kit.bucket.re-indexeddb
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.indexeddb :refer [IndexedDB]]
            [c3kit.bucket.re-memory :as re-memory]
            [reagent.core :as r]))

(defmethod api/-create-impl :re-indexeddb [config schemas]
  (let [legend    (atom (legend/build schemas))
        store     (or (:store config) (r/atom {}))
        idb-atom  (atom nil)
        db-name   (or (:db-name config) "c3kit-bucket")
        online-fn (or (:online? config) (constantly true))]
    (IndexedDB. legend store idb-atom db-name online-fn re-memory/entity re-memory/do-find)))
```

### Why Two Files / Two `:impl` Keywords

A single `:indexeddb` impl with a `:store-type` config key was considered. However, ClojureScript does not support `requiring-resolve` or dynamic requires. A single file would need to `(:require ... [c3kit.bucket.re-memory ...] [reagent.core ...])` at the top level, forcing all consumers to pull in reagent even if they only want non-reactive IndexedDB (e.g., a vanilla ClojureScript app without React). Keeping two factory files avoids this unwanted transitive dependency.

### What Gets Deleted

- The `ReIndexedDB` deftype (the entire deftype block in `re_indexeddb.cljs`)
- All requires in `re_indexeddb.cljs` that are only needed for the deftype body (`idb-common`, `idb-io`, `memory`, `migrator`)

### What Stays the Same

- Consumer API: `:impl :indexeddb` and `:impl :re-indexeddb` config keys unchanged
- All other config keys (`:store`, `:db-name`, `:online?`) unchanged
- All protocol method behavior unchanged

## Testing

- Existing `indexeddb_spec` tests should pass with no changes (the `:indexeddb` factory still produces the same behavior)
- Existing `re_indexeddb_spec` tests should pass with no changes (the `:re-indexeddb` factory still produces the same behavior, just via the shared deftype)
- Verify that `re_indexeddb.cljs` no longer defines its own deftype
