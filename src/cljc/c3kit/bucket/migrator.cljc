(ns c3kit.bucket.migrator
  (:require [c3kit.apron.schema :as s]
            [c3kit.bucket.api :as api]))

(def default-migration-schema {:kind (s/kind :migration)
                               :id   {:type :int}
                               :name {:type :string}
                               :at   {:type :timestamp}})

(defprotocol Migrator
  "API for migration operations"
  (-installed-schema-legend [this legend] "Returns a map/legend of all schemas installed.  Uses existing legend keys for renamed tables/columns.")
  (-install-schema! [this schema])
  (-add-attribute! [this schema attr] [this kind attr spec])
  (-remove-attribute! [this kind attr] "Remove an attribute from the database schema. All values will be removed.")
  (-rename-attribute! [this kind attr new-kind new-attr] "Rename the attribute.  SQL implementations may not allow the kind to change.")
  )

(defmulti migration-schema :impl)
(defmethod migration-schema :default [_config] default-migration-schema)

;; ----- API -----

(defn installed-schema-legend
  "Returns a map/legend of all schemas installed.  Uses existing legend keys for renamed tables/columns."
  [legend]
  (-installed-schema-legend @api/impl legend))

(defn installed-schema-legend-
  "installed-schema-legend with explicit db"
  [db legend]
  (-installed-schema-legend db legend))

(defn install-schema!
  "Install the given schema into the database schema."
  [schema]
  (-install-schema! @api/impl schema))

(defn install-schema!-
  "install-schema with explicit db"
  [db schema]
  (-install-schema! db schema))

(defn add-attribute!
  "Add an attribute to the database schema."
  [kind attr spec] (-add-attribute! @api/impl kind attr spec))

(defn add-attribute!-
  "add-attribute! with explicit db"
  [db kind attr spec]
  (-add-attribute! db kind attr spec))

(defn remove-attribute!
  "Add an attribute to the database schema."
  [kind attr] (-remove-attribute! @api/impl kind attr))

(defn remove-attribute!-
  "remove-attribute! with explicit db"
  [db kind attr]
  (-remove-attribute! db kind attr))

(defn rename-attribute!
  "Add an attribute to the database schema."
  [kind attr new-kind new-attr] (-rename-attribute! @api/impl kind attr new-kind new-attr))

(defn rename-attribute!-
  "rename-attribute! with explicit db"
  [db kind attr new-kind new-attr]
  [kind attr new-kind new-attr] (-rename-attribute! db kind attr new-kind new-attr))

;; ^^^^^ API ^^^^^
