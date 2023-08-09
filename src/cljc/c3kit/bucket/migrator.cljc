(ns c3kit.bucket.migrator
  (:require [c3kit.apron.schema :as s]))

(def default-migration-schema {:kind (s/kind :migration)
                               :id   {:type :int}
                               :name {:type :string}
                               :at   {:type :timestamp}})

(defprotocol Migrator
  "API for migration operations"
  (installed-schema-legend [this legend] "Returns a map/legend of all schemas installed.  Uses existing legend keys for renamed tables/columns.")
  (install-schema! [this schema] "Install the given schema into the database schema.")
  (add-attribute! [this schema attr] [this kind attr spec] "Add an attribute to the database schema.")
  (remove-attribute! [this kind attr] "Remove an attribute from the database schema. All values will be removed.")
  (rename-attribute! [this kind attr new-kind new-attr] "Rename the attribute.  SQL implementations may not allow the kind to change.")
  )

(defmulti schema :impl)
(defmethod schema :default [_config] default-migration-schema)
