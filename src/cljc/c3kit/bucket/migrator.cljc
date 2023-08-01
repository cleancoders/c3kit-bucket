(ns c3kit.bucket.migrator
  (:require [c3kit.apron.schema :as s]))

(def default-migration-schema {:kind (s/kind :migration)
                               :id   {:type :int}
                               :name {:type :string}
                               :at   {:type :timestamp}})

(defprotocol Migrator
  "API for migration operations"
  (installed-schema-legend [this legend])
  (install-schema! [this schema])
  (install-attribute! [this schema attr])
  )

(defmulti schema :impl)
(defmethod schema :default [_config] default-migration-schema)
