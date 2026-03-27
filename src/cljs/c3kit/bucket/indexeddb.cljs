(ns c3kit.bucket.indexeddb
  (:refer-clojure :rename {find core-find count core-count reduce core-reduce})
  (:require [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as idb]
            [c3kit.bucket.idb-io :as io]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as migrator]))

;region IndexedDB deftype

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

;endregion

;region Registration

(defmethod api/-create-impl :indexeddb [config schemas]
  (let [legend    (atom (legend/build schemas))
        store     (or (:store config) (atom {}))
        idb-atom  (atom nil)
        db-name   (or (:db-name config) "c3kit-bucket")
        online-fn (or (:online? config) (constantly true))]
    (IndexedDB. legend store idb-atom db-name online-fn memory/entity memory/do-find)))

;endregion
