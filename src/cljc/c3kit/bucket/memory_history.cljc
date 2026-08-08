(ns c3kit.bucket.memory-history
  "In-memory history decorator for any api/DB implementation.
  History begins at wrap time. The version log lives in process memory."
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.time :as time]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.history :as history]
            [c3kit.bucket.memory :as memory]
            [c3kit.bucket.migrator :as migrator]))

(defn- instant-millis [t]
  #?(:clj  (.getTime ^java.util.Date t)
     :cljs (.getTime t)))

(defn- version-key [t]
  (if (number? t) :db/tx :db/instant))

(defn- version-qualifies? [version t]
  (let [k (version-key t)]
    (if (= :db/tx k)
      (<= (get version k) t)
      (<= (instant-millis (get version k)) (instant-millis t)))))

(defn- strip-meta [e]
  (dissoc e :db/tx :db/instant :db/deleted?))

(defn- last-live-as-of [versions t]
  (when-let [qualifying (seq (filter #(version-qualifies? % t) versions))]
    (let [last-v (last qualifying)]
      (when-not (:db/deleted? last-v)
        (strip-meta last-v)))))

(defn- snapshot-store [versions t]
  (reduce-kv
    (fn [store id vs]
      (if-let [e (last-live-as-of vs t)]
        (let [kind (:kind e)]
          (-> store
              (assoc-in [:all id] e)
              (assoc-in [kind id] e)))
        store))
    {}
    versions))

(defn- previous-content [versions id]
  (when-let [vs (get versions id)]
    (when-let [prev (last vs)]
      (when-not (:db/deleted? prev)
        (strip-meta prev)))))

(defn- ->date [millis]
  #?(:clj  (java.util.Date. (long millis))
     :cljs (js/Date. millis)))

(defn- next-instant
  "Wall-clock now, but strictly after any previously recorded instant so as-of
  by Date remains useful when multiple txs land in the same millisecond."
  [versions]
  (let [now-ms  (instant-millis (time/now))
        prev-ms (->> (vals versions)
                     (mapcat identity)
                     (keep :db/instant)
                     (map instant-millis)
                     (reduce max 0))]
    (->date (if (<= now-ms prev-ms) (inc prev-ms) now-ms))))

(defn- record!
  "Record versions for a batch of tx results. One tx id and instant shared across the batch."
  [versions tx-counter results]
  (let [txid (swap! tx-counter inc)]
    (swap! versions
           (fn [vs]
             (let [instant (next-instant vs)]
               (reduce
                 (fn [vs result]
                   (let [id (:id result)]
                     (cond
                       (nil? id)
                       vs

                       (api/delete? result)
                       (if (previous-content vs id)
                         (update vs id ccc/conjv {:db/tx txid :db/instant instant :db/deleted? true})
                         vs)

                       :else
                       (let [prev (previous-content vs id)]
                         (if (= prev result)
                           vs
                           (update vs id ccc/conjv (assoc result :db/tx txid :db/instant instant)))))))
                 vs
                 results))))))

(defn- record-deletions! [versions tx-counter doomed]
  (record! versions tx-counter (map api/soft-delete doomed)))

(deftype MemoryHistoryDecorator [db versions tx-counter]
  api/DB
  (close [_] (api/close db))
  (-legend [_] (api/-legend db))
  (-entity [_ kind id] (api/-entity db kind id))
  (-find [_ kind options] (api/-find db kind options))
  (-count [_ kind options] (api/-count db kind options))
  (-reduce [_ kind f init options] (api/-reduce db kind f init options))
  (-tx [_ e]
    (let [result (api/-tx db e)]
      (when result (record! versions tx-counter [result]))
      result))
  (-tx* [_ es]
    (let [results (api/-tx* db es)]
      (when (seq results) (record! versions tx-counter results))
      results))
  (-clear [_]
    (api/-clear db)
    (reset! versions {}))
  (-delete-all [_ kind]
    (let [doomed (api/-find db kind {})]
      (api/-delete-all db kind)
      (record-deletions! versions tx-counter doomed)))

  history/HistoryDB
  (-history [_ entity]
    (assert (:id entity) "history requires an entity with :id")
    (vec (get @versions (:id entity) [])))
  (-as-of [this t]
    (let [snapshot (snapshot-store @versions t)
          mem-db   (memory/->MemoryDB (api/-legend this) (atom snapshot))]
      (history/->ReadOnlyDB mem-db)))
  (-created-at [_ id-or-entity]
    (when-let [vs (seq (get @versions (history/->id id-or-entity)))]
      (:db/instant (first vs))))
  (-updated-at [_ id-or-entity]
    (when-let [vs (seq (get @versions (history/->id id-or-entity)))]
      (:db/instant (last vs))))
  (-excise! [_ id-or-entity]
    (let [id (history/->id id-or-entity)]
      (when-let [e (api/entity- db id)]
        (api/delete- db e))
      (swap! versions dissoc id)))

  migrator/Migrator
  (-schema-exists? [_ schema] (migrator/-schema-exists? db schema))
  (-installed-schema-legend [_ expected] (migrator/-installed-schema-legend db expected))
  (-install-schema! [_ schema] (migrator/-install-schema! db schema))
  (-add-attribute! [_ schema attr] (migrator/-add-attribute! db schema attr))
  (-add-attribute! [_ kind attr spec] (migrator/-add-attribute! db kind attr spec))
  (-remove-attribute! [_ kind attr] (migrator/-remove-attribute! db kind attr))
  (-rename-attribute! [_ kind attr new-kind new-attr] (migrator/-rename-attribute! db kind attr new-kind new-attr)))

(defn decorate
  "Wrap any api/DB with in-memory history recording. History begins at wrap time."
  [db]
  (MemoryHistoryDecorator. db (atom {}) (atom 1000)))

(defmethod api/-create-impl :memory-history [config schemas]
  (decorate (api/create-db (or (:storage config) {:impl :memory}) schemas)))
