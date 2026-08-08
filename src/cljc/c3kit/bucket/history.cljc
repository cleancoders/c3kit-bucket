(ns c3kit.bucket.history
  "Impl-agnostic API for entity history: timelines, point-in-time reads, timestamps, excision.
  Follows c3kit.bucket.api conventions: each operation has a default-db version (history, as-of, ...)
  and an explicit-db version suffixed with a dash (history-, as-of-, ...)."
  (:require [c3kit.bucket.api :as api]))

(defprotocol HistoryDB
  "API for entity history operations"
  (-history [db entity])
  (-as-of [db t])
  (-created-at [db id-or-entity])
  (-updated-at [db id-or-entity])
  (-excise! [db id-or-entity]))

(defn ->id
  "Returns the entity id from an id or entity map."
  [id-or-entity]
  (if (map? id-or-entity) (:id id-or-entity) id-or-entity))

(deftype ReadOnlyDB [db]
  api/DB
  (close [_] nil)
  (-legend [_] (api/-legend db))
  (-entity [_ kind id] (api/-entity db kind id))
  (-find [_ kind options] (api/-find db kind options))
  (-count [_ kind options] (api/-count db kind options))
  (-reduce [_ kind f init options] (api/-reduce db kind f init options))
  (-tx [_ _] (throw (ex-info "as-of view is read-only" {})))
  (-tx* [_ _] (throw (ex-info "as-of view is read-only" {})))
  (-clear [_] (throw (ex-info "as-of view is read-only" {})))
  (-delete-all [_ _] (throw (ex-info "as-of view is read-only" {}))))

(defn supported?
  "true when the current (or given) db implements HistoryDB."
  ([] (supported? @api/impl))
  ([db] (satisfies? HistoryDB db)))

(defn history-
  "history with explicit db.
  Returns a vector of every version of the entity, oldest → newest.
  Each version is the full entity stamped with :db/tx and :db/instant.
  A deletion appears as {:db/tx <id> :db/instant <inst> :db/deleted? true}.
  Unknown entity (or no recorded history) => [].
  Requires (:id entity); asserts otherwise."
  [db entity]
  (-history db entity))

(defn history
  "Returns a vector of every version of the entity, oldest → newest.
  Each version is the full entity stamped with :db/tx and :db/instant.
  A deletion appears as {:db/tx <id> :db/instant <inst> :db/deleted? true}.
  Unknown entity (or no recorded history) => [].
  Requires (:id entity); asserts otherwise."
  [entity]
  (history- @api/impl entity))

(defn created-at-
  "created-at with explicit db. Instant of the first tx, or nil."
  [db id-or-entity]
  (-created-at db id-or-entity))

(defn created-at
  "Instant (java.util.Date / js/Date) of the first tx, or nil."
  [id-or-entity]
  (created-at- @api/impl id-or-entity))

(defn updated-at-
  "updated-at with explicit db. Instant of the most recent tx (deletion counts), or nil."
  [db id-or-entity]
  (-updated-at db id-or-entity))

(defn updated-at
  "Instant of the most recent tx (deletion counts), or nil."
  [id-or-entity]
  (updated-at- @api/impl id-or-entity))

(defn with-timestamps-
  "with-timestamps with explicit db. Adds :db/created-at and :db/updated-at."
  [db entity]
  (assoc entity
    :db/created-at (created-at- db entity)
    :db/updated-at (updated-at- db entity)))

(defn with-timestamps
  "Adds :db/created-at and :db/updated-at timestamps to the entity."
  [entity]
  (with-timestamps- @api/impl entity))

(defn as-of-
  "as-of with explicit db. Returns a READ-ONLY api/DB view as of time t.
  t is an instant OR a tx id, inclusive.
  Write operations (-tx, -tx*, -clear, -delete-all) throw."
  [db t]
  (-as-of db t))

(defn as-of
  "Returns a READ-ONLY api/DB view of the database as it was at time t.
  Composes with all bucket.api read fns:
    (api/find- (as-of t) :bibelot :where {:color \"blue\"})
    (api/entity- (as-of t) :bibelot id)
  Write operations throw. t is an instant OR a tx id, inclusive."
  [t]
  (as-of- @api/impl t))

(defn entity-as-of-
  "entity-as-of with explicit db. Sugar for (api/entity- (as-of- db t) kind id)."
  [db t kind id]
  (api/entity- (as-of- db t) kind id))

(defn entity-as-of
  "Sugar: (api/entity- (as-of t) kind id)"
  [t kind id]
  (entity-as-of- @api/impl t kind id))

(defn find-as-of-
  "find-as-of with explicit db. Sugar for (apply api/find- (as-of- db t) kind opts)."
  [db t kind & opts]
  (apply api/find- (as-of- db t) kind opts))

(defn find-as-of
  "Sugar: (apply api/find- (as-of t) kind opts)"
  [t kind & opts]
  (apply find-as-of- @api/impl t kind opts))

(defn ffind-as-of-
  "ffind-as-of with explicit db. First match of find-as-of-."
  [db t kind & opts]
  (first (apply find-as-of- db t kind opts)))

(defn ffind-as-of
  "Sugar: first match of find-as-of."
  [t kind & opts]
  (apply ffind-as-of- @api/impl t kind opts))

(defn excise!-
  "excise! with explicit db. Erase the entity AND all trace of it from history."
  [db id-or-entity]
  (-excise! db id-or-entity))

(defn excise!
  "Erase the entity AND all trace of it from history."
  [id-or-entity]
  (excise!- @api/impl id-or-entity))
