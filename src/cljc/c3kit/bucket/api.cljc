(ns c3kit.bucket.api
  (:refer-clojure :rename {find    core-find
                           count   core-count
                           reduce  core-reduce
                           -find   core--find
                           -count  core--count
                           -reduce core--reduce})
  (:require #?(:clj [c3kit.apron.app :as app])
            [c3kit.apron.log :as log]
            [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.apron.schema :as schema]
            #?(:clj [c3kit.apron.util :as util])))

(defprotocol DB
  "API for database operations"
  (-clear [this])
  (-count [this kind options])
  (-delete-all [this kind])
  (-entity [this kind id])
  (-find [this kind options])
  (-reduce [this kind f init options])
  (-tx [this entity])
  (-tx* [this entities])
  )

#?(:clj  (defonce impl (app/resolution! :bucket/impl))
   :cljs (def impl (atom nil)))
#?(:cljs (def set-impl! (partial reset! impl)))

(defn -id-type [legend kind] (get-in (legend/for-kind legend kind) [:id :type]))
(defn -coerced-id
  ([legend kind id] (-coerced-id (-id-type legend kind) id))
  ([type id]
   (let [coerce (schema/type-coercer! type)]
     (try (coerce id)
          (catch #?(:clj Exception :cljs :default) e
            (log/warn (str "failed to coerce id of type " type " - " (ex-message e)))
            id)))))

(defn -kvs->kv-pairs [kvs]
  (assert (even? (core-count kvs)) "filter params must come in pairs")
  (let [kv-pairs (partition 2 kvs)]
    (assert (every? #(keyword? %) (map first kv-pairs)) "filter attributes must be keywords")
    kv-pairs))

(defn -apply-take [options entities]
  (if-let [n (:take options)]
    (take n entities)
    entities))

(defn -apply-drop [options entities]
  (if-let [n (:drop options)]
    (drop n entities)
    entities))

(defn -apply-drop-take [options entities] (-apply-take options (-apply-drop options entities)))

;; ---- API -----

(def ^:dynamic *safety* true)
(defn -assert-safety-off! [action] (assert (not *safety*) (str "Safety if on! Refusing to " action)))
(defn set-safety! [on?] #?(:clj (alter-var-root #'*safety* (fn [_] on?)) :cljs (set! *safety* on?)))
#?(:clj (defmacro with-safety-off [& body] `(with-redefs [*safety* false] ~@body)))

(defn legend
  "Returns the legend (map of :kind -> schema) of the database implementation."
  [db] (deref (.-legend db)))

(defn entity
  "Retrieve an entity by its id.
  kind is required by some implementations, otherwise nil is returned when the kind does not match."
  ([id] (when id (-entity @impl nil id)))
  ([kind id] (when id (-entity @impl kind id))))

(defn entity-
  "entity with explicit db"
  ([db id] (when id (-entity db nil id)))
  ([db kind id] (when id (-entity db kind id))))

(defn entity!
  "Like entity but throws an Exception when the entity doesn't exist."
  ([id] (entity! nil id))
  ([kind id] (or (entity kind id) (throw (ex-info "Entity missing!" {:kind kind :id id})))))

(defn entity!-
  "entity! with explicit db"
  ([db id] (entity!- db nil id))
  ([db kind id] (or (entity- db kind id) (throw (ex-info "Entity missing!" {:kind kind :id id})))))

(defn find
  "Searches for entities.
Options:
  :where   - key-value pairs to filter matching entities, without which *ALL* entities of kind will be returned.
    value         - (= % value)
    nil           - (nil? %)
    [values]      - (some #(= % value) values)
    ['not value]  - (not (= % value))
    ['> value]    - (> % value)
    ['< value]    - (< % value)
  :take   - int - returns only this many entities"
  ([kind & opt-args] (-find @impl kind (ccc/->options opt-args))))

(defn find-
  "find with explicit db"
  ([db kind & opt-args] (-find db kind (ccc/->options opt-args))))

(defn ffind
  "Shorthand for (ffind kind :where {k1 v1 ...})"
  [kind & opt-args] (first (-find @impl kind (assoc (ccc/->options opt-args) :take 1))))

(defn ffind-
  "ffind with explicit db"
  [db kind & opt-args] (first (-find db kind (assoc (ccc/->options opt-args) :take 1))))

(defn find-by
  "Shorthand for (ffind kind :where {k1 v1 ...})"
  [kind & kvs] (-find @impl kind {:where (-kvs->kv-pairs kvs)}))

(defn find-by-
  "find-by with explicit db"
  [db kind & kvs] (-find db kind {:where (-kvs->kv-pairs kvs)}))

(defn ffind-by
  "Shorthand for (ffind kind :where {k1 v1 ...})"
  [kind & kvs] (first (-find @impl kind {:where (-kvs->kv-pairs kvs) :take 1})))

(defn ffind-by-
  "ffind-by with explicit db"
  [db kind & kvs] (first (-find db kind {:where (-kvs->kv-pairs kvs) :take 1})))

(defn reduce-
  "reduce with explicit db"
  [db kind f init & opt-args] (-reduce db kind f init (ccc/->options opt-args)))

(defn reduce
  "Reduces all matching entities over f.
Useful when processing many entities without loading them all at the same time.
    f       - (fn [result entity] ...)
    init    - initial value for reduction
    options - same as for find"
  [kind f init & opt-args] (-reduce @impl kind f init (ccc/->options opt-args)))

(defn count
  "Count the number of entities that match the provided filters."
  [kind & opt-args]
  (-count @impl kind (ccc/->options opt-args)))

(defn count-
  "count with explicit db"
  [db kind & opt-args] (-count db kind (ccc/->options opt-args)))

(defn count-by
  "Shorthand for (count kind :where {k1 v1 ...})"
  [kind & kvs] (-count @impl kind {:where (-kvs->kv-pairs kvs)}))

(defn count-by-
  "count-by with explicit db"
  [db kind & kvs] (-count db kind {:where (-kvs->kv-pairs kvs)}))

(defn tx-
  "tx with explicit db"
  [db & args]
  (let [e (ccc/->options args)]
    (when (seq e)
      (-tx db e))))

(defn tx
  "Transacts (save, update, or delete) the entity.
  Arguments, assumed to be in key-value pairs, will be merged into the entity prior to saving.
  Retracts entity when it's metadata has :retract."
  [& args] (apply tx- @impl args))

(defn tx*
  "Transact multiple entities, synchronously in a transaction.  If any error occurs, none of the changes persist."
  ([entities] (tx* @impl entities))
  ([db entities] (->> entities
                      (remove nil?)
                      (-tx* db))))

(defn delete-
  "delete with explicit db"
  ([db entity] (tx- db (assoc entity :db/delete? true)))
  ([db kind id] (tx- db {:kind kind :id id :db/delete? true})))

(defn delete
  "Delete an entity"
  ([entity] (delete- @impl entity))
  ([kind id] (delete- @impl kind id)))

(defn soft-delete
  "Returns the entity marked for deletion for tx."
  ([entity] {:kind (:kind entity) :id (:id entity) :db/delete? true})
  ([kind id] {:kind kind :id id :db/delete? true}))

(defn delete?
  "true when passed an entity marked for deletion or the result of deleted entity."
  [entity]
  (or (:db/delete? (meta entity))
      (:db/delete? entity)))

(defn reload-
  "reload with explicit db"
  [db e] (when-let [id (:id e)] (entity- db (:kind e) id)))

(defn reload
  "Returns the entity freshly loaded from the database."
  [e] (reload- @impl e))

(defn delete-all-
  "delete-all with explicit db"
  [db kind]
  (-assert-safety-off! "delete-all")
  (-delete-all db kind))

(defn delete-all
  "Delete all entities"
  [kind] (delete-all- @impl kind))

(defn clear-
  "clear with explicit db"
  [db] (-clear db))

(defn clear
  "Clear the database.  Removes all entities stored.
Presumably only for tests or in-memory implementation, but it will work on any implementation.
Requires the *safety* be turned off."
  [] (-clear @impl))

(defmulti -create-impl (fn [config _schema] (:impl config)))
(defn create-db
  "Create an instance of DB based off the configuration.
  config - a map that may contain the following keys
    :impl - :memory | :datomic | :jdbc
    :on-save - a fn called on each entity before it is saved in the database (fn [entity] ...)
    :on-load - a fn called on each entity after it is loaded from the database (fn [entity] ...)
    DATOMIC
    :uri - datomic connection uri
    JDBC (included keys for jdbc/next library https://github.com/seancorfield/next-jdbc/blob/develop/src/next/jdbc.clj#L76)
    :dialect :h2 | :postgres | :mssql
    "
  [config schemas]
  (-create-impl config schemas))

(defn cas
  "Compare And Swap.  Marks the entity for compare and swap of old-attrs.  If the entity values do not match
  those in old-attrs, the transaction will fail and an exception will be thrown."
  [old-attrs entity]
  (when-not (:id entity)
    (throw (ex-info "cas may not be applied to new entities." {:old-attrs old-attrs :entity entity})))
  (when old-attrs
    (assert (map? old-attrs))
    (with-meta entity {:cas old-attrs})))

(defn -get-cas [entity] (:cas (meta entity)))

(defn -check-cas! [cas entity original]
  (doseq [[k v] cas]
    (let [actual (get original k)]
      (when-not (= v actual)
        (throw (ex-info (str "cas failure: " (pr-str v) " " (pr-str actual)) {:cas cas :entity entity :original original}))))))

#?(:clj
   (defn load-config
     "Read config/bucket.edn from the classpath.  The config may should contain:
       :impl         - and other keys required by create-db
       :full-schema  - symbol of qualified var that holds a seq of all db schemas (for migration)
       :migration-ns - symbol of namespace where all migration scripts are located
       :config-var   - symbol of qualified var that holds the config map which will be merged in"
     []
     (let [config (util/read-edn-resource "config/bucket.edn")]
       (if-let [config-var (:config-var config)]
         (let [dynamic-config (util/var-value config-var)]
           (-> (merge config dynamic-config)
               (dissoc :config-var)))
         config))))

#?(:clj
   (defn -start-service [app]
     (let [config      (load-config)
           _           (log/info "Starting bucket service: " (:impl config))
           schemas-var (:full-schema config)
           _           (when-not schemas-var (throw (ex-info ":full-schema missing from bucket config" config)))
           schemas     (util/var-value schemas-var)
           impl        (create-db config schemas)]
       (assoc app :bucket/impl impl
                  :bucket/config config
                  :bucket/schemas schemas))))

#?(:clj
   (defn -stop-service [app]
     (log/info "Stopping bucket service")
     (dissoc app :bucket/impl :bucket/config :bucket/schemas)))

#?(:clj (def service (app/service 'c3kit.bucket.api/-start-service 'c3kit.bucket.api/-stop-service)))

;; TODO - MDM:
;;  2) middleware for saving and loading. timestamps is a saving middleware
;;  3) apply to test data.  Bring in entity and for-kind features
;;  4) seeding entity
;;  7) datomic specific features
