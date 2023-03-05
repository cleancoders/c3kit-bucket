(ns c3kit.bucket.api
  (:require #?(:clj [c3kit.apron.app :as app])
            [c3kit.apron.log :as log]
            [c3kit.apron.corec :as ccc]
            [c3kit.apron.time :as time]
            [c3kit.apron.legend :as legend]
            [c3kit.apron.schema :as schema]))

(def ^:dynamic *safety* true)
(defn -assert-safety-off! [action] (assert (not *safety*) (str "Safety if on! Refusing to " action)))
(defn set-safety! [on?] #?(:clj (alter-var-root #'*safety* (fn [_] on?)) :cljs (set! *safety* on?)))
#?(:clj (defmacro with-safety-off [& body] `(with-redefs [*safety* false] ~@body)))

(defn delete? [entity]
  (or (:db/delete? (meta entity))
      (:db/delete? entity)))

(defprotocol DB
  "API for database operations"
  (-install-schema [this schemas])
  (-clear [this])
  (-count [this kind options])
  (-delete-all [this kind])
  (-entity [this kind id])
  (-find [this kind options])
  (-reduce [this kind f init options])
  (-tx [this entity])
  (-tx* [this entity])
  )

#?(:clj  (defonce impl (app/resolution! :db))
   :cljs (def impl (atom nil)))
#?(:cljs (def set-impl! (partial reset! impl)))

(defn -id-type [legend kind] (get-in (legend/for-kind @legend kind) [:id :type]))
(defn -coerced-id
  ([legend kind id] (-coerced-id (-id-type legend kind) id))
  ([type id]
   (let [coerce (schema/type-coercer! type)]
     (try (coerce id)
          (catch #?(:clj Exception :cljs :default) e
            (log/warn (str "failed to coerce id of type " type " - " (ex-message e)))
            id)))))

(defn -kvs->kv-pairs [kvs]
  (assert (even? (count kvs)) "filter params must come in pairs")
  (let [kv-pairs (partition 2 kvs)]
    (assert (every? #(keyword? %) (map first kv-pairs)) "filter attributes must be keywords")
    kv-pairs))

(defn -apply-take [options entities]
  (if-let [n (:take options)]
    (take n entities)
    entities))

(defn legend
  "Returns the legend (map of :kind -> schema) of the database implementation."
  [db] (deref (.-legend db)))

(defn install-schema
  ([schemas] (install-schema @impl schemas))
  ([db schemas] (-install-schema db schemas)))

(defn entity
  "Retrieve an entity by its id.
  kind is required by some implementations, otherwise nil is returned when the kind does not match."
  ([id] (when id (-entity @impl nil id)))
  ([kind id] (when id (-entity @impl kind id))))

(defn entity!
  "Like entity but throws an Exception when the entity doesn't exist."
  ([id] (entity! nil id))
  ([kind id] (or (entity kind id) (throw (ex-info "Entity missing!" {:kind kind :id id})))))

(defn find
  "Searches for entities.
Options:
  :where  - key-value pairs to filter matching entities, without which *ALL* entities of kind will be returned.
    value         - (= % value)
    nil           - (nil? %)
    [values]      - (some #(= % value) values)
    ['not value]  - (not (= % value))
    ['> value]    - (> % value)
    ['< value]    - (< % value)"
  [kind & opt-args]
  (-find @impl kind (ccc/->options opt-args)))

(defn ffind
  "Shorthand for (ffind kind :where {k1 v1 ...})"
  [kind & opt-args]
  (first (-find @impl kind (assoc (ccc/->options opt-args) :take 1))))

(defn find-by
  "Shorthand for (ffind kind :where {k1 v1 ...})"
  [kind & kvs]
  (-find @impl kind {:where (-kvs->kv-pairs kvs)}))

(defn ffind-by
  "Shorthand for (ffind kind :where {k1 v1 ...})"
  [kind & kvs]
  (first (-find @impl kind {:where (-kvs->kv-pairs kvs) :take 1})))

(defn reduce
  [kind f init & opt-args]
  (-reduce @impl kind f init (ccc/->options opt-args)))

(defn count-by [kind & kvs]
  (-count @impl kind {:where (-kvs->kv-pairs kvs)}))

(defn count [kind & opt-args]
  (-count @impl kind (ccc/->options opt-args)))

(defn -apply-created-at [schema e]
  (if (and (:created-at schema) (nil? (:created-at e)))
    (assoc e :created-at (time/now))
    e))

(defn -apply-update-at [schema e]
  (if (:updated-at schema)
    (assoc e :updated-at (time/now))
    e))

(defn -update-timestamps [schema e]
  (->> (-apply-created-at schema e)
       (-apply-update-at schema)))

(defn tx
  "Transacts (save, update, or retract) the entity.
  Arguments, assumed to be in key-value pairs, will be merged into the entity prior to saving.
  Retracts entity when it's metadata has :retract."
  [& args]
  (let [e (ccc/->options args)]
    (when (seq e)
      ;(-tx @impl (-maybe-add-id e))
      (-tx @impl e)
      )))

(defn tx*
  "Transact multiple entities, synchronously"
  [entities]
  (->> entities
       (remove nil?)
       ;(map -maybe-add-id)
       (-tx* @impl)))

(defn delete
  "Delete an entity"
  ([entity] (tx (assoc entity :db/delete? true)))
  ([kind id] (tx {:kind kind :id id :db/delete? true})))

(defn soft-delete
  ([entity] {:kind (:kind entity) :id (:id entity) :db/delete? true})
  ([kind id] {:kind kind :id id :db/delete? true}))

(defn reload [e] (when-let [id (:id e)] (entity (:kind e) id)))

(defn delete-all
  "Delete all entities"
  [kind]
  (-delete-all @impl kind))

(defn clear
  "Clear the database.  Removes all entities stored"
  []
  (-clear @impl))


;; TODO - MDM:
;;  1) clean up api (general find that takes a options :where :limit :count :sort ...)
;;  2) middleware for saving and loading. timestamps is a saving middleware
;;  3) apply to test data.  Bring in entity and for-kind features
;;  4) seeding entity
;;  5) document
;;  6) Ability to use multiple dbs at the same time through api
