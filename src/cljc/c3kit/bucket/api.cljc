(ns c3kit.bucket.api
  (:require #?(:clj [c3kit.apron.app :as app])
            [c3kit.apron.log :as log]
            [c3kit.apron.corec :as ccc]
            [c3kit.apron.time :as time]
            [c3kit.apron.legend :as legend]
            [c3kit.apron.schema :as schema]))

(defn delete? [entity]
  (or (:db/delete? (meta entity))
      (:db/delete? entity)))

(defprotocol DB
  "API for database operations"
  (-clear [this])
  (-count-all [this kind])
  (-count-by [this kind key-vals])
  (-delete-all [this kind])
  (-entity [this kind id])
  (-find-all [this kind])
  (-find-by [this kind key-vals])
  (-ffind-by [this kind key-vals])
  ;(-find-page-by [this kind params key-vals])
  (-maybe-add-int-id [this e])
  (-reduce-by [this kind f val key-vals])
  (-tx [this entity])
  (-tx* [this entity])
  )

#?(:clj  (defonce impl (app/resolution! :db))
   :cljs (def impl (atom nil)))
#?(:cljs (def set-impl! (partial reset! impl)))

(defn id-type [kind] (get-in (legend/for-kind kind) [:id :type]))
(defn coerced-id [kind id]
  (let [type   (id-type kind)
        coerce (schema/type-coercer! type)]
    (try (coerce id)
         (catch #?(:clj Exception :cljs :default) e
           (log/warn (str "failed to coerce id of kind " kind " - " (ex-message e)))
           id))))

(defn- -maybe-generate-id-helper [e]
  (case (id-type (:kind e))
    :uuid (assoc e :id (ccc/new-uuid))
    :int (-maybe-add-int-id @impl e)
    (throw (ex-info (str "don't know how to generate id of type" id-type) {}))))

(defn -maybe-add-id [e]
  (cond (delete? e) e
        (:id e) e
        :else (-maybe-generate-id-helper e)))

(defn entity [kind id]
  (when id
    (-entity @impl kind id)))

(defn entity! [kind id]
  (or (entity kind id) (throw (ex-info "Entity missing!" {:kind kind :id id}))))

(defn find-by [kind & kvs]
  (-find-by @impl kind kvs))

(defn ffind-by [kind & kvs]
  (-ffind-by @impl kind kvs))

(defn find-all [kind]
  (when kind (-find-all @impl kind)))

;(defn find-page-by [kind params & kvs]
;  (-find-page-by @impl kind params kvs))

(defn reduce-by [kind f val & kvs]
  (-reduce-by @impl kind f val kvs))

(defn count-all [kind]
  (-count-all @impl kind))

(defn count-by [kind & kvs]
  (-count-by @impl kind kvs))

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

(defn reload [e] (when-let [id (:id e)] (entity (:kind e) id)))

(defn delete-all
  "Delete all entities"
  [kind]
  (-delete-all @impl kind))

(defn clear
  "Clear the database.  Removes all entities stored"
  []
  (-clear @impl))

