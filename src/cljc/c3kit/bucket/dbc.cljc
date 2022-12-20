(ns c3kit.bucket.dbc)

(defn entity! [e id]
  (or e
      (throw (ex-info "Entity missing!" {:id id}))))

(defn entity-of-kind [e kind]
  (when (and e (= kind (:kind e)))
    e))

(defn soft-retract [id-or-entity]
  (-> (if (number? id-or-entity) {:id id-or-entity} id-or-entity)
      (assoc :kind :db/retract)))

(defn entity-of-kind! [e kind id]
  (or (entity-of-kind e kind)
      (throw (ex-info "Entity kind mismatch" {:expected kind :actual (:kind e) :id id}))))

(defn retract? [entity]
  (or (:retract (meta entity))
      (= :db/retract (:kind entity))))
