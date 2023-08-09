(ns c3kit.bucket.migration
  (:require [c3kit.apron.legend :as legend]
            [c3kit.apron.log :as log]
            [c3kit.apron.time :as time]
            [c3kit.apron.util :as util]
            [c3kit.bucket.api :as db]
            [c3kit.bucket.migrator :as migrator]
            [clojure.set :as set]
            [clojure.string :as str]))

;(defmethod migration-schema :postgres [_config]
;  (merge-with merge default-migration-schema {:kind {:db {:table "migrations"}}
;                                              :id   {:db {:type "serial primary key"}}
;                                              :name {:db {:type "varchar(255) UNIQUE"}}}))

(def LOCK "LOCK")
(def SYNC_SCHEMAS "SYNC_SCHEMAS")
(def migration-name-pattern #"[0-9]{8}.*")
(defn migration-name? [arg] (boolean (when (string? arg) (re-matches migration-name-pattern arg))))

(defn ensure-migration-schema! [{:keys [_db _preview?] :as config}]
  (when-not _preview?
    (when-not (migrator/installed-schema-legend @_db "migration")
      (migrator/install-schema! @_db (migrator/schema config))
      (log/warn "Installed 'migration' schema because it was missing."))))

(defn fetch-migration [{:keys [_db]} name]
  (db/ffind-by- @_db :migration :name name))

(defn create-migration! [{:keys [_db]} name]
  (db/tx- @_db {:kind :migration :name name :at (time/now)}))

(defn delete-migration! [{:keys [_db] :as config} name]
  (when-let [migration (fetch-migration config name)]
    (db/delete- @_db migration)))

(defn applied-migrations [{:keys [_db]}]
  (db/find- @_db :migration))

(defn applied-migration-names [config]
  (->> (applied-migrations config)
       (map :name)
       (filter migration-name?)
       sort))

;; ----- locking -----

(defn fetch-lock [{:keys [_db] :as config}]
  (or (fetch-migration config LOCK) (db/tx- @_db {:kind :migration :name LOCK})))

(defn attempt-lock! [{:keys [_db] :as config}]
  (let [lock (fetch-lock config)]
    (try
      (db/tx- @_db (db/cas {:at nil} (assoc lock :at (time/now))))
      (catch Exception _ nil))))

(defn release-lock! [{:keys [_db] :as config}]
  (let [lock (fetch-lock config)]
    (when (:at lock)
      (db/tx- @_db lock :at nil))))

(defn locked? [config]
  (let [lock (fetch-lock config)]
    (boolean (and lock (:at lock)))))

(def lock-wait-time (time/seconds 10))
(def lock-check-delay (time/milliseconds 250))

(defn wait-for-unlock! [config]
  (when (locked? config)
    (log/report "Waiting for lock to release...")
    (let [attempts (/ lock-wait-time lock-check-delay)]
      (loop [attempts attempts]
        (when (not (pos? attempts))
          (throw (Exception. "Migration failed to acquire lock on database!")))
        (when (locked? config)
          (Thread/sleep lock-check-delay)
          (recur (dec attempts)))))))

(defn -attempt-with-lock [{:keys [_preview?] :as config} action-fn]
  (cond _preview? (do (action-fn) true)
        (attempt-lock! config) (try
                                 (log/report "Lock acquired.")
                                 (action-fn)
                                 true
                                 (finally
                                   (release-lock! config)
                                   (log/report "Lock released.")))
        :else false))

(defmacro attempt-with-lock!
  "Attempts to acquire lock and execute the body while locked, then release the lock.  Returns true if it succeeded.
  Returns false if it was unable to acquire the lock, meaning the body was never executed."
  [config & body]
  `(-attempt-with-lock ~config (fn [] ~@body)))

;; ^^^^^ locking ^^^^^

(defn available-migration-names [{:keys [migration-ns] :as config}]
  (when (or (nil? migration-ns) (str/blank? (name migration-ns)))
    (throw (ex-info ":migration-ns is missing from the database config." config)))
  (let [filenames (util/resources-in (name migration-ns))]
    (if (seq filenames)
      (->> filenames
           (filter #(re-matches #"[0-9]{8}.*\.clj" %))
           (map util/path->namespace)
           sort)
      (log/warn "No migrations found in :migration-ns" config))))

(defn- str> [a b] (pos? (compare a b)))
(defn- str< [a b] (neg? (compare a b)))
(defn- str<= [a b] (not (str> a b)))
(defn- str>= [a b] (not (str< a b)))

(defn calculate-ups [available current target]
  (filter #(and (str> % current) (str<= % target)) available))
(defn calculate-downs [available existing current target]
  (let [available-path (filter #(and (str> % target) (str<= % current)) available)
        existing-path  (filter #(and (str> % target)) existing)]
    (when-not (= available-path existing-path)
      (throw (Exception. (str target " - can't find consistent path down. migration(s) were not recorded."))))
    (reverse existing-path)))

(defn calculate-ups-and-downs [available existing target]
  (let [target  (if (nil? target) (last available) target)
        current (last existing)]
    (cond (nil? target) {:up [] :down []}
          (not (contains? (set available) target)) (throw (Exception. (str target "migration missing: " (pr-str target))))
          (= target current) {:up [] :down []}
          (str> target current) {:up (calculate-ups available current target) :down []}
          (str< target current) {:up [] :down (calculate-downs available existing current target)}
          :else (log/error "How did I get here?"))))

(defn init-config [config]
  (let [db (delay (db/create-db config [(migrator/schema config)]))]
    (merge
      {:_db db}
      config)))

(defn migrations-needed [config available target]
  (let [applied (applied-migration-names config)
        {:keys [up down] :as result} (calculate-ups-and-downs available applied target)]
    (when (or (seq up) (seq down))
      result)))

(defn -maybe-migrate-unlocked! [{:keys [migration-ns _preview?] :as config} available target]
  (if-let [{:keys [up down]} (migrations-needed config available target)]
    (do
      (when (seq up)
        (log/report "Running Database Migrations UP")
        (doseq [m up]
          (let [var-sym (symbol (str migration-ns "." m "/up"))
                m-fn    (util/resolve-var var-sym)]
            (log/info (str m " - applying migration UP"))
            (when-not _preview?
              (m-fn)
              (create-migration! config m)))))
      (when (seq down)
        (log/report "Running Database Migrations DOWN")
        (doseq [m down]
          (let [var-sym (symbol (str migration-ns "." m "/down"))
                m-fn    (util/resolve-var var-sym)]
            (log/info (str m " - applying migration DOWN"))
            (when-not _preview?
              (m-fn)
              (delete-migration! config m))))))
    (log/report "No migrations needed.")))

(defn- maybe-lock-and-migrate! [config available target]
  (wait-for-unlock! config)
  (if (migrations-needed config available target)
    (when-not (attempt-with-lock! config (-maybe-migrate-unlocked! config available target))
      (recur config available target))
    (log/report "No migrations needed.")))

(defn migrate!
  ([config] (migrate! config nil))
  ([{:keys [_preview?] :as config} target]
   (let [config    (init-config config)
         available (available-migration-names config)]
     (if _preview?
       (-maybe-migrate-unlocked! config available target)
       (maybe-lock-and-migrate! config available target)))))

;; ^^^^^ migration ^^^^^

;; ----- synchronization -----

(defn sync-attribute [{:keys [_preview? _db] :as config} schema attr installed-attrs]
  (let [spec (get schema attr)
        kind (-> schema :kind :value)]
    (if-let [actual-type (get installed-attrs attr)]
      (let [expected (or (-> spec :db :type) (:type spec))]
        (when-not (= (str/lower-case (name expected)) (str/lower-case (name actual-type)))
          (log/warn (str kind "/" (name attr) " - type mismatch. expected: '" expected "' but was '" actual-type "'"))))
      (do (log/warn (str kind "/" (name attr) " - attribute missing. Creating."))
          (when-not _preview? (migrator/add-attribute! @_db schema attr))))))

(defn log-extra-attributes [schema attributes]
  (let [expected (set (keys (dissoc schema :kind)))
        actual   (set (keys attributes))
        extra    (set/difference actual expected)
        kind     (-> schema :kind :value)]
    (doseq [attr extra]
      (log/warn (str kind "/" attr " - extra attribute. Unused?")))))

(defn sync-kind [{:keys [_preview? _db _installed-legend] :as config} schema]
  (let [kind            (-> schema :kind :value)
        installed-attrs (get _installed-legend kind)]
    (if (seq installed-attrs)
      (doseq [attr (sort (keys (dissoc schema :kind)))]
        (sync-attribute config schema attr installed-attrs))
      (do (log/warn (str kind " - kind missing. Creating."))
          (when-not _preview? (migrator/install-schema! @_db schema))))
    (log-extra-attributes schema installed-attrs)))

(defn fetch-sync [{:keys [_db] :as config}]
  (or (fetch-migration config SYNC_SCHEMAS) (db/tx- @_db {:kind :migration :name SYNC_SCHEMAS})))

(def sync-buffer-time (time/minutes 3))

(defn sync-needed? [{:keys [_preview? _db] :as config}]
  (let [last-sync      (fetch-sync config)
        last-synced-at (or (:at last-sync) time/epoch)]
    (time/before? last-synced-at (time/ago sync-buffer-time))))

(defn perform-synchronization! [{:keys [_db] :as config} schemas]
  (log/report (str "Synchronizing " (count schemas) " schema(s) with the database"))
  (let [legend           (legend/build schemas)
        installed-legend (migrator/installed-schema-legend @_db legend)
        config           (assoc config :_installed-legend installed-legend :_expected-legend legend)]
    (doseq [schema (sort-by #(-> % :kind :value) schemas)]
      (sync-kind config schema))
    (let [expected (set (map #(-> % :kind :value) schemas))
          actual   (set (keys installed-legend))
          extra    (disj (set/difference actual expected) "migration")]
      (doseq [kind (sort extra)]
        (log/warn (str kind " - extra kind. Unused?"))))))

(defn maybe-sync-schemas-unlocked! [{:keys [_preview? _db] :as config} schemas]
  (if (sync-needed? config)
    (do (perform-synchronization! config schemas)
        (when-not _preview?
          (db/tx- @_db (fetch-sync config) :at (time/now))))
    (log/info "Synchronization was performed recently. Skipping.")))

(defn- maybe-lock-and-sync! [config schemas]
  (wait-for-unlock! config)
  (if (sync-needed? config)
    (when-not (attempt-with-lock! config (maybe-sync-schemas-unlocked! config schemas))
      (recur config schemas))
    (log/info "Synchronization was performed recently. Skipping.")))

;; ^^^^^ synchronization ^^^^^

(def usage (str "\n**** c3kit migration USAGE:\n"
                "\n"
                "clj -Mmigrate:test [command | migration-name] [preview]\n"
                "\n"
                " Commands:\n"
                "   help:  prints this message\n"
                "   init:  synchronize the database schema with :full-schema-var\n"
                "   list:  show available and when they were applied\n"
                " migration-name - e.g. 20220806-first\n"
                "   migrations will be applied UP or DOWN to make the named migration the latest applied\n"
                " no command will migrate UP to latest migration\n"
                " preview: migrations/syncing will not update the database\n"))


(defn list-migrations [config]
  (let [available (available-migration-names config)
        applied   (reduce #(assoc %1 (:name %2) %2) {} (applied-migrations config))]
    (println "\nmigration listing:\n")
    (println (format "%-50s %-20s" "Migration" "Applied At"))
    (println (apply str (take 75 (repeat "-"))))
    (doseq [name available]
      (let [at (get-in applied [name :at])]
        (println (format "%-50s %s" name (if at (time/unparse :iso8601 at) "")))))))

;(defn run [args]
;  (let [first-arg (first args)]
;    (cond (= "help" first-arg) (println usage)
;          ;(= "init" first-arg) (sync-schemas! config (vals all/schemas))
;          (= "list" first-arg) (list-migrations config)
;          (nil? first-arg) (migrate! config nil)
;          (migration-name? first-arg) (migrate! config first-arg)
;          :else (do (println "ERROR - unrecognized migration argument:" first-arg)
;                    (println usage)))))

;(defn -main [& args]
;  (app/start! [main/legend main/db])
;  (if (contains? (set args) "preview")
;    (binding [migration/preview? true]
;      (println "Migration preview mode turned on")
;      (run (ccc/removev= args "preview")))
;    (run args)))
