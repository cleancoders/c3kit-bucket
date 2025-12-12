(ns c3kit.bucket.migration
  (:require [c3kit.apron.app :as app]
            [c3kit.apron.corec :as ccc]
            [c3kit.apron.legend :as legend]
            [c3kit.apron.log :as log]
            [c3kit.apron.time :as time]
            [c3kit.apron.util :as util]
            [c3kit.bucket.api :as db]
            [c3kit.bucket.migrator :as migrator]
            [clojure.set :as set]
            [clojure.string :as str]))

(def LOCK "LOCK")
(def SYNC_SCHEMAS "SYNC_SCHEMAS")
(def migration-name-pattern #"[0-9]{8}.*")
(defn migration-name? [arg] (boolean (when (string? arg) (re-matches migration-name-pattern arg))))

(defn -ensure-migration-schema! [{:keys [-db] :as config}]
  (let [schema (migrator/migration-schema config)]
    (swap! (.-legend -db) assoc (db/-schema-kind schema) schema)
    (when-not (migrator/-schema-exists? -db schema)
      (migrator/-install-schema! -db schema)
      (log/warn "Installed 'migration' schema because it was missing."))))

(defn -fetch-migration [{:keys [-db]} name]
  (db/ffind-by- -db :migration :name name))

(defn -create-migration! [{:keys [-db]} name]
  (db/tx- -db {:kind :migration :name name :at (time/now)}))

(defn -delete-migration! [{:keys [-db] :as config} name]
  (when-let [migration (-fetch-migration config name)]
    (db/delete- -db migration)))

(defn -applied-migrations [{:keys [-db]}]
  (db/find- -db :migration))

(defn -applied-migration-names [config]
  (->> (-applied-migrations config)
       (map :name)
       (filter migration-name?)
       sort))

;; ----- locking -----

(defn -fetch-lock [{:keys [-db] :as config}]
  (or (-fetch-migration config LOCK)
      (db/tx- -db {:kind :migration :name LOCK})))

(defn -attempt-lock! [{:keys [-db] :as config}]
  (let [lock (-fetch-lock config)]
    (try
      (db/tx- -db (db/cas {:at nil} (assoc lock :at (time/now))))
      (catch Exception _))))

(defn -release-lock! [{:keys [-db] :as config}]
  (let [lock (-fetch-lock config)]
    (when (:at lock)
      (db/tx- -db lock :at nil))))

(defn -locked? [config]
  (let [lock (-fetch-lock config)]
    (boolean (and lock (:at lock)))))

(def ^:private lock-wait-time (time/seconds 10))
(def ^:private lock-check-delay (time/milliseconds 250))

(defn -wait-for-unlock! [config]
  (when (-locked? config)
    (log/report "Waiting for lock to release...")
    (let [attempts (/ lock-wait-time lock-check-delay)]
      (loop [attempts attempts]
        (when-not (pos? attempts)
          (throw (Exception. "Migration failed to acquire lock on database!")))
        (when (-locked? config)
          (Thread/sleep ^Long lock-check-delay)
          (recur (dec attempts)))))))

(defn -attempt-with-lock [{:keys [-preview?] :as config} action-fn]
  (cond -preview? (do (action-fn) true)
        (-attempt-lock! config) (try
                                  (log/report "Lock acquired.")
                                  (action-fn)
                                  true
                                  (finally
                                    (-release-lock! config)
                                    (log/report "Lock released.")))
        :else false))

(defmacro attempt-with-lock!
  "Attempts to acquire lock and execute the body while locked, then release the lock.  Returns true if it succeeded.
  Returns false if it was unable to acquire the lock, meaning the body was never executed."
  [config & body]
  `(-attempt-with-lock ~config (fn [] ~@body)))

(defn- config-from-service []
  (let [config  (:bucket/config app/app)
        schemas (:bucket/schemas app/app)
        impl    (:bucket/impl app/app)]
    (when-not (some? config) (throw (ex-info "bucket service must be started in order to sync-schemas" {})))
    (assoc config :-db impl :-schemas schemas)))

;; ^^^^^ locking ^^^^^

(defn -available-migration-names [{:keys [migration-ns] :as config}]
  (when (or (nil? migration-ns) (str/blank? (name migration-ns)))
    (throw (ex-info ":migration-ns is missing from the database config." config)))
  (let [filenames (util/resources-in (name migration-ns))]
    (if (seq filenames)
      (->> filenames
           (filter #(re-matches #"[0-9]{8}.*\.clj" %))
           (map util/path->namespace)
           sort)
      (log/warn "No migrations found in :migration-ns" (select-keys config [:migration-ns])))))

(defn- str> [a b] (pos? (compare a b)))
(defn- str< [a b] (neg? (compare a b)))
(defn- str<= [a b] (not (str> a b)))

(defn- calculate-ups [available current target]
  (filter #(and (str> % current) (str<= % target)) available))

(defn- calculate-downs [available existing current target]
  (let [available-path (filter #(and (str> % target) (str<= % current)) available)
        existing-path  (filter #(str> % target) existing)]
    (when-not (= available-path existing-path)
      (throw (Exception. (str target " - can't find consistent path down. migration(s) were not recorded."))))
    (reverse existing-path)))

(defn -calculate-ups-and-downs [available existing target]
  (let [target  (if (nil? target) (last available) target)
        current (last existing)]
    (cond (nil? target) {:up [] :down []}
          (not (contains? (set available) target)) (throw (Exception. (str target "migration missing: " (pr-str target))))
          (= target current) {:up [] :down []}
          (str> target current) {:up (calculate-ups available current target) :down []}
          (str< target current) {:up [] :down (calculate-downs available existing current target)}
          :else (log/error "How did I get here?"))))

(defn- migrations-needed [config available target]
  (let [applied (-applied-migration-names config)
        {:keys [up down] :as result} (-calculate-ups-and-downs available applied target)]
    (when (or (seq up) (seq down))
      result)))

(defn -maybe-migrate-unlocked! [{:keys [migration-ns -preview?] :as config} available target]
  (if-let [{:keys [up down]} (migrations-needed config available target)]
    (do
      (when (seq up)
        (log/report "Running Database Migrations UP")
        (doseq [m up]
          (let [var-sym (symbol (str migration-ns "." m "/up"))
                m-fn    (util/resolve-var var-sym)]
            (log/info (str m " - applying migration UP"))
            (when-not -preview?
              (m-fn)
              (-create-migration! config m)))))
      (when (seq down)
        (log/report "Running Database Migrations DOWN")
        (doseq [m down]
          (let [var-sym (symbol (str migration-ns "." m "/down"))
                m-fn    (util/resolve-var var-sym)]
            (log/info (str m " - applying migration DOWN"))
            (when-not -preview?
              (m-fn)
              (-delete-migration! config m))))))
    (log/report "No migrations needed.")))

(defn- maybe-lock-and-migrate! [config available target]
  (-wait-for-unlock! config)
  (if (migrations-needed config available target)
    (when-not (attempt-with-lock! config (-maybe-migrate-unlocked! config available target))
      (recur config available target))
    (log/report "No migrations needed.")))

(defn migrate!
  "Migrate the database. The :migration-ns (in config) will be search for migration scripts that will be run either up
  or down to reach the target migration.  If no target is provided, it will migrate up to the latest."
  ([] (migrate! (config-from-service)))
  ([config] (migrate! config nil))
  ([{:keys [-preview?] :as config} target]
   (-ensure-migration-schema! config)
   (let [available (-available-migration-names config)]
     (if -preview?
       (-maybe-migrate-unlocked! config available target)
       (maybe-lock-and-migrate! config available target)))))

;; ^^^^^ migration ^^^^^

;; ----- service -----

(defn -start-service [app]
  (migrate!)
  app)

(def service (app/service 'c3kit.bucket.migration/-start-service nil))

;; ^^^^^ service ^^^^^

;; ----- synchronization -----

(defn- type-name [db-type]
  (-> db-type
      (cond-> (vector? db-type) first)
      name
      str/lower-case))

(defn- valid-type? [expected actual-type]
  (->> [expected actual-type]
       (map type-name)
       (apply =)))

(defn- spec-type [spec]
  (or (-> spec :db :type)
      (-> spec :type)))

(defn- sync-attribute [{:keys [-preview? -db]} schema attr installed-attrs]
  (let [spec (get schema attr)
        kind (db/-schema-kind schema)]
    (if-let [actual-type (spec-type (get installed-attrs attr))]
      (let [expected (spec-type spec)]
        (when-not (valid-type? expected actual-type)
          (log/warn (str kind "/" (name attr) " - type mismatch. expected: '" expected "' but was '" actual-type "'"))))
      (do (log/warn (str kind "/" (name attr) " - attribute missing. Creating."))
          (when-not -preview? (migrator/-add-attribute! -db schema attr))))))

(defn- schema-attrs [schema]
  (set (keys (dissoc schema :kind))))

(defn- log-extra-attributes [schema attributes]
  (let [expected (schema-attrs schema)
        actual   (schema-attrs attributes)
        extra    (set/difference actual expected)
        kind     (db/-schema-kind schema)]
    (doseq [attr extra]
      (log/warn (str kind "/" (name attr) " - extra attribute. Unused?")))))

(defn- sync-schema [config schema installed-attrs]
  (doseq [attr (sort (keys (dissoc schema :kind)))]
    (sync-attribute config schema attr installed-attrs)))

(defn- maybe-install-schema [{:keys [-preview? -db]} schema]
  (when-not -preview? (migrator/-install-schema! -db schema)))

(defn- install-schema [config schema]
  (let [kind (db/-schema-kind schema)]
    (log/warn (str kind " - kind missing. Creating."))
    (maybe-install-schema config schema)))

;; TODO - MDM: Handle name translation here.
(defn- sync-kind [{:keys [-installed-legend] :as config} schema]
  (let [kind            (db/-schema-kind schema)
        installed-attrs (get -installed-legend kind)]
    (cond
      (empty? installed-attrs) (install-schema config schema)
      (:kind schema) (sync-schema config schema installed-attrs)
      :else (maybe-install-schema config schema))
    (log-extra-attributes schema installed-attrs)))

(defn- fetch-sync [{:keys [-db] :as config}]
  (or (-fetch-migration config SYNC_SCHEMAS)
      (db/tx- -db {:kind :migration :name SYNC_SCHEMAS})))

(def ^:private sync-buffer-time (time/minutes 3))

(defn- sync-needed? [config]
  (let [last-sync      (fetch-sync config)
        last-synced-at (or (:at last-sync) time/epoch)]
    (time/before? last-synced-at (time/ago sync-buffer-time))))

(defn- perform-synchronization! [{:keys [-db] :as config} schemas]
  (log/report (str "Synchronizing " (count schemas) " schema(s) with the database"))
  (let [schemas          (map db/-normalize-schema schemas)
        legend           (legend/build schemas)
        installed-legend (migrator/-installed-schema-legend -db legend)
        config           (assoc config :-installed-legend installed-legend :_expected-legend legend)]
    (doseq [schema (sort-by db/-schema-kind schemas)]
      (sync-kind config schema))
    (let [expected (set (map db/-schema-kind schemas))
          actual   (set (keys installed-legend))
          extra    (disj (set/difference actual expected) :migration)]
      (doseq [kind (sort extra)]
        (log/warn (str kind " - extra kind. Unused?"))))))

(defn -maybe-sync-schemas-unlocked! [{:keys [-preview? -db] :as config} schemas]
  (if (sync-needed? config)
    (do (perform-synchronization! config schemas)
        (when-not -preview?
          (db/tx- -db (fetch-sync config) :at (time/now))))
    (log/info "Synchronization was performed recently. Skipping.")))

(defn sync-schemas!
  "Non-destructive synchronization of database schema from the supplied schemas.  That is, any new kinds or attributes
  will be added, but existing attributes will not be changed or removed.  Inconsistencies will be logged."
  ([]
   (let [config (config-from-service)]
     (sync-schemas! config (:-schemas config))))
  ([config schemas]
   (-ensure-migration-schema! config)
   (-wait-for-unlock! config)
   (if (sync-needed? config)
     (when-not (attempt-with-lock! config (-maybe-sync-schemas-unlocked! config schemas))
       (recur config schemas))
     (log/info "Synchronization was performed recently. Skipping."))))

;; ^^^^^ synchronization ^^^^^

;; ----- Main -----

(defn- list-migrations [config]
  (-ensure-migration-schema! config)
  (let [available (-available-migration-names config)
        applied   (reduce #(assoc %1 (:name %2) %2) {} (-applied-migrations config))]
    (println "\nmigration listing:\n")
    (println (format "%-50s %-20s" "Migration" "Applied At"))
    (println (apply str (repeat 75 "-")))
    (doseq [name available]
      (let [at (get-in applied [name :at])]
        (println (format "%-50s %s" name (if at (time/unparse :iso8601 at) "")))))))

(def ^:private usage
  (str "\n**** c3kit migration USAGE:\n"
       "\n"
       "clj -Mmigrate:test [command | migration-name] [preview]\n"
       "\n"
       " Commands:\n"
       "   help:  prints this message\n"
       "   sync:  synchronize the database schema with :full-schema-var\n"
       "   list:  show available and when they were applied\n"
       " migration-name - e.g. 20220806-first\n"
       "   migrations will be applied UP or DOWN to make the named migration the latest applied\n"
       " no arguments will migrate UP to latest migration\n"
       " preview: migrations/syncing will not update the database\n"))

(defn -main [& args]
  (app/start! [db/service])
  (let [impl     @db/impl
        config   (:bucket/config app/app)
        schemas  (:bucket/schemas app/app)
        preview? (contains? (set args) "preview")
        config   (assoc config :-db impl :-preview? preview?)
        args     (ccc/removev= args "preview")]
    (when preview? (println "Migration preview mode turned on"))
    (let [first-arg (first args)]
      (cond (= "help" first-arg) (println usage)
            (= "sync" first-arg) (sync-schemas! config schemas)
            (= "list" first-arg) (list-migrations config)
            (nil? first-arg) (migrate! config nil)
            (migration-name? first-arg) (migrate! config first-arg)
            :else (do (println "ERROR - unrecognized migration argument:" first-arg)
                      (println usage)
                      (System/exit -1)))))
  (System/exit 0))

;; ^^^^^ Main ^^^^^
