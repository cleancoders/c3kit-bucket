(ns c3kit.bucket.migration
  (:require [c3kit.apron.log :as log]
            [c3kit.apron.schema :as s]
            [c3kit.apron.time :as time]
            [c3kit.apron.util :as util]
            [c3kit.bucket.api :as db]
            [clojure.string :as str])
  (:import (java.net URI)
           (java.nio.file FileSystems Files Paths)))

(def default-migration-schema {:kind (assoc (s/kind :migration) :db {:table "migration"})
                               :id   {:type :int :db {:type "serial primary key"}}
                               :name {:type :string :db {:type "varchar(255) UNIQUE"}}
                               :at   {:type :timestamp}})

(defmulti migration-schema :impl)
(defmethod migration-schema :default [_config] default-migration-schema)

;(defmethod migration-schema :postgres [_config]
;  (merge-with merge default-migration-schema {:kind {:db {:table "migrations"}}
;                                              :id   {:db {:type "serial primary key"}}
;                                              :name {:db {:type "varchar(255) UNIQUE"}}}))

(def LOCK "LOCK")

(defn fetch-migration [{:keys [_db]} name]
  (db/ffind-by- @_db :migration :name name))

(defn create-migration! [{:keys [_db]} name]
  (db/tx- @_db {:kind :migration :name name :at (time/now)}))

(defn delete-migration! [{:keys [_db] :as config} name]
  (when-let [migration (fetch-migration config name)]
    (db/delete- @_db migration)))

(defn update-migration-time! [name]
  #_(let [sql "UPDATE migration SET at=?::timestamp WHERE name=?"]
      (next/execute-one! (connection) [sql (s/->timestamp (time/now)) name])
      (fetch-migration name)))

(defn applied-migrations [{:keys [_db]}]
  (db/find- @_db :migration))

(defn applied-migration-names [config]
  (->> (applied-migrations config)
       (map :name)
       (remove #{LOCK})
       sort))

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

;(defn attempt-lock! [config]
;  ;(ensure-migration-table!)
;  ;(if (fetch-lock)
;  ;  false
;  ;  (try
;  ;    (let [sql "INSERT INTO migration (name, at) VALUES (?, ?::timestamp) RETURNING id"]
;  ;      (next/execute-one! (connection) [sql LOCK (s/->timestamp (time/now))])
;  ;      (fetch-lock))
;  ;    (catch PSQLException _ false)))
;  )

(def lock-wait-time (time/seconds 10))
(def lock-check-delay (time/milliseconds 250))

(defn wait-for-unlock! [config]
  (let [attempts (/ lock-wait-time lock-check-delay)]
    (loop [attempts attempts]
      (when (not (pos? attempts))
        (throw (Exception. "Migration failed to acquire lock on database!")))
      (when (locked? config)
        (Thread/sleep lock-check-delay)
        (recur (dec attempts))))))

;(defn -with-lock [config action-fn]
;  (when-let [lock (wait-for-lock! config)]
;    (try
;      (action-fn)
;      (finally
;        (release-lock! config)))))

;(defmacro with-lock! [config & body] `(-with-lock ~config (fn [] ~@body)))

;(defn default-serial? [column]
;  (when-let [default (:columns/column_default column)]
;    (str/starts-with? default "nextval(")))
;
;(defn default-value [column]
;  (when-not (default-serial? column)
;    (if-let [default (:columns/column_default column)]
;      (first (str/split default #"\:\:"))
;      nil)))
;
;(defmulti -type-spec :columns/udt_name)
;(defmethod -type-spec :default [column] (:columns/udt_name column))
;(defmethod -type-spec "varchar" [column] (str "varchar(" (:columns/character_maximum_length column) ")"))
;(defmethod -type-spec "numeric" [column] (str "numeric(" (:columns/numeric_precision column) "," (:columns/numeric_scale column) ")"))
;(defmethod -type-spec "int4" [column] (if (default-serial? column) "serial" "int4"))
;
;(defn column-type-spec [constraints column]
;  ;(prn "column: " column)
;  (let [default (default-value column)
;        type    (-type-spec column)
;        name    (:columns/column_name column)]
;    (->> [type (get constraints name) (when default (str "DEFAULT " default))]
;         (remove nil?)
;         (str/join " "))))
;
;(defn- index-column [primary-key result column]
;  (assoc result (:columns/column_name column) (column-type-spec primary-key column)))
;
;(defn result-set->table-constraints [result-set]
;  (let [name-frequencies      (frequencies (map :table_constraints/constraint_name result-set))
;        multi-col-constraints (reduce (fn [r [c n]] (if (> n 1) (conj r c) r)) #{} name-frequencies)]
;    (->> result-set
;         (remove #(contains? multi-col-constraints (:table_constraints/constraint_name %)))
;         (remove #(= "FOREIGN KEY" (:table_constraints/constraint_type %)))
;         (reduce #(assoc %1 (:constraint_column_usage/column_name %2) (:table_constraints/constraint_type %2)) {}))))
;
;(defn table-constraints [table]
;  (let [sql    (str "SELECT * FROM information_schema.table_constraints tc "
;                    "inner join information_schema.constraint_column_usage cu "
;                    "on cu.constraint_name = tc.constraint_name "
;                    "WHERE tc.table_name=?")
;        result (next/execute! (connection) [sql table])]
;    (result-set->table-constraints result)))
;
;(defn table-columns [table]
;  (let [constraints (table-constraints table)]
;    (->> ["SELECT * FROM information_schema.columns WHERE table_name = ?" table]
;         (next/execute! (connection))
;         (reduce (partial index-column constraints) {}))))

#_(defn sync-kind [schema]
    (let [kind       (-> schema :kind :value)
          table-name (jdbc/table-name schema)
          columns    (table-columns table-name)]
      (if (seq columns)
        (doseq [attr (sort (keys (dissoc schema :kind)))]
          (sync-attr schema attr columns))
        (do (log/warn (str kind " - table missing. Creating."))
            (when-not preview? (jdbc/create-table-from-schema (connection) schema))))
      (log-extra-columns schema columns)))

#_(defn sync-schemas-unlocked! [schemas]
    (log/report (str "\nSynchronizing " (count schemas) " schema(s) with the database"))
    (doseq [schema (sort-by #(-> % :kind :value) schemas)]
      (sync-kind schema))
    (let [expected (set (map jdbc/table-name schemas))
          actual   (set (existing-tables))
          extra    (disj (set/difference actual expected) "migration")]
      (doseq [table (sort extra)]
        (log/warn (str table " - extra table. Unused?")))))

#_(defn sync-schemas! [schemas]
    (with-lock!
      (let [last-sync      (fetch-migration SYNC_SCHEMAS)
            last-synced-at (or (:at last-sync) time/epoch)]
        (if (time/before? last-synced-at (-> 1 time/minutes time/ago))
          (do (sync-schemas-unlocked! schemas)
              (when-not preview?
                (if last-sync
                  (update-migration-time! SYNC_SCHEMAS)
                  (create-migration! SYNC_SCHEMAS))))
          (log/info "Synchronization was performed within the last 5 min. Skipping.")))))

(defn namespace->path [namespace] (-> namespace (str/replace "-" "_") (str/replace "." "/")))
(defn namespace->package [namespace] (-> namespace (str/replace "-" "_")))
(defn path->namespace [path] (-> path (str/replace #".clj$" "") (str/replace "_" "-") (str/replace "/" ".")))

(defn- path-listing [path]
  (let [listing (Files/list path)]
    (mapv #(str (.getFileName %)) (-> listing .iterator iterator-seq))))

(defn package-filenames [package]
  (let [^String package-path      (namespace->path package)
        ^ClassLoader class-loader (.getContextClassLoader (Thread/currentThread))
        package-url               (.getResource class-loader package-path)
        ^URI package-uri          (when package-url (.toURI package-url))]
    (when package-url
      (if (= "jar" (.getScheme package-uri))
        (with-open [fs (FileSystems/newFileSystem package-uri {})]
          (path-listing (.getPath fs package-path (into-array String []))))
        (path-listing (Paths/get package-uri))))))

(defn available-migration-names [{:keys [migration-ns] :as config}]
  (when (or (nil? migration-ns) (str/blank? (name migration-ns)))
    (throw (ex-info ":migration-ns is missing from the database config." config)))
  (let [filenames (package-filenames (name migration-ns))]
    (if (seq filenames)
      (->> filenames
           (filter #(re-matches #"[0-9]{8}.*\.clj" %))
           (map path->namespace)
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
  (merge {:_db (delay (db/create-db config [(migration-schema config)]))} config))

(defn migrate-unlocked! [{:keys [migration-ns _preview?] :as config} target]
  (let [config    (init-config config)
        available (available-migration-names config)
        applied   (applied-migration-names config)
        {:keys [up down]} (calculate-ups-and-downs available applied target)]
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
            (delete-migration! config m)))))))

;; TODO - MDM:
;;  1) Wait for lock to release if exists
;;  2) Check if any change if needed
;;  3) If a change is needed:
;;    4) acquire lock
;;    5) recalculate changes needed
;;    6) make needed changes, if any
;;    7) finally, release lock

#_(defn migrate!
    ([] (migrate! nil))
    ([target] (with-lock! (migrate-unlocked! target))))
