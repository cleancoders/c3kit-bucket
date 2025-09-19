(ns c3kit.bucket.migration-spec
  (:require [c3kit.apron.log :as log]
            [c3kit.apron.time :as time]
            [c3kit.bucket.api :as db]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.migration :as sut]
            [c3kit.bucket.migrator :as migrator]
            [c3kit.bucket.spec-helperc :as helperc]
            [speclj.core :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(def db-config {:impl         :memory
                :migration-ns "c3kit.bucket.migration_samples"})

(declare config now)

(describe "Migration"

  (with now (time/now))
  (redefs-around [time/now (constantly @now)])

  (context "loading migration namespaces"

    (it "find available migrations - missing :migration-ns"
      (should-throw ExceptionInfo ":migration-ns is missing from the database config."
        (sut/-available-migration-names {})))

    (it "find available migrations - package doesn't exist"
      (log/capture-logs
        (should= nil (sut/-available-migration-names {:migration-ns "some.missing.ns"})))
      (should= :warn (:level (first (log/parse-captured-logs)))))

    (it "find available migrations"
      (let [filenames (sut/-available-migration-names db-config)]
        (should= 3 (count filenames))
        (should= ["20230101" "20230202" "20230303"] filenames)))

    )

  (context "utilities"

    (it "migration-name?"
      (should= false (sut/migration-name? nil))
      (should= false (sut/migration-name? ""))
      (should= false (sut/migration-name? "blah"))
      (should= false (sut/migration-name? "eighteen"))
      (should= false (sut/migration-name? "nope12345678"))
      (should= false (sut/migration-name? "nope-12345678"))
      (should= false (sut/migration-name? "-12345678"))
      (should= true (sut/migration-name? "12345678"))
      (should= true (sut/migration-name? "12345678fodder"))
      (should= true (sut/migration-name? "12345678-fodder")))

    (it "ups and downs"
      (should= {:up [] :down []} (sut/-calculate-ups-and-downs [] [] nil))
      (should= {:up ["1" "2" "3"] :down []} (sut/-calculate-ups-and-downs ["1" "2" "3"] [] nil))
      (should= {:up ["1"] :down []} (sut/-calculate-ups-and-downs ["1" "2" "3"] [] "1"))
      (should= {:up ["1" "2"] :down []} (sut/-calculate-ups-and-downs ["1" "2" "3"] [] "2"))
      (should= {:up ["1" "2" "3"] :down []} (sut/-calculate-ups-and-downs ["1" "2" "3"] [] "3"))
      (should= {:up [] :down []} (sut/-calculate-ups-and-downs ["1" "2" "3"] ["1" "2" "3"] nil))
      (should= {:up ["3"] :down []} (sut/-calculate-ups-and-downs ["1" "2" "3"] ["1" "2"] nil))
      (should= {:up ["2" "3"] :down []} (sut/-calculate-ups-and-downs ["1" "2" "3"] ["1"] nil))
      (should= {:up [] :down []} (sut/-calculate-ups-and-downs ["1" "2" "3"] ["1"] "1"))
      (should= {:up ["2"] :down []} (sut/-calculate-ups-and-downs ["1" "2" "3"] ["1"] "2"))
      (should= {:up ["2" "3"] :down []} (sut/-calculate-ups-and-downs ["1" "2" "3"] ["1"] "3"))
      (should= {:up [] :down ["3" "2"]} (sut/-calculate-ups-and-downs ["1" "2" "3"] ["1" "2" "3"] "1"))
      (should= {:up [] :down ["3"]} (sut/-calculate-ups-and-downs ["1" "2" "3"] ["1" "2" "3"] "2"))
      (should= {:up [] :down []} (sut/-calculate-ups-and-downs ["1" "2" "3"] ["1" "2" "3"] "3"))
      (should-throw (sut/-calculate-ups-and-downs ["1" "2" "3"] [] "BLAH"))
      (should-throw (sut/-calculate-ups-and-downs ["1" "2" "3"] ["1" "3"] "1")))

    )

  (context "with mem db"

    (helperc/with-schemas [migrator/default-migration-schema])
    (with config (assoc db-config :-db @db/impl))

    (it "applied-migrations"
      (should= [] (sut/-applied-migrations @config))
      (let [migration (db/tx {:kind :migration :name "joe" :at (time/now)})]
        (should= [migration] (sut/-applied-migrations @config))))

    (it "applied-migrations-names"
      (should= [] (sut/-applied-migration-names @config))
      (sut/-create-migration! @config "00000001-joe")
      (sut/-create-migration! @config "00000002-ann")
      (sut/-create-migration! @config sut/LOCK)
      (sut/-create-migration! @config sut/SYNC_SCHEMAS)
      (should= ["00000001-joe" "00000002-ann"] (sut/-applied-migration-names @config)))

    (it "create/fetch/delete-migration!"
      (let [migration (sut/-create-migration! @config "joe")]
        (should= migration (sut/-fetch-migration @config "joe"))
        (sut/-delete-migration! @config "joe")
        (should= nil (sut/-fetch-migration @config "joe"))))

    (context "locking"

      (it "locked?"
        (should= false (sut/-locked? @config))
        (let [lock (db/tx (sut/-fetch-lock @config))]
          (should= false (sut/-locked? @config))
          (db/tx lock :at (time/now))
          (should= true (sut/-locked? @config))))

      (it "lock!"
        (should= false (sut/-locked? @config))
        (should-not-be-nil (sut/-attempt-lock! @config))
        (should= true (sut/-locked? @config))
        (let [lock (sut/-fetch-lock @config)]
          (should= sut/LOCK (:name lock))
          (should= @now (:at lock))))

      (it "lock! - already taken"
        (should-not-be-nil (sut/-attempt-lock! @config))
        (should-be-nil (sut/-attempt-lock! @config))
        (should= 1 (db/count-by :migration :name sut/LOCK)))

      (it "release-lock! - already released"
        (sut/-release-lock! @config)
        (should= nil (:at (sut/-fetch-lock @config))))

      (it "release-lock!"
        (sut/-attempt-lock! @config)
        (sut/-release-lock! @config)
        (should= nil (:at (sut/-fetch-lock @config))))

      (context "wait-for-unlock!"

        (tags :slow)

        (it "fail"
          (sut/-attempt-lock! @config)
          (with-redefs [sut/lock-wait-time   1000
                        sut/lock-check-delay 100]
            (should-throw (sut/-wait-for-unlock! @config))))

        (it "pass"
          (sut/-attempt-lock! @config)
          (future (do (Thread/sleep 500) (sut/-release-lock! @config)))
          (with-redefs [sut/lock-wait-time   1000
                        sut/lock-check-delay 100]
            (should-not-throw (sut/-wait-for-unlock! @config))))
        )
      )

    (context "sync-schemas"

      (around [it] (log/capture-logs (it)))

      (it "creates migration"
        (sut/-ensure-migration-schema! @config)
        (sut/-maybe-sync-schemas-unlocked! @config [spec/thingy])
        (let [migration (sut/-fetch-migration @config sut/SYNC_SCHEMAS)
              logs      (log/parse-captured-logs)]
          (should-not-be-nil migration)
          (should-contain "Synchronizing 1 schema(s) with the database" (map :message logs))))

      (it "won't run within 5 min of last run"
        (sut/-ensure-migration-schema! @config)
        (sut/-create-migration! @config sut/SYNC_SCHEMAS)
        (sut/-maybe-sync-schemas-unlocked! @config [spec/thingy])
        (let [logs (log/parse-captured-logs)]
          (should-not-contain "Synchronizing 1 schema(s) with the database" (map :message logs))))

      (it "updates migration"
        (sut/-ensure-migration-schema! @config)
        (let [migration (sut/-create-migration! @config sut/SYNC_SCHEMAS)]
          (db/tx migration :at (-> 6 time/minutes time/ago)))
        (sut/-maybe-sync-schemas-unlocked! @config [spec/thingy])
        (let [migration (sut/-fetch-migration @config sut/SYNC_SCHEMAS)
              logs      (log/parse-captured-logs)]
          (should> (time/millis-since-epoch (:at migration)) (time/millis-since-epoch (-> 1 time/seconds time/ago)))
          (should-contain "Synchronizing 1 schema(s) with the database" (map :message logs))))

      (it "updates migration with a vector type"
        (sut/-ensure-migration-schema! @config)
        (let [migration (sut/-create-migration! @config sut/SYNC_SCHEMAS)]
          (db/tx migration :at (-> 6 time/minutes time/ago)))
        (sut/-maybe-sync-schemas-unlocked! @config [spec/doodad])
        (let [migration (sut/-fetch-migration @config sut/SYNC_SCHEMAS)
              logs      (log/parse-captured-logs)]
          (should> (time/millis-since-epoch (:at migration)) (time/millis-since-epoch (-> 1 time/seconds time/ago)))
          (should-contain "Synchronizing 1 schema(s) with the database" (map :message logs))))

      (it "enum already installed"
        (sut/-ensure-migration-schema! @config)
        (migrator/-install-schema! (:-db @config) spec/bibelot-states)
        (sut/-maybe-sync-schemas-unlocked! @config [spec/bibelot-states])
        (let [legend (migrator/installed-schema-legend- (:-db @config) [spec/bibelot-states])
              values (-> legend :bibelot.state :values)]
          (should= :bibelot.state (-> legend :bibelot.state :enum))
          (should= 3 (count values))
          (should-contain :pending values)
          (should-contain :active values)
          (should-contain :disabled values)))

      (it "adds and removes enum values"
        (sut/-ensure-migration-schema! @config)
        (migrator/-install-schema! (:-db @config) (assoc spec/bibelot-states :values [:pending :active :blah]))
        (sut/-maybe-sync-schemas-unlocked! @config [spec/bibelot-states])
        (let [legend (migrator/installed-schema-legend- (:-db @config) [spec/bibelot-states])
              values (-> legend :bibelot.state :values)]
          (should= :bibelot.state (-> legend :bibelot.state :enum))
          (should= 3 (count values))
          (should-contain :pending values)
          (should-contain :active values)
          (should-contain :disabled values)))
      )

    (context "migrate!"

      (it "0 to now"
        (log/capture-logs (sut/migrate! @config nil))
        (let [logs (log/parse-captured-logs)]
          (should= ["Lock acquired."
                    "Running Database Migrations UP"
                    "20230101 - applying migration UP"
                    "20230101 UP"
                    "20230202 - applying migration UP"
                    "20230202 UP"
                    "20230303 - applying migration UP"
                    "20230303 UP"
                    "Lock released."] (map :message logs)))
        (should= ["20230101" "20230202" "20230303"] (sut/-applied-migration-names @config)))

      (it "now to 1"
        (sut/-create-migration! @config "20230101")
        (sut/-create-migration! @config "20230202")
        (sut/-create-migration! @config "20230303")
        (log/capture-logs (sut/migrate! @config "20230101"))
        (let [logs (log/parse-captured-logs)]
          (should= ["Lock acquired."
                    "Running Database Migrations DOWN"
                    "20230303 - applying migration DOWN"
                    "20230303 DOWN"
                    "20230202 - applying migration DOWN"
                    "20230202 DOWN"
                    "Lock released."] (map :message logs)))
        (should= ["20230101"] (sut/-applied-migration-names @config)))

      (it "already current"
        (sut/-create-migration! @config "20230101")
        (sut/-create-migration! @config "20230202")
        (sut/-create-migration! @config "20230303")
        (log/capture-logs (sut/migrate! @config nil))
        (let [logs (log/parse-captured-logs)]
          (should= ["No migrations needed."] (map :message logs))))
      )

    )

  )
