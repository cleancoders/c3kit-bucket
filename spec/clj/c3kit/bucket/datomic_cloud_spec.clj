(ns c3kit.bucket.datomic-cloud-spec
  (:require [c3kit.apron.log :as log]
            [c3kit.apron.schema :as s]
            [c3kit.apron.time :as time]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.datomic-common :as common-api]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.datomic-cloud :as sut]
            [c3kit.bucket.migrator :as migrator]
            [c3kit.bucket.spec-helperc :as helper]
            [speclj.core :refer :all])
  (:import (java.util Date)))

(def config {:impl          :datomic-cloud
             :server-type   :datomic-local
             :storage-dir   :mem
             :creds-profile "sandbox"
             :system        "cleancoders-poker-sandbox"
             :db-name       "spec"
             :endpoint      "https://d13mjr7yek.execute-api.us-west-2.amazonaws.com/"
             :region        "us-west-2"})

(def bibelot
  {:kind  (s/kind :bibelot)
   :id    {:type :long}
   :name  {:type :string}
   :size  {:type :long}
   :color {:type :string}})


(declare db)
(declare biby)
(defn sleep [entity] (Thread/sleep 10) entity)

(describe "Datomic Cloud"

  (with-stubs)
  (around [it] (api/with-safety-off (it)))

  (context "api"
    (spec/crud-specs config)
    (spec/nil-value-specs config)
    (spec/find-specs config)
    (spec/filter-specs config)
    (spec/reduce-specs config)
    (spec/count-specs config)
    (spec/kind-in-entity-is-optional config)
    (spec/multi-value-fields config)
    (spec/cas config)
    )

  (context "ref attribute"
    (helper/with-schemas config [spec/bibelot (assoc spec/thingy :fuzz {:type :ref}) spec/disorganized])

    (it "saves just the id"
      (let [bibelot (api/tx {:kind :bibelot :name "bibby"})
            thingy  (api/tx {:kind :thingy :id 123 :name "thingy" :fuzz (:id bibelot)})]
        (should= (:id bibelot) (:fuzz thingy))
        (should (api/entity (:fuzz thingy)))))
    )

  (context "safety"
    (around [it] (with-redefs [api/*safety* true] (it)))

    (it "clear" (should-throw AssertionError (common-api/clear config)))
    (it "delete-all" (should-throw AssertionError (common-api/delete-all config :foo))))

  (context "unique behavior"

    (with db (api/create-db config [spec/bibelot]))

    (it "one kv with nil value"
      (log/capture-logs
       (should= [] (api/find- @db :bibelot :where {:name nil})))
      (should-contain "search for nil value (:bibelot :name), returning no results." (log/captured-logs-str)))

    )

  (context "find-datalog"
    (helper/with-schemas config [bibelot])

    (it "returns proper entity"
      (api/tx {:kind :bibelot :name "bibby"})
      (let [results (sut/find-datalog '[:find (pull ?e [*]) :in $ :where [?e :bibelot/name]])]
        (should= 1 (count results))
        (should= "bibby" (:name (first results)))))
    )

  (context "migrator"

    (with db (api/create-db config []))
    (before (api/clear- @db))

    (it "schema"
      (let [schema (migrator/migration-schema config)]
        (should= :migration (-> schema :kind :value))
        (should= :int (-> schema :id :type))
        (should= [:unique-value] (-> schema :name :db))))

    (it "installed-schema-legend"
      (let [_      (common-api/transact! @db (common-api/->db-schema spec/bibelot false))
            result (migrator/-installed-schema-legend @db {:bibelot spec/bibelot})]
        (should= {:type :string} (-> result :bibelot :name))
        (should= {:type :long} (-> result :bibelot :size))
        (should= {:type :string} (-> result :bibelot :color))))

    (it "install-schema!"
      (let [schema (assoc-in spec/bibelot [:kind :value] :bubble)
            _      (migrator/-install-schema! @db schema)
            result (migrator/-installed-schema-legend @db {:bubble schema})]
        (should= {:type :string} (-> result :bubble :name))
        (should= {:type :long} (-> result :bubble :size))
        (should= {:type :string} (-> result :bubble :color))))

    (it "schema-exists?"
      (should= false (migrator/-schema-exists? @db spec/bibelot))
      (migrator/-install-schema! @db spec/bibelot)
      (should= true (migrator/-schema-exists? @db spec/bibelot)))

    (it "add-attribute!"
      (let [_      (migrator/-add-attribute! @db :gum :name {:type :string})
            result (migrator/-installed-schema-legend @db {:bibelot spec/bibelot})]
        (should= {:type :string} (-> result :gum :name))))

    (it "add-attribute! - schema attr"
      (let [_      (migrator/-add-attribute! @db (assoc-in spec/bibelot [:kind :value] :gum) :name)
            result (migrator/-installed-schema-legend @db {:bibelot spec/bibelot})]
        (should= {:type :string} (-> result :gum :name))))

    (it "remove-attribute!"
      (let [_          (migrator/-install-schema! @db spec/bibelot)
            bibelot    (api/tx- @db {:kind :bibelot :name "red" :size 2 :color "red"})
            _          (migrator/-remove-attribute! @db :bibelot :color)
            reloaded   (api/reload- @db bibelot)
            new-legend (migrator/-installed-schema-legend @db nil)]
        (should= nil (:color reloaded))
        (should-not-contain :color (:bibelot new-legend))))

    (it "remove-attribute! that doesn't exist"
      (migrator/-install-schema! @db spec/bibelot)
      (log/capture-logs
       (should-not-throw (migrator/-remove-attribute! @db :bibelot :fizz))
       (should-not-throw (migrator/-remove-attribute! @db :fizz :bang))))

    (it "remove-attribute! - multi"                         ;
      (let [db         (api/create-db config [])
            _          (migrator/-install-schema! db spec/doodad)
            doodad     (api/tx- db {:kind :doodad :names ["bill" "bob"] :numbers [123 456]})
            _          (migrator/-remove-attribute! db :doodad :numbers)
            reloaded   (api/reload- db doodad)
            new-legend (migrator/-installed-schema-legend db nil)]
        (should= nil (:numbers reloaded))
        (should-not-contain :numbers (:doodad new-legend))))

    (it "rename-attribute!"
      (let [_          (migrator/-install-schema! @db spec/bibelot)
            bibelot    (api/tx- @db {:kind :bibelot :name "red" :size 2 :color "red"})
            _          (migrator/-rename-attribute! @db :bibelot :color :bibelot :hue)
            new-legend (migrator/-installed-schema-legend @db nil)
            reloaded   (api/reload- @db bibelot)]
        (should= nil (:color reloaded))
        (should-not-contain :color (:bibelot new-legend))
        (should= :string (get-in new-legend [:bibelot :hue :type]))))

    (it "rename-attribute! - new attribute exists"
      (migrator/-install-schema! @db spec/bibelot)
      (should-throw (migrator/-rename-attribute! @db :bibelot :color :bibelot :size)))

    (it "rename-attribute! - existing missing"
      (migrator/-install-schema! @db spec/bibelot)
      (log/capture-logs
       (should-not-throw (migrator/-rename-attribute! @db :blah :color :blah :size)))))

  (context "history"
    (helper/with-schemas config [spec/bibelot spec/thingy])
    (with biby (-> (api/tx :kind :bibelot :name "Biby" :size 1 :color "blue")
                   sleep
                   (api/tx :size 2)
                   sleep
                   (api/tx :color "green")
                   sleep
                   (api/tx :size 3 :color "red")))

    (it "of entity"
      (let [history (sut/history @biby)]
        (should= 4 (count history))
        (doseq [h history]
          (should (:db/tx h))
          (should (:db/instant h)))
        (should= {:name "Biby" :size 1 :color "blue"} (select-keys (nth history 0) [:name :size :color]))
        (should= {:name "Biby" :size 2 :color "blue"} (select-keys (nth history 1) [:name :size :color]))
        (should= {:name "Biby" :size 2 :color "green"} (select-keys (nth history 2) [:name :size :color]))
        (should= {:name "Biby" :size 3 :color "red"} (select-keys (nth history 3) [:name :size :color]))))

    (it "created-at"
      (let [moment (sut/created-at @biby)
            now    (time/now)]
        (should-be-a Date moment)
        (should (time/after? moment (-> 1 time/seconds time/ago)))
        (should (time/before? moment now))))

    (it "updated-at"
      (Thread/sleep 10)
      (let [updated (api/tx @biby :size 4)
            moment  (sut/updated-at updated)
            _       (Thread/sleep 10)
            now     (time/now)]
        (should-be-a Date moment)
        (should (time/after? moment (-> 1 time/seconds time/ago)))
        (should (time/before? moment now))
        (should (time/after? moment (sut/created-at updated)))))

    (it "with-timestamps"
      (let [updated (api/tx @biby :size 4)
            result  (sut/with-timestamps updated)]
        (should= (sut/created-at updated) (:db/created-at result))
        (should= (sut/updated-at updated) (:db/updated-at result))))
    )

  )
