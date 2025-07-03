(ns c3kit.bucket.impl-spec
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.bucket.api :as api]
            [speclj.core #?(:clj :refer :cljs :refer-macros) [after after-all around around-all before before before-all
                                                              context describe focus-context focus-describe focus-it it
                                                              pending should should-be should-be-a should-be-nil
                                                              should-be-same should-contain should-end-with should-fail
                                                              should-have-invoked should-invoke should-not should-not
                                                              should-not-be should-not-be-a should-not-be-nil
                                                              should-not-be-same should-not-contain should-not-end-with
                                                              should-not-have-invoked should-not-invoke
                                                              should-not-start-with should-not-throw should-not=
                                                              should-not== should-start-with should-throw should<
                                                              should<= should= should== should> should>= stub tags
                                                              with with-all with-stubs xit redefs-around]]
            [c3kit.bucket.api :as sut #?(:clj :refer :cljs :refer-macros) [with-safety-off]]
            [c3kit.bucket.spec-helperc :as helper]
            [c3kit.apron.log :as log]
            [c3kit.apron.schema :as s]
            [c3kit.apron.time :as time :refer [ago minutes]]))

(def bibelot
  {:kind  (s/kind :bibelot)
   :id    {:type :long}
   :name  {:type :string}
   :size  {:type :long}
   :color {:type :string}})

(def thingy
  {:kind    (s/kind :thingy)
   :id      {:type :int}
   :foo     {:type :string}
   :bar     {:type :long}
   :fizz    {:type :long}
   :spell   {:type :long}
   :truthy? {:type :boolean}
   :bang    {:type :instant}
   :name    {:type :string}})

(def doodad
  {:kind    (s/kind :doodad)
   :id      s/id
   :names   {:type [:string]}
   :numbers {:type [:long]}
   :letters {:type [:keyword]}})

(def disorganized
  {:name {:type :string}
   :type {:type :string}
   :id   {:type :long}
   :kind (s/kind :disorganized)})

;(def timepiece
;  {:kind       (s/kind :timepiece)
;   :id         {:type :long :db {:type "bigint IDENTITY PRIMARY KEY"}}
;   :created-at {:type :instant}
;   :updated-at {:type :instant}
;   :date       {:type :date}
;   :timestamp  {:type :timestamp}
;   :value      {:type :string :db {:type "varchar(255)"}}})

(def child :undefined)
(def original :undefined)
(def now :undefined)

(defn set-find-by [kind & kvs]
  (set (apply sut/find-by kind kvs)))

(defn crud-specs [config]

  (context "CRUD"
    (with-stubs)

    (helper/with-schemas config [bibelot thingy disorganized])

    (it "returns nil on missing id"
      (log/capture-logs
        (should= nil (sut/entity :bibelot -1))
        (should= nil (sut/entity :bibelot nil))
        (should= nil (sut/entity :bibelot ""))
        (should= nil (sut/entity :bibelot "-1"))
        ))

    (it "tx nil entities"
      (should-not-throw (sut/tx nil))
      (should-not-throw (sut/tx* [nil])))

    (it "tx empty collections"
      (with-redefs [sut/-tx* (stub :-tx*)]
        (sut/tx* nil)
        (sut/tx* [])
        (sut/tx* [nil])
        (sut/tx* [nil nil])
        (should-not-have-invoked :-tx*))
      (should-be-nil (sut/tx* [nil nil])))

    (it "creates one"
      (let [saved (sut/tx {:kind :bibelot :name "thingy"})]
        (should= :bibelot (:kind saved))
        (should= "thingy" (:name saved))))

    (it "creates one - disorganized schema"
      (let [saved (sut/tx {:kind :disorganized :name "messy" :type "drawer"})]
        (should= :disorganized (:kind saved))
        (should= "messy" (:name saved))
        (should= "drawer" (:type saved))))

    (it "creates one - pre-populated"
      (let [saved (sut/tx {:kind :thingy :id 123 :name "thingy"})]
        (should= :thingy (:kind saved))
        (should= "thingy" (:name saved))))

    (it "updates one"
      (let [saved   (sut/tx {:kind :bibelot :name "thingy"})
            updated (sut/tx saved :name "new-y")]
        (should= "new-y" (:name updated))))

    (it "reads created"
      (let [saved  (sut/tx {:kind :bibelot :name "thingy"})
            loaded (sut/entity :bibelot (:id saved))]
        (should= :bibelot (:kind loaded))
        (should= "thingy" (:name loaded))
        (should= (:id saved) (:id loaded))))

    (it "creates many"
      (let [thing-one {:kind :bibelot :name "thing-one"}
            thing-two {:kind :thingy :id 123 :foo "thing-two"}
            things    (sut/tx* [thing-one thing-two])
            [saved-one saved-two] things]
        (should= 2 (count things))
        (should-not-be-nil (:id (second things)))
        (should= :bibelot (:kind saved-one))
        (should= :thingy (:kind saved-two))
        (should= "thing-one" (:name saved-one))
        (should= "thing-two" (:foo saved-two))))

    (it "entity! by id"
      (let [saved (sut/tx {:kind :bibelot :name "thingy"})]
        (should= saved (sut/entity! :bibelot (:id saved)))
        (should-throw (sut/entity! :bibelot 9999))))

    (it "entity! by id - coerced"
      (let [saved (sut/tx {:kind :bibelot :name "thingy"})]
        (should= saved (sut/entity! :bibelot (str (:id saved))))
        (should-throw (sut/entity! :bibelot "9999"))))

    (it "entity! by entity"
      (let [saved (sut/tx {:kind :bibelot :name "thingy"})]
        (should= saved (sut/entity! :bibelot saved))
        (should-throw (sut/entity! :bibelot (assoc saved :id 9999)))))

    (it "updating"
      (let [saved   (sut/tx {:kind :bibelot :name "thingy"})
            updated (sut/tx saved :name "whatsamajigger")
            loaded  (sut/entity :bibelot (:id saved))]
        (should= "whatsamajigger" (:name loaded))
        (should= (:id saved) (:id loaded))
        (should= (:id saved) (:id updated))))

    (it "updating entity with missing fields doesn't clear them"
      (let [saved   (sut/tx {:kind :bibelot :name "thingy" :size 42 :color "pink"})
            updated (sut/tx saved :name "whatsamajigger")
            loaded  (sut/entity :bibelot (:id saved))]
        (should= "whatsamajigger" (:name updated))
        (should= "whatsamajigger" (:name loaded))
        (should= 42 (:size updated))
        (should= 42 (:size loaded))
        (should= "pink" (:color updated))
        (should= "pink" (:color loaded))))

    (it "updating many"
      (let [saved-one  (sut/tx {:kind :bibelot :name "thingy"})
            saved-two  (sut/tx {:kind :bibelot :name "another thing"})
            [updated-one updated-two :as updates] (sut/tx* [(assoc saved-one :name "whatsamajigger") (assoc saved-two :name "whatchamacallit")])
            loaded-one (sut/entity :bibelot (:id saved-one))
            loaded-two (sut/entity :bibelot (:id saved-two))]
        (should= 2 (count updates))
        (should= "whatsamajigger" (:name loaded-one))
        (should= (:id saved-one) (:id loaded-one))
        (should= (:id saved-two) (:id loaded-two))
        (should= (:id saved-one) (:id updated-one))
        (should= (:id saved-two) (:id updated-two))))

    (it "delete via metadata"
      (let [saved   (sut/tx {:kind :bibelot :name "thingy"})
            updated (sut/tx (with-meta saved {:db/delete? true}))]
        (should= nil (sut/entity :bibelot (:id saved)))
        (should= {:kind :bibelot :id (:id saved) :db/delete? true} updated)))

    (it "deleting via :db/delete? attr"
      (let [saved   (sut/tx {:kind :bibelot :name "thingy"})
            updated (sut/tx (assoc saved :db/delete? true))]
        (should= nil (sut/entity :bibelot (:id saved)))
        (should= {:kind :bibelot :id (:id saved) :db/delete? true} updated)))

    (it "deleting when passed an entity"
      (let [saved     (sut/tx {:kind :bibelot :name "thingy"})
            retracted (sut/delete saved)]
        (should= {:kind :bibelot :id (:id saved) :db/delete? true} retracted)
        (should= nil (sut/entity :bibelot (:id saved)))))

    (it "deleting when passed an id"
      (let [saved     (sut/tx {:kind :bibelot :name "thingy"})
            retracted (sut/delete :bibelot (:id saved))]
        (should= {:kind :bibelot :id (:id saved) :db/delete? true} retracted)
        (should= nil (sut/entity :bibelot (:id saved)))))

    (it "delete-all of a kind of an entity"
      (let [b1 (sut/tx {:kind :bibelot :name "thing 1"})
            b2 (sut/tx {:kind :bibelot :name "thing 2"})]
        (sut/delete-all :bibelot)
        (should= [] (sut/find :bibelot))
        (should= nil (sut/reload b1))
        (should= nil (sut/reload b2))))

    (context "clears"
      (it "it clears the db"
        (sut/tx {:kind :bibelot :name "hello"})
        (sut/clear)
        (should= [] (sut/find :bibelot)))
      )

    ;(context "timestamps"
    ;
    ;  (helper/with-schemas db-impl [timepiece])
    ;  (with now (time/now))
    ;  (with-stubs)
    ;  (wire-helperc/stub-now @now)
    ;
    ;  (it "created-at only populated once"
    ;    (let [entity  (sut/tx {:kind :timepiece :value "foo"})
    ;          day-ago (-> 1 time/days time/ago)
    ;          updated (sut/tx entity :created-at day-ago)]
    ;      (should= (time/unparse :dense @now) (time/unparse :dense (:created-at entity)))
    ;      (should= (time/unparse :dense day-ago) (time/unparse :dense (:created-at updated)))
    ;      (should= (time/unparse :dense day-ago) (time/unparse :dense (:created-at (sut/tx updated :value "bar"))))))
    ;
    ;  (it "updated-at always updated"
    ;    (let [entity  (sut/tx {:kind :timepiece :value "foo"})
    ;          day-ago (-> 1 time/days time/ago)
    ;          updated (sut/tx entity :updated-at day-ago)]
    ;      (should= (time/unparse :dense @now) (time/unparse :dense (:updated-at entity)))
    ;      (should= (time/unparse :dense @now) (time/unparse :dense (:updated-at updated)))))
    ;
    ;  (it "with tx*"
    ;    (let [entity (first (sut/tx* [{:kind :timepiece :value "foo"}]))]
    ;      (should= (time/unparse :dense @now) (time/unparse :dense (:created-at entity)))
    ;      (should= (time/unparse :dense @now) (time/unparse :dense (:updated-at entity)))))
    ;
    ;  )

    (context "tx*"

      (it "add and delete"
        (let [b1     (sut/tx {:kind :bibelot :name "thing 1"})
              result (sut/tx* [(assoc b1 :db/delete? true)
                               {:kind :bibelot :name "thing 2"}])]
          (should= 2 (count result))
          (should= {:kind :bibelot :id (:id b1) :db/delete? true} (first result))
          (should= nil (sut/reload b1))
          (should= "thing 2" (:name (second result)))
          (should= "thing 2" (:name (sut/reload (second result))))))

      (it "failed transaction"
        (let [b1 (sut/tx {:kind :bibelot :name "thing 1"})]
          (should-throw (sut/tx* [(assoc b1 :db/delete? true)
                                  {:kind :bibelot :name "thing 2"}
                                  {:kind :blah :crash "burn"}]))
          (should= b1 (sut/reload b1))
          (should= nil (sut/ffind-by :bibelot :name "thing 2"))
          ))
      )
    )
  )

(defn multi-value-fields [config]
  (context "multi-value fields"

    (helper/with-schemas config [doodad])

    (it "loading"
      (let [saved  (sut/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
            loaded (sut/entity (:id saved))]
        (should= (:id loaded) (:id saved))
        (should= #{"foo" "bar"} (set (:names loaded)))
        (should= #{8 42} (set (:numbers loaded)))))

    (it "find by attribute"
      (let [saved  (sut/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
            loaded (sut/find-by :doodad :names "bar")]
        (should= 1 (count loaded))
        (should= (:id saved) (:id (first loaded)))))

    (it "retracting [string] value"
      (let [saved   (sut/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
            updated (sut/tx saved :names nil)]
        (should-be-nil (seq (:names updated)))))

    (it "retracting one value from [string]"
      (let [saved   (sut/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
            updated (sut/tx saved :names ["foo"])]
        (should= #{"foo"} (set (:names updated)))))

    (it "adding one value to [string]"
      (let [saved   (sut/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
            updated (sut/tx saved :names ["foo" "bar" "fizz"])]
        (should= #{"foo" "bar" "fizz"} (set (:names updated)))))

    (it "find 'not="
      (let [d1 (sut/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
            d2 (sut/tx {:kind :doodad :names ["foo" "bang"] :numbers [8 43]})]
        (should= [d1] (sut/find-by :doodad :names "foo" :numbers ['not= 43]))
        (should= [d2] (sut/find-by :doodad :names "foo" :numbers ['not= 42]))
        (should= [] (sut/find-by :doodad :names "foo" :numbers ['not= 42 43]))
        (should= [d2] (sut/find-by :doodad :names ['not= "bar"] :numbers 8))
        (should= [d1] (sut/find-by :doodad :names ['not= "bang"] :numbers 8))
        (should= [] (sut/find-by :doodad :names ['not= "bar" "bang"] :numbers 8))))

    (it "find or"
      (let [d1 (sut/tx {:kind :doodad :names ["foo" "bar"] :numbers [8 42]})
            d2 (sut/tx {:kind :doodad :names ["foo" "bang"] :numbers [8 43]})]
        (should= (set [d1 d2]) (set-find-by :doodad :names ["foo" "BLAH"]))
        (should= (set [d1 d2]) (set-find-by :doodad :names ["bar" "bang"]))
        (should= [d1] (sut/find-by :doodad :names ["bar" "BLAH"]))
        (should= [] (sut/find-by :doodad :names ["ARG" "BLAH"]))))
    )
  )

(defn kind-in-entity-is-optional [config]
  (context "kind in (entity) is optional"

    (helper/with-schemas config [bibelot thingy])

    (it "entity"
      (let [foo (sut/tx {:kind :bibelot :name "foo"})]
        (should= foo (sut/entity (:id foo)))
        (should= foo (sut/entity :bibelot (:id foo)))
        (should= nil (sut/entity :thingy (:id foo)))))

    (it "entity!"
      (let [foo (sut/tx {:kind :bibelot :name "foo"})]
        (should= foo (sut/entity! (:id foo)))
        (should= foo (sut/entity! :bibelot (:id foo)))
        (should-throw (sut/entity! :thingy (:id foo)))))
    )
  )

(defn kind-is-required [db-ctor]
  (context "kind is required"

    (helper/with-schemas db-ctor [bibelot thingy])

    (it "entity"
      (let [foo (sut/tx {:kind :bibelot :name "foo"})]
        (should-throw #?(:clj UnsupportedOperationException) (sut/entity (:id foo)))
        (should= nil (sut/entity :thingy (:id foo)))))

    (it "entity!"
      (let [foo (sut/tx {:kind :bibelot :name "foo"})]
        (should-throw #?(:clj UnsupportedOperationException) (sut/entity! (:id foo)))
        (should-throw (sut/entity! :thingy (:id foo)))))
    )
  )

(defn find-specs [config]
  (context "find"
    (helper/with-schemas config [bibelot thingy])

    (it "empty db"
      (should= [] (sut/find :bibelot))
      (should= [] (sut/find :thingy))
      (should= [] (sut/find-by :bibelot :name "nothing")))

    (context "(populated db)"
      (before (sut/clear)
              (sut/tx {:kind :bibelot :name "hello"})
              (sut/tx {:kind :bibelot :name "world"})
              (sut/tx {:kind :bibelot :name "world" :size 2})
              (sut/tx {:kind :bibelot :name "hi!" :size 2}))

      (it "all"
        (sut/tx {:kind :thingy :id 123 :name "world"})
        (should= 4 (count (sut/find :bibelot)))
        (should= 1 (count (sut/find :thingy))))

      (it ":take option"
        (let [all (sut/find :bibelot)]
          (should= (take 1 all) (sut/find :bibelot {:take 1}))
          (should= (take 2 all) (sut/find :bibelot {:take 2}))
          (should= (take 3 all) (sut/find :bibelot {:take 3}))
          (should= all (sut/find :bibelot {:take 4}))
          (should= all (sut/find :bibelot {:take 99}))))

      (it ":drop option"
        (let [all (sut/find :bibelot)]
          (should= (drop 1 all) (sut/find :bibelot {:drop 1}))
          (should= (drop 2 all) (sut/find :bibelot {:drop 2}))
          (should= (drop 3 all) (sut/find :bibelot {:drop 3}))
          (should= [] (sut/find :bibelot {:drop 4}))
          (should= [] (sut/find :bibelot {:drop 99}))))

      (it "drop and take options (pagination)"
        (let [all (sut/find :bibelot)]
          (should= (take 1 (drop 1 all)) (sut/find :bibelot {:drop 1 :take 1}))
          (should= (take 1 (drop 2 all)) (sut/find :bibelot {:drop 2 :take 1}))
          (should= (take 3 all) (sut/find :bibelot {:drop 0 :take 3}))))

      (it ":name"
        (let [[entity :as entities] (sut/find-by :bibelot :name "hello")]
          (should= 1 (count entities))
          (should= "hello" (:name entity))
          (should= nil (:size entity))))

      (it "two attributes"
        (let [[entity :as entities] (sut/find-by :bibelot :name "world" :size 2)]
          (should= 1 (count entities))
          (should= "world" (:name entity))
          (should= 2 (:size entity))))

      (it "returns all found"
        (let [entities (sut/find-by :bibelot :name "world")
              world-1  (first (remove :size entities))
              world-2  (ccc/ffilter :size entities)]
          (should= 2 (count entities))
          (should= "world" (:name world-1))
          (should= nil (:size world-1))
          (should= "world" (:name world-2))
          (should= 2 (:size world-2))))

      (it "all by size"
        (let [entities (sut/find-by :bibelot :size 2)]
          (should= 2 (count entities))
          (should-contain "world" (map :name entities))
          (should-contain "hi!" (map :name entities))))

      (it "nil size"
        (let [entities (sut/find-by :bibelot :name "world" :size nil)]
          (should= 1 (count entities))
          (should= "world" (:name (first entities)))))

      (it "nil name"
        (should= [] (sut/find-by :bibelot :size 2 :name nil))
        (let [nil-name (sut/tx {:kind :bibelot :name nil :size 2})]
          (should= [nil-name] (sut/find-by :bibelot :size 2 :name nil))))

      (it "ffind-by"
        (let [world (sut/ffind-by :bibelot :name "world" :size 2)]
          (should= "world" (:name world))))

      )
    )
  )

(defn filter-specs [config]
  (context "filters"
    (helper/with-schemas config [bibelot thingy])

    (context "(populated db)"
      (before (sut/clear)
              (sut/tx {:kind :bibelot :name "hello"})
              (sut/tx {:kind :bibelot :name "world"})
              (sut/tx {:kind :bibelot :name "world" :size 2})
              (sut/tx {:kind :bibelot :name "hi!" :size 2}))

      (it "="
        (let [result       (sut/find-by :bibelot :name ['= "hi!"])
              result-names (map :name result)]
          (should-contain "hi!" result-names)
          (should-not-contain "world" result-names)))

      (it "= many"
        (let [result       (sut/find-by :bibelot :name ['= "hi!" "world"])
              result-names (map :name result)]
          (should-contain "hi!" result-names)
          (should-contain "world" result-names)
          (should-not-contain "hello" result-names)))

      (it "not= nil"
        (let [result       (sut/find-by :bibelot :size ['not= nil])
              result-names (map :name result)]
          (should-not-contain "hello" result-names)
          (should-contain "world" result-names)
          (should-contain "hi!" result-names)))

      (it "or"
        (let [b1  (sut/tx :kind :bibelot :name "Bee" :color "red" :size 1)
              b2  (sut/tx :kind :bibelot :name "Bee" :color "blue" :size 2)
              b3  (sut/tx :kind :bibelot :name "Ant" :color "blue" :size 1)
              _b4 (sut/tx :kind :bibelot :color "blue" :size 1)]
          (should= (set [b1 b2 b3]) (set-find-by :bibelot :name ["Bee" "Ant"]))
          (should= (set [b1 b2 b3]) (set-find-by :bibelot :name #{"Bee" "Ant"}))
          (should= [b3] (sut/find-by :bibelot :name ["BLAH" "Ant"]))
          ;(should= [b4] (sut/find-by :bibelot :name [nil]))
          ;(should= [b3 b4] (sut/find-by :bibelot :name ["BLAH" nil "Ant"] :size ['not= nil] ))
          ;(should= [b2] (sut/find-by :bibelot :name ["Bee" nil] :size 2))
          ;(should= [b3 b4] (sut/find-by :bibelot :name ["Ant" nil] :size ['not= nil] ))
          (should= [] (sut/find-by :bibelot :name ["BLAH" "BLAH" "BLAH"]))
          (should= [] (sut/find-by :bibelot :name ["BLAH" "ARG"]))
          (should= [] (sut/find-by :bibelot :name []))))

      (it "< > string"
        (let [result       (set-find-by :bibelot :name ['> "g"] :name ['< "i"])
              result-names (map :name result)]
          (should-contain "hi!" result-names)
          (should-not-contain "world" result-names)))

      (it "<= >= string"
        (sut/clear)
        (let [b1 (sut/tx :kind :bibelot :name "aaa")
              b2 (sut/tx :kind :bibelot :name "bbb")
              b3 (sut/tx :kind :bibelot :name "ccc")]
          (should= (set [b1 b2 b3]) (set-find-by :bibelot :name ['>= "aaa"]))
          (should= (set [b2 b3]) (set-find-by :bibelot :name ['>= "bbb"]))
          (should= (set [b1 b2 b3]) (set-find-by :bibelot :name ['<= "ccc"]))
          (should= (set [b1 b2]) (set-find-by :bibelot :name ['<= "bbb"]))))

      (it "< <= > >= long"
        (sut/clear)
        (let [b1 (sut/tx :kind :bibelot :size 1)
              b2 (sut/tx :kind :bibelot :size 2)
              b3 (sut/tx :kind :bibelot :size 3)]
          (should= (set [b1 b2 b3]) (set-find-by :bibelot :size ['> 0]))
          (should= (set [b1 b2 b3]) (set-find-by :bibelot :size ['>= 1]))
          (should= (set [b2 b3]) (set-find-by :bibelot :size ['> 1]))
          (should= [b3] (sut/find-by :bibelot :size ['> 2]))
          (should= [] (sut/find-by :bibelot :size ['> 3]))
          (should= (set [b1 b2 b3]) (set-find-by :bibelot :size ['< 4]))
          (should= (set [b1 b2 b3]) (set-find-by :bibelot :size ['<= 3]))
          (should= (set [b1 b2]) (set-find-by :bibelot :size ['< 3]))
          (should= [b1] (sut/find-by :bibelot :size ['< 2]))
          (should= [] (sut/find-by :bibelot :size ['< 1]))))

      (it "< <= > >= date"
        (let [thing1 (sut/tx :kind :thingy :id 123 :bang (-> 1 minutes ago))
              thing2 (sut/tx :kind :thingy :id 456 :bang (-> 3 minutes ago))
              thing3 (sut/tx :kind :thingy :id 789 :bang (-> 5 minutes ago))]
          (should= [thing2 thing3] (reverse (sort-by :bang (sut/find-by :thingy :bang ['< (-> 2 minutes ago)]))))
          (should= [thing2 thing3] (reverse (sort-by :bang (sut/find-by :thingy :bang ['<= (:bang thing2)]))))
          (should= [thing1] (sut/find-by :thingy :bang ['> (-> 2 minutes ago)]))
          (should= [thing1 thing2] (reverse (sort-by :bang (sut/find-by :thingy :bang ['>= (:bang thing2)]))))
          (should= [thing2] (sut/find-by :thingy :bang ['< (-> 2 minutes ago)] :bang ['> (-> 4 minutes ago)]))))

      (it "compare against entity with nil value"
        (sut/clear)
        (let [b1  (sut/tx :kind :bibelot :name "1" :size 1)
              _b2 (sut/tx :kind :bibelot :name "nil" :size nil)]
          (should= [b1] (sut/find-by :bibelot :size ['>= 0]))))

      (it "compare against boolean value"
        (sut/clear)
        (let [_thing1 (sut/tx :kind :thingy :id 123 :truthy? true)
              thing2  (sut/tx :kind :thingy :id 124 :truthy? false)
              _thing3 (sut/tx :kind :thingy :id 125 :truthy? nil)]
          (should= [thing2] (sut/find-by :thingy :truthy? false))))

      (it "like fuzzy match with anything before or after"
        (let [result       (sut/find-by :bibelot :name ['like "%orl%"])
              result-names (map :name result)]
          (should-contain "world" result-names)
          (should-not-contain "hi!" result-names)))

      (it "case-insensitive fuzzy match"
        (let [result       (sut/find-by :bibelot :name ['ilike "%OrL%"])
              result-names (map :name result)]
          (should-contain "world" result-names)
          (should-not-contain "hi!" result-names)))

      (it "like fuzzy match with anything after"
        (let [_            (sut/tx {:kind :bibelot :name "hello world"})
              result       (sut/find-by :bibelot :name ['like "worl%"])
              result-names (map :name result)]
          (should-contain "world" result-names)
          (should-not-contain "hello world" result-names)))

      (it "like fuzzy match with _"
        (let [_            (sut/tx {:kind :bibelot :name "words"})
              result       (sut/find-by :bibelot :name ['like "wor__"])
              result-names (map :name result)]
          (should-contain "world" result-names)
          (should-contain "words" result-names)
          (should-not-contain "hello" result-names)))

      (it "like with exact match"
        (let [_            (sut/tx {:kind :bibelot :name "words"})
              result       (sut/find-by :bibelot :name ['like "world"])
              result-names (map :name result)]
          (should-contain "world" result-names)
          (should-not-contain "words" result-names)
          (should-not-contain "hello" result-names)))
      )
    )
  )

(defn reduce-specs [config]
  (context "reduce"
    (helper/with-schemas config [bibelot thingy])
    (before (sut/clear)
            (sut/tx {:kind :bibelot :name "hello"})
            (sut/tx {:kind :bibelot :name "world"})
            (sut/tx {:kind :bibelot :name "world" :size 2})
            (sut/tx {:kind :bibelot :name "hi!" :size 2}))

    (it "sum of none"
      (should= 0 (sut/reduce :bibelot #(+ %1 (:size %2)) 0 :where {:name "fake"})))

    (it "sum of one"
      (should= 2 (sut/reduce :bibelot #(+ %1 (:size %2)) 0 :where {:name "hi!"})))

    (it "sum of all"
      (should= 4 (sut/reduce :bibelot #(+ %1 (or (:size %2) 0)) 0)))

    (it "reduced map"
      (should= {"world" [2] "hi!" [2]} (sut/reduce :bibelot #(update %1 (:name %2) conj (:size %2)) {}
                                                   :where {:size ['not= nil]})))
    )
  )

(defn count-specs [config]
  (context "count"
    (helper/with-schemas config [bibelot thingy])

    (it "empty db"
      (should= 0 (sut/count :bibelot))
      (should= 0 (sut/count-by :bibelot :name "nothing")))

    (context "populated db"
      (before (sut/clear)
              (sut/tx {:kind :bibelot :name "hello"})
              (sut/tx {:kind :bibelot :name "world"})
              (sut/tx {:kind :bibelot :name "world" :size 2})
              (sut/tx {:kind :bibelot :name "hi!" :size 2}))

      (it "all"
        (sut/tx {:kind :thingy :id 123 :name "world"})
        (should= 4 (sut/count :bibelot))
        (should= 1 (sut/count :thingy)))

      (it "count-by: :name"
        (should= 1 (sut/count-by :bibelot :name "hello")))

      (it "count-by: two attributes"
        (should= 1 (sut/count-by :bibelot :name "world" :size 2)))
      )
    )
  )

(defn nil-value-specs [config]

  (context "retracting null values"

    (helper/with-schemas config [bibelot thingy])

    (with child (sut/tx {:kind :bibelot :name "child" :color "golden"}))
    (with original (sut/tx {:kind    :thingy
                            :id      123
                            :foo     "foo"
                            :bar     (:id @child)
                            :truthy? true
                            :fizz    1234
                            :bang    (time/now)}))


    (it "can set a string to nil"
      (let [result (sut/tx (assoc @original :foo nil))]
        (should= nil (:foo result))
        (should= nil (:foo (sut/reload @original)))))

    (it "can set a uuid to nil"
      (let [result (sut/tx (assoc @original :bar nil))]
        (should= nil (:bar result))
        (should= nil (:bar (sut/reload @original)))))

    (it "can set an int to nil"
      (let [result (sut/tx (assoc @original :fizz nil))]
        (should= nil (:fizz result))
        (should= nil (:fizz (sut/reload @original)))))

    (it "can set a boolean to nil"
      (let [result (sut/tx (assoc @original :truthy? nil))]
        (should-be-nil (:truthy? result))
        (should-be-nil (:truthy? (sut/reload @original)))))

    (it "can set an instant to nil"
      (let [result (sut/tx (assoc @original :bang nil))]
        (should-be-nil (:bang result))
        (should-be-nil (:bang (sut/reload @original)))))
    )
  )

(defn cas [config]

  (context "cas"
    (helper/with-schemas config [bibelot thingy])

    (it "empty"
      (let [red (sut/tx {:kind :bibelot :name "red" :size 1 :color "red"})]
        (should= red (sut/tx (sut/cas {} red)))))

    (it "against full entity"
      (let [red (sut/tx {:kind :bibelot :name "red" :size 1 :color "red"})]
        (should= red (sut/tx (sut/cas red red)))))

    (it "matching 1"
      (let [red (sut/tx {:kind :bibelot :name "red" :size 1 :color "red"})]
        (should= red (sut/tx (sut/cas {:name "red"} red)))
        (should= red (sut/tx (sut/cas {:color "red"} red)))
        (should= red (sut/tx (sut/cas {:size 1} red)))
        (should= red (sut/tx (sut/cas {:name "red" :size 1 :color "red"} red)))))

    (it "matching 2"
      (let [red  (sut/tx {:kind :bibelot :name "red" :size 1 :color "red"})
            blue (sut/tx {:kind :bibelot :name "blue" :size 1 :color "red"})]
        (should= red (sut/tx (sut/cas {:color "red"} red)))
        (should= blue (sut/reload blue))))

    (it "mis-matching"
      (let [red (sut/tx {:kind :bibelot :name "red" :size 1 :color "red"})]
        (should-throw (sut/tx (sut/cas {:name "blue"} red)))
        (should-throw (sut/tx (sut/cas {:color "green"} red)))
        (should-throw (sut/tx (sut/cas {:size 2} red)))))

    (it "mis-match in tx*"
      (let [red   (sut/tx {:kind :bibelot :name "red" :size 1 :color "red"})
            green (sut/tx {:kind :bibelot :name "green" :size 2 :color "green"})]
        (should-throw (sut/tx* [(sut/cas {:name "blue"} (assoc red :size 9)) (assoc green :size 9)]))
        (should= 1 (:size (sut/reload red)))
        (should= 2 (:size (sut/reload green)))))

    )

  )

(defn broken-in-datomic [config]

  (context "broken in datomic"
    (helper/with-schemas config [bibelot thingy])
    (before (sut/tx {:kind :bibelot :name "hello"})
            (sut/tx {:kind :bibelot :name "world"})
            (sut/tx {:kind :bibelot :name "world" :size 2})
            (sut/tx {:kind :bibelot :name "hi!" :size 2}))

    (context "find-by"

      (it "or with nils"
        (let [_b1 (sut/tx :kind :bibelot :name "Bee" :color "red" :size 1)
              b2  (sut/tx :kind :bibelot :name "Bee" :color "blue" :size 2)
              b3  (sut/tx :kind :bibelot :name "Ant" :color "blue" :size 1)
              b4  (sut/tx :kind :bibelot :color "blue" :size 1)]
          (should= [b4] (sut/find-by :bibelot :name [nil]))
          (should= [b3 b4] (sut/find-by :bibelot :name ["BLAH" nil "Ant"] :size ['not= nil]))
          (should= [b2] (sut/find-by :bibelot :name ["Bee" nil] :size 2))
          (should= [b3 b4] (sut/find-by :bibelot :name ["Ant" nil] :size ['not= nil]))))

      (it "= nil"
        (let [result (sut/find-by :bibelot :name "world" :size ['= nil])]
          (should= ["world"] (map :name result))))

      (it "not= many with nil"
        (let [result (sut/find-by :bibelot :size ['not= nil 2])]
          (should= [] (map :name result))))

      (it "not= many"
        (let [result       (sut/find-by :bibelot :name ['not= "hello" "world"])
              result-names (map :name result)]
          (should-contain "hi!" result-names)
          (should-not-contain "hello" result-names)
          (should-not-contain "world" result-names)))

      (it "not= with nothing"
        (let [result (sut/find-by :bibelot :name ['not=])
              all    (sut/find :bibelot)]
          (should= (set all) (set result))))

      (it "not="
        (let [result       (sut/find-by :bibelot :name ['not= "hi!"])
              result-names (map :name result)]
          (should-not-contain "hi!" result-names)
          (should-contain "hello" result-names)
          (should-contain "world" result-names)))
      )
    )

  )
