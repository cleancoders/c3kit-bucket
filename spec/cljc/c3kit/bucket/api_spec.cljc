(ns c3kit.bucket.api-spec
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [after after-all around around-all before before before-all
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
                                                              with with-all with-stubs xit]]
            [c3kit.bucket.api :as sut]
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
  {:kind (s/kind :thingy)
   :id   {:type :int}
   :foo  {:type :string}
   :bar  {:type :long}
   :fizz {:type :long}
   :bang {:type :instant}
   :name {:type :string}})

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

(defn crud-specs [db-impl]

  (context "CRUD"

    (helper/with-schemas db-impl [bibelot thingy])

    (it "returns nil on missing id"
      (log/capture-logs
        (should= nil (sut/entity :bibelot -1))
        (should= nil (sut/entity :bibelot nil))
        (should= nil (sut/entity :bibelot ""))
        (should= nil (sut/entity :bibelot "-1"))))

    (it "tx nil entities"
      (should-not-throw (sut/tx nil))
      (should-not-throw (sut/tx* [nil])))

    (it "creates one"
      (let [saved (sut/tx {:kind :bibelot :name "thingy"})]
        (should= :bibelot (:kind saved))
        (should= "thingy" (:name saved))))

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
        (should-throw (sut/entity! :bibelot 9999))))

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
        (should= [] (sut/find-all :bibelot))
        (should= nil (sut/reload b1))
        (should= nil (sut/reload b2))))

    (context "clears"
      (it "it clears the db"
        (sut/tx {:kind :bibelot :name "hello"})
        (sut/clear)
        (should= [] (sut/find-all :bibelot)))
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
          (should= nil (sut/ffind-by :bibelot :name "thing 2"))))
      )
    )
  )

(defn find-by [db-impl]
  (context "find-by"
    (helper/with-schemas db-impl [bibelot thingy])

    (it "empty db"
      (should= [] (sut/find-by :bibelot :name "nothing")))

    (context "populated db"
      (before (sut/clear)
              (sut/tx {:kind :bibelot :name "hello"})
              (sut/tx {:kind :bibelot :name "world"})
              (sut/tx {:kind :bibelot :name "world" :size 2})
              (sut/tx {:kind :bibelot :name "hi!" :size 2})
              )

      (it "find-by: :name"
        (let [[entity :as entities] (sut/find-by :bibelot :name "hello")]
          (should= 1 (count entities))
          (should= "hello" (:name entity))
          (should= nil (:size entity))))

      (it "find-by: two attributes"
        (let [[entity :as entities] (sut/find-by :bibelot :name "world" :size 2)]
          (should= 1 (count entities))
          (should= "world" (:name entity))
          (should= 2 (:size entity))))

      (it "find-by: returns all found"
        (let [[world-1 world-2 :as entities] (sut/find-by :bibelot :name "world")]
          (should= 2 (count entities))
          (should= "world" (:name world-1))
          (should= nil (:size world-1))
          (should= "world" (:name world-2))
          (should= 2 (:size world-2))
          ))

      (it "find-by: all by size"
        (let [[world hi! :as entities] (sut/find-by :bibelot :size 2)]
          (should= 2 (count entities))
          (should= "world" (:name world))
          (should= "hi!" (:name hi!))))

      (it "find-by: nil size"
        (let [[hello world :as entities] (sut/find-by :bibelot :size nil)]
          (should= 2 (count entities))
          (should= "hello" (:name hello))
          (should= "world" (:name world))))

      (it "nil"
        (should= [] (sut/find-by :bibelot :name nil))
        (let [nil-name (sut/tx {:kind :bibelot :name nil :size 2})]
          (should= [nil-name] (sut/find-by :bibelot :name nil))))

      (it "first-find-by"
        (let [world (sut/ffind-by :bibelot :name "world" :size 2)]
          (should= "world" (:name world))))

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

      (it "= nil"
        (let [result       (sut/find-by :bibelot :size ['= nil])
              result-names (map :name result)]
          (should-contain "hello" result-names)
          (should-contain "world" result-names)
          (should-not-contain "hi!" result-names)))

      (it "not="
        (let [result       (sut/find-by :bibelot :name ['not= "hi!"])
              result-names (map :name result)]
          (should-not-contain "hi!" result-names)
          (should-contain "hello" result-names)
          (should-contain "world" result-names)))

      (it "not= many"
        (let [result       (sut/find-by :bibelot :name ['not= "hello" "world"])
              result-names (map :name result)]
          (should-contain "hi!" result-names)
          (should-not-contain "hello" result-names)
          (should-not-contain "world" result-names)))

      (it "not= nil"
        (let [result       (sut/find-by :bibelot :size ['not= nil])
              result-names (map :name result)]
          (should-not-contain "hello" result-names)
          (should-contain "world" result-names)
          (should-contain "hi!" result-names)))

      (it "not= many with nil"
        (should= [] (sut/find-by :bibelot :size ['not= nil 2])))

      (it "or"
        (let [b1 (sut/tx :kind :bibelot :name "Bee" :color "red" :size 1)
              b2 (sut/tx :kind :bibelot :name "Bee" :color "blue" :size 2)
              b3 (sut/tx :kind :bibelot :name "Ant" :color "blue" :size 1)
              b4 (sut/tx :kind :bibelot :color "blue" :size 1)]
          (should= [b1 b2 b3] (sut/find-by :bibelot :name ["Bee" "Ant"]))
          (should= [b1 b2 b3] (sut/find-by :bibelot :name #{"Bee" "Ant"}))
          (should= [b3] (sut/find-by :bibelot :name ["BLAH" "Ant"]))
          (should= [b4] (sut/find-by :bibelot :name [nil]))
          (should= [b3 b4] (sut/find-by :bibelot :name ["BLAH" nil "Ant"]))
          (should= [b2] (sut/find-by :bibelot :name ["Bee" nil] :size 2))
          (should= [b3 b4] (sut/find-by :bibelot :name ["Ant" nil]))
          (should= [] (sut/find-by :bibelot :name ["BLAH" "BLAH" "BLAH"]))
          (should= [] (sut/find-by :bibelot :name ["BLAH" "ARG"]))))

      (it "< > string"
        (let [result       (sut/find-by :bibelot :name ['> "g"] :name ['< "i"])
              result-names (map :name result)]
          (should-contain "hi!" result-names)
          (should-not-contain "world" result-names)))

      (it "<= >= string"
        (sut/clear)
        (let [b1 (sut/tx :kind :bibelot :name "aaa")
              b2 (sut/tx :kind :bibelot :name "bbb")
              b3 (sut/tx :kind :bibelot :name "ccc")]
          (should= [b1 b2 b3] (sut/find-by :bibelot :name ['>= "aaa"]))
          (should= [b2 b3] (sut/find-by :bibelot :name ['>= "bbb"]))
          (should= [b1 b2 b3] (sut/find-by :bibelot :name ['<= "ccc"]))
          (should= [b1 b2] (sut/find-by :bibelot :name ['<= "bbb"]))))

      (it "< <= > >= long"
        (sut/clear)
        (let [b1 (sut/tx :kind :bibelot :size 1)
              b2 (sut/tx :kind :bibelot :size 2)
              b3 (sut/tx :kind :bibelot :size 3)]
          (should= [b1 b2 b3] (sut/find-by :bibelot :size ['> 0]))
          (should= [b1 b2 b3] (sut/find-by :bibelot :size ['>= 1]))
          (should= [b2 b3] (sut/find-by :bibelot :size ['> 1]))
          (should= [b3] (sut/find-by :bibelot :size ['> 2]))
          (should= [] (sut/find-by :bibelot :size ['> 3]))
          (should= [b1 b2 b3] (sut/find-by :bibelot :size ['< 4]))
          (should= [b1 b2 b3] (sut/find-by :bibelot :size ['<= 3]))
          (should= [b1 b2] (sut/find-by :bibelot :size ['< 3]))
          (should= [b1] (sut/find-by :bibelot :size ['< 2]))
          (should= [] (sut/find-by :bibelot :size ['< 1]))))

      (it "< <= > >= date"
        (let [thing1 (sut/tx :kind :thingy :id 123 :bang (-> 1 minutes ago))
              thing2 (sut/tx :kind :thingy :id 456 :bang (-> 3 minutes ago))
              thing3 (sut/tx :kind :thingy :id 789 :bang (-> 5 minutes ago))]
          (should= [thing2 thing3] (sut/find-by :thingy :bang ['< (-> 2 minutes ago)]))
          (should= [thing2 thing3] (sut/find-by :thingy :bang ['<= (:bang thing2)]))
          (should= [thing1] (sut/find-by :thingy :bang ['> (-> 2 minutes ago)]))
          (should= [thing1 thing2] (sut/find-by :thingy :bang ['>= (:bang thing2)]))
          (should= [thing2] (sut/find-by :thingy :bang ['< (-> 2 minutes ago)] :bang ['> (-> 4 minutes ago)]))
          )
        )


      (it "compare against entity with null value"
        (sut/clear)
        (let [b1  (sut/tx :kind :bibelot :size 1)
              _b2 (sut/tx :kind :bibelot :size nil)]
          (should= [b1] (sut/find-by :bibelot :size ['>= 0]))))

      (it "like fuzzy match with anything before or after"
        (let [result       (sut/find-by :bibelot :name ['like "%orl%"])
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

(defn reduce-by [db-impl]
  (context "reduce-by"
    (helper/with-schemas db-impl [bibelot thingy])
    (before (sut/clear)
            (sut/tx {:kind :bibelot :name "hello"})
            (sut/tx {:kind :bibelot :name "world"})
            (sut/tx {:kind :bibelot :name "world" :size 2})
            (sut/tx {:kind :bibelot :name "hi!" :size 2}))

    (it "sum of none"
      (should= 0 (sut/reduce-by :bibelot #(+ %1 (:size %2)) 0 :name "fake")))
    (it "sum of one"
      (should= 2 (sut/reduce-by :bibelot #(+ %1 (:size %2)) 0 :name "hi!")))
    (it "sum of all"
      (should= 4 (sut/reduce-by :bibelot #(+ %1 (or (:size %2) 0)) 0)))
    (it "reduced map"
      (should= {"world" [2] "hi!" [2]} (sut/reduce-by :bibelot #(update %1 (:name %2) conj (:size %2)) {} :size ['not= nil])))))

(defn count-by [db-impl]
  (context "count-by"
    (helper/with-schemas db-impl [bibelot thingy])

    (it "empty db"
      (should= 0 (sut/count-by :bibelot :name "nothing")))

    (context "populated db"
      (before (sut/clear)
              (sut/tx {:kind :bibelot :name "hello"})
              (sut/tx {:kind :bibelot :name "world"})
              (sut/tx {:kind :bibelot :name "world" :size 2})
              (sut/tx {:kind :bibelot :name "hi!" :size 2})
              )

      (it "count-by: :name"
        (should= 1 (sut/count-by :bibelot :name "hello")))

      (it "count-by: two attributes"
        (should= 1 (sut/count-by :bibelot :name "world" :size 2)))

      (it "count-by: returns all found"
        (should= 2 (sut/count-by :bibelot :name "world")))

      (it "count-by: all by size"
        (should= 2 (sut/count-by :bibelot :size 2)))

      (it "="
        (should= 1 (sut/count-by :bibelot :name ['= "hi!"])))

      (it "= many"
        (should= 3 (sut/count-by :bibelot :name ['= "hello" "world"])))

      (it "not="
        (should= 3 (sut/count-by :bibelot :name ['not= "hi!"])))

      (it "not= many"
        (should= 2 (sut/count-by :bibelot :name ['not= "hi!" "hello"])))

      (it "or"
        (sut/tx :kind :bibelot :name "Bee" :color "red" :size 1)
        (sut/tx :kind :bibelot :name "Bee" :color "blue" :size 2)
        (sut/tx :kind :bibelot :name "Ant" :color "blue" :size 1)
        (should= 3 (sut/count-by :bibelot :name ["Bee" "Ant"]))
        (should= 1 (sut/count-by :bibelot :name ["BLAH" "Ant"]))
        (should= 0 (sut/count-by :bibelot :name ["BLAH" "ARG"])))

      (it "< > string"
        (should= 2 (sut/count-by :bibelot :name ['> "g"] :name ['< "i"])))

      (it "<= >= string"
        (sut/clear)
        (sut/tx :kind :bibelot :name "aaa")
        (sut/tx :kind :bibelot :name "bbb")
        (sut/tx :kind :bibelot :name "ccc")
        (should= 3 (sut/count-by :bibelot :name ['>= "aaa"]))
        (should= 2 (sut/count-by :bibelot :name ['>= "bbb"]))
        (should= 3 (sut/count-by :bibelot :name ['<= "ccc"]))
        (should= 2 (sut/count-by :bibelot :name ['<= "bbb"])))

      (it "< <= > >= long"
        (sut/clear)
        (sut/tx :kind :bibelot :size 1)
        (sut/tx :kind :bibelot :size 2)
        (sut/tx :kind :bibelot :size 3)
        (should= 3 (sut/count-by :bibelot :size ['> 0]))
        (should= 3 (sut/count-by :bibelot :size ['>= 1]))
        (should= 2 (sut/count-by :bibelot :size ['> 1]))
        (should= 1 (sut/count-by :bibelot :size ['> 2]))
        (should= 0 (sut/count-by :bibelot :size ['> 3]))
        (should= 3 (sut/count-by :bibelot :size ['< 4]))
        (should= 3 (sut/count-by :bibelot :size ['<= 3]))
        (should= 2 (sut/count-by :bibelot :size ['< 3]))
        (should= 1 (sut/count-by :bibelot :size ['< 2]))
        (should= 0 (sut/count-by :bibelot :size ['< 1])))

      (it "< <= > >= date"
        (let [_thing1 (sut/tx :kind :thingy :id 123 :bang (-> 1 minutes ago))
              thing2  (sut/tx :kind :thingy :id 456 :bang (-> 3 minutes ago))
              _thing3 (sut/tx :kind :thingy :id 789 :bang (-> 5 minutes ago))]
          (should= 2 (sut/count-by :thingy :bang ['< (-> 2 minutes ago)]))
          (should= 2 (sut/count-by :thingy :bang ['<= (:bang thing2)]))
          (should= 1 (sut/count-by :thingy :bang ['> (-> 2 minutes ago)]))
          (should= 2 (sut/count-by :thingy :bang ['>= (:bang thing2)]))
          (should= 1 (sut/count-by :thingy :bang ['< (-> 2 minutes ago)] :bang ['> (-> 4 minutes ago)]))))

      (it "compare against entity with null value"
        (sut/clear)
        (sut/tx :kind :bibelot :size 1)
        (sut/tx :kind :bibelot :size nil)
        (should= 1 (sut/count-by :bibelot :size ['>= 0])))

      (it "like fuzzy match with anything before or after"
        (should= 2 (sut/count-by :bibelot :name ['like "%orl%"])))

      (it "like fuzzy match with anything after"
        (sut/tx {:kind :bibelot :name "hello world"})
        (should= 2 (sut/count-by :bibelot :name ['like "worl%"])))

      (it "like fuzzy match with _"
        (sut/tx {:kind :bibelot :name "words"})
        (should= 3 (sut/count-by :bibelot :name ['like "wor__"])))

      (it "like with exact match"
        (sut/tx {:kind :bibelot :name "words"})
        (should= 2 (sut/count-by :bibelot :name ['like "world"])))

      (it "ILIKE with exact match"
        (sut/tx {:kind :bibelot :name "words"})
        (should= 2 (sut/count-by :bibelot :name ['ilike "WORLD"]))))))

(defn find-all [db-impl]
  (context "find-all"
    (helper/with-schemas db-impl [bibelot thingy])
    (before (sut/clear))

    (it "has none"
      (should= [] (sut/find-all :thingy)))

    (it "finds all entities of a kind"
      (sut/tx* [{:kind :bibelot :name "hello"}
                {:kind :bibelot :name "world"}
                {:kind :bibelot :name "hola"}
                {:kind :thingy :id 123 :foo "mundo"}])
      (should= 3 (count (sut/find-all :bibelot)))
      (should= 1 (count (sut/find-all :thingy))))
    )
  )

(defn nil-value-specs [db-impl]

  (context "retracting null values"

    (helper/with-schemas db-impl [bibelot thingy])

    (with child (sut/tx {:kind :bibelot :name "child" :color "golden"}))
    (with original (sut/tx {:kind :thingy
                            :id   123
                            :foo  "foo"
                            :bar  (:id @child)
                            :fizz 1234
                            :bang (time/now)}))


    (it "can set a string to nil"
      (let [result (sut/tx (assoc @original :foo nil))]
        (should= nil (:foo result))
        (should= nil (:foo (sut/reload @original)))))

    (it "can set a uuid to nil"
      (let [result (sut/tx (assoc @original :bar nil))]
        (should= nil (:bar result))
        (should= nil (:bar (sut/reload @original)))))

    (it "can set a int to nil"
      (let [result (sut/tx (assoc @original :fizz nil))]
        (should= nil (:fizz result))
        (should= nil (:fizz (sut/reload @original)))))

    (it "can set a instant to nil"
      (let [result (sut/tx (assoc @original :bang nil))]
        (should= nil (:bang result))
        (should= nil (:bang (sut/reload @original)))))
    )
  )

(defn count-all [db-impl]
  (context "counts rows"
    (helper/with-schemas db-impl [bibelot thingy])
    (before (sut/clear))

    (it "0 of a kind"
      (should= 0 (sut/count-all :bibelot)))

    (it "3 of a kind"
      (sut/tx* [{:kind :bibelot :name "hello"} {:kind :bibelot :name "hola"} {:kind :bibelot :name "shalom"}])
      (should= 3 (sut/count-all :bibelot)))
    )
  )




