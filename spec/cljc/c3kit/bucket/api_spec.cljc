(ns c3kit.bucket.api-spec
  (:require [c3kit.bucket.api :as api]
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
            [c3kit.apron.time :as time :refer [ago minutes]]
            #?(:clj [c3kit.apron.util :as util])))

(describe "API"

  (context "safety"

    (it "is on by default"
      (should= true sut/*safety*))

    (it "with-safety-off"
      (with-safety-off
        (should= false sut/*safety*)))

    (it "setters"
      (sut/set-safety! false)
      (should= false sut/*safety*)
      (sut/set-safety! true)
      (should= true sut/*safety*))

    )

  (context "cas"

    (it "with id"
      (let [entity {:kind :widget :id 123}
            result (sut/cas {:foo "bar"} entity)]
        (should= {:foo "bar"} (:cas (meta result)))))

    (it "without id"
      (should-throw #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core.ExceptionInfo)
        "cas may not be applied to new entities."
        (sut/cas {:foo "bar"} {:kind :widget})))

    )

  #?(:clj
     (context "clj"

       (with-stubs)

       (context "config"

         (it "loading"
           (with-redefs [util/read-edn-resource (stub :read-edn {:return {:foo "bar"}})]
             (let [result (sut/load-config)]
               (should= {:foo "bar"} result)
               (should-have-invoked :read-edn {:with ["config/bucket.edn"]}))))


         (it "loading from alternative resource"
           (with-redefs [util/read-edn-resource (stub :read-edn {:return {:foo "bar"}})]
             (let [result (sut/load-config "foo/bar/config.edn")]
               (should= {:foo "bar"} result)
               (should-have-invoked :read-edn {:with ["foo/bar/config.edn"]}))))

         (it "config-var"
           (let [config {:foo "bar" :config-var 'foo.bar}]
             (with-redefs [util/read-edn-resource (stub :read-edn {:return config})
                           util/var-value         (stub :var-value {:return {:fizz "bang"}})]
               (let [result (sut/load-config)]
                 (should= {:foo "bar" :fizz "bang"} result)
                 (should-have-invoked :var-value {:with ['foo.bar]})))))

         )

       (context "service"

         (it "start"
           (let [config {:full-schema 'foo/bar :impl :memory}]
             (with-redefs [sut/load-config (constantly config)
                           util/var-value  (constantly [])]
               (let [app (sut/-start-service {})]
                 (should-contain :bucket/impl app)
                 (should= config (:bucket/config app))
                 (should= [] (:bucket/schemas app))))))

         (it "start - missing :full-schema"
           (with-redefs [sut/load-config (constantly {:impl :memory})]
             (should-throw (sut/-start-service {}))))

         (it "stop"
           (with-redefs [api/close (stub :close)]
             (let [app (sut/-stop-service {:bucket/impl    :blah
                                           :bucket/config  :blah
                                           :bucket/schemas :blah})]
               (should-not-contain :bucket/impl app)
               (should-not-contain :bucket/config app)
               (should-not-contain :bucket/schemas app))
             (should-have-invoked :close)))

         )
       )
     )
  )


