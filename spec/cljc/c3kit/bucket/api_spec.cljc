(ns c3kit.bucket.api-spec
  (:require [c3kit.bucket.api :as api]
            [speclj.core #?(:clj :refer :cljs :refer-macros) [context describe it should-contain should-have-invoked
                                                              should-not-contain should-throw should= stub with-stubs]]
            [c3kit.bucket.api :as sut #?(:clj :refer :cljs :refer-macros) [with-safety-off]]
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


