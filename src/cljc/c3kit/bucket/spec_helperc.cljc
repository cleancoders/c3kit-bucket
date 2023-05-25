(ns c3kit.bucket.spec-helperc
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [around around-all before]]
            [c3kit.apron.log :as log]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.memory]))

(log/warn!)

(defn with-schemas
  ([schemas] (with-schemas {:impl :memory} schemas))
  ([config schemas]
   (list
     (around-all [it]
       (api/with-safety-off
         (with-redefs [api/impl (delay (api/create-db config schemas))]
           (it))))
     (before (api/clear)))))

