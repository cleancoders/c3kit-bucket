(ns c3kit.bucket.spec-helperc
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [around around-all before]]
            [c3kit.apron.log :as log]
            [c3kit.bucket.api :as db]
            [c3kit.bucket.memory]))

(log/warn!)

(defn with-schemas
  ([schemas] (with-schemas (db/create-db {:impl :memory} schemas)))
  ([config schemas]
   (list
     (around-all [it]
       (with-redefs [db/impl (delay (db/create-db config schemas))]
         (it)))
     (before (db/clear)))))

