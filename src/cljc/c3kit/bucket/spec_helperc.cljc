(ns c3kit.bucket.spec-helperc
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [around]]
            [c3kit.apron.log :as log]
            [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as db]
            [c3kit.bucket.memory :as memory]))

(log/warn!)

(defn with-schemas
  ([schemas] (with-schemas (memory/create-db) schemas))
  ([impl schemas]
   (around [it]
     (with-redefs [db/impl (delay impl)]
       (db/install-schema schemas)
       (db/clear)
       (it)))))

