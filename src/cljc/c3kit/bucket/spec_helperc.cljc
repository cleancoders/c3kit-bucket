(ns c3kit.bucket.spec-helperc
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [around]]
            [c3kit.apron.log :as log]
            [c3kit.apron.legend :as legend]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.memory :as memory]))

(log/warn!)

(defn with-schemas
  ([schemas] (with-schemas (memory/create-db) schemas))
  ([impl schemas]
   (let [schemas    (if (sequential? schemas) (flatten schemas) [schemas])
         schema-map (reduce #(assoc %1 (-> %2 :kind :value) %2) {} schemas)]
     (around [it]
       (with-redefs [legend/index schema-map
                     api/impl     (delay impl)]
         (api/clear)
         (it))))))

