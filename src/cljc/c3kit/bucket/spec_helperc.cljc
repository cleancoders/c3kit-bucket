(ns c3kit.bucket.spec-helperc
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [around-all before]]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.memory]))

(defmacro with-impl [config schemas & body]
  `(with-redefs [api/*safety* false
                 ;; TODO - MDM: api/set-impl! fails with the use of delay.  Use atom?
                 api/impl     (delay (api/create-db ~config ~schemas))]
     (api/clear)
     (try ~@body (finally (api/close @api/impl)))))

(defn with-schemas
  ([schemas] (with-schemas {:impl :memory} schemas))
  ([config schemas]
   (list
     (around-all [it]
       (with-redefs [api/*safety* false
                     ;; TODO - MDM: api/set-impl! fails with the use of delay.  Use atom?
                     api/impl     (delay (api/create-db config schemas))]
         (try
           (it)
           (finally
             (api/close @api/impl)))))
     (before (api/clear)))))
