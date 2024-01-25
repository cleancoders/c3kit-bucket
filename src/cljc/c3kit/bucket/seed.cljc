(ns c3kit.bucket.seed
  (:require [c3kit.bucket.api :as db]))


(defn attr-matches? [fields entity attr]
  (= (get fields attr)
     (get entity attr)))

(defn attrs-match? [fields entity]
  (every? (partial attr-matches? fields entity) (keys fields)))

(deftype Entity [atm kind search-fields other-fields]
  #?(:clj clojure.lang.IDeref :cljs cljs.core/IDeref)
  (#?(:clj deref :cljs -deref) [_]
    (if-let [e (db/reload @atm)]
      e
      (if-let [e (apply db/ffind-by kind (flatten (seq search-fields)))]
        (if (attrs-match? other-fields e)
          (do
            (println "EXISTS:   " (pr-str kind search-fields))
            (reset! atm e))
          (do
            (println "UPDATING: " (pr-str kind search-fields))
            (reset! atm (db/tx (merge e other-fields)))))
        (let [entity (merge {:kind kind} search-fields other-fields)]
          (println "CREATING: " (pr-str kind search-fields))
          (reset! atm (db/tx entity)))))))

(defn entity
  "Creates a defer-able entity that will, when deref-ed, ensure the entity is in the database and returns the
  currently stored state of the entity.
  search-fields is the minimal set of attributes that identify the entity.
  other-fields is all the rest of the attributes that will be saved in this entity."
  ([kind search-fields] (entity kind search-fields {}))
  ([kind search-fields other-fields] (Entity. (atom nil) kind search-fields other-fields)))
