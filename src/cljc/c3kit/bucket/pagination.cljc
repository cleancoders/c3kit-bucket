(ns c3kit.bucket.pagination
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.schema :as s])
  #?(:clj (:import (java.util.regex PatternSyntaxException))))

(def default-page-size 20)
(def default-page 1)

(defn ->int [v] (when v (s/->int v)))

(defn present-entities [raw page page-size present-fn]
  (let [valid-entities (take page-size (drop (* (dec page) page-size) raw))]
    (if-not present-fn valid-entities (map present-fn valid-entities))))

(defn paginate [request search-fn options]
  (let [{:keys [present-fn]} options
        q         (-> request :params :q)
        page-size (or (-> request :params :page-size ->int) default-page-size)
        page      (or (-> request :params :page ->int) default-page)
        raw       (try (search-fn {:q q :page page :page-size page-size}) (catch #?(:clj PatternSyntaxException :cljs js/Error) _ []))
        raw       (if (map? (first raw)) raw (map first (ccc/rsort-by second raw)))
        entities  (present-entities raw page page-size present-fn)]
    {:n         (count raw)
     :q         q
     :page      page
     :page-size page-size
     :entities  entities}))