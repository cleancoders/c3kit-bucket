(ns c3kit.bucket.migration_samples.20230101
  "20230101 sample migration"
  (:require [c3kit.apron.log :as log]))

(defn up [] (log/info "20230101 UP"))
(defn down [] (log/info "20230101 DOWN"))
