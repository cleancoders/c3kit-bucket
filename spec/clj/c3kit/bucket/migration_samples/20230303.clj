(ns c3kit.bucket.migration_samples.20230303
  "20230303 sample migration"
  (:require [c3kit.apron.log :as log]))

(defn up [] (log/info "20230303 UP"))
(defn down [] (log/info "20230303 DOWN"))
