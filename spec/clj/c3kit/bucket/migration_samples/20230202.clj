(ns c3kit.bucket.migration_samples.20230202
  "20230202 sample migration"
  (:require [c3kit.apron.log :as log]))

(defn up [] (log/info "20230202 UP"))
(defn down [] (log/info "20230202 DOWN"))
