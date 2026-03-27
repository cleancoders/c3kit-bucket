(ns c3kit.bucket.idb-integration-test
  (:require [cljs.test :refer-macros [deftest async is testing]]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as idb]
            [c3kit.bucket.idb-io :as io]
            [c3kit.bucket.indexeddb]
            [c3kit.apron.schema :as s]))

(def bibelot
  {:kind  (s/kind :bibelot)
   :id    {:type :long}
   :name  {:type :string}
   :size  {:type :long}
   :color {:type :string}})

(def thingy
  {:kind (s/kind :thingy)
   :id   {:type :int}
   :name {:type :string}
   :foo  {:type :string}})

(deftest persistence-round-trip
  (async done
    (let [db (api/create-db {:impl :indexeddb :db-name "integration-persist-1"} [bibelot])]
      (-> (idb/init! db)
          (.then (fn [db] (api/-tx db {:kind :bibelot :name "widget" :size 5})))
          (.then (fn [saved]
                   (is (= "widget" (:name saved)))
                   (is (some? (:id saved)))
                   (reset! (.-store db) {:all {}})
                   (idb/rehydrate! db)))
          (.then (fn [db]
                   (let [found (api/find-by- db :bibelot :name "widget")]
                     (is (= 1 (count found)))
                     (is (= "widget" (:name (first found))))
                     (is (= 5 (:size (first found)))))
                   (api/close db)
                   (.deleteDatabase js/indexedDB "integration-persist-1")))
          (.then (fn [_] (done)))
          (.catch (fn [e] (is (nil? e) (str "Unexpected: " e)) (done)))))))

(defn ^:export run []
  (cljs.test/run-tests 'c3kit.bucket.idb-integration-test))
