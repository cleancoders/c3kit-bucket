(ns c3kit.bucket.idb-integration-test
  (:require [cljs.test :refer-macros [deftest async is testing]]
            [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as idb]
            [c3kit.bucket.idb-io :as io]
            [c3kit.bucket.indexeddb]
            [c3kit.apron.schema :as s]))

;region Test Reporter

(def ^:private green "\u001b[32m")
(def ^:private red "\u001b[31m")
(def ^:private reset-color "\u001b[0m")
(def ^:private counters-before (atom {:fail 0 :error 0}))
(def ^:private failure-messages (atom []))

(defn- report-counters [env]
  (let [rc (:report-counters env)]
    {:fail (:fail rc 0) :error (:error rc 0)}))

(defmethod cljs.test/report [:cljs.test/default :begin-test-var] [m]
  (reset! counters-before (report-counters (cljs.test/get-current-env))))

(defmethod cljs.test/report [:cljs.test/default :end-test-var] [m]
  (let [after     (report-counters (cljs.test/get-current-env))
        failed?   (or (> (:fail after) (:fail @counters-before))
                      (> (:error after) (:error @counters-before)))
        test-name (-> m :var meta :name str)
        color     (if failed? red green)]
    (println (str color test-name reset-color))))

(defmethod cljs.test/report [:cljs.test/default :fail] [m]
  (cljs.test/inc-report-counter! :fail)
  (swap! failure-messages conj
    (str "\nFAIL in " (cljs.test/testing-vars-str m)
         (when-let [msg (:message m)] (str "\n" msg))
         "\nexpected: " (pr-str (:expected m))
         "\n  actual: " (pr-str (:actual m)))))

(defmethod cljs.test/report [:cljs.test/default :error] [m]
  (cljs.test/inc-report-counter! :error)
  (swap! failure-messages conj
    (str "\nERROR in " (cljs.test/testing-vars-str m)
         (when-let [msg (:message m)] (str "\n" msg))
         "\nexpected: " (pr-str (:expected m))
         "\n  actual: " (pr-str (:actual m)))))

(defmethod cljs.test/report [:cljs.test/default :summary] [m]
  (doseq [msg @failure-messages] (println msg))
  (println "\nRan" (:test m) "tests containing"
           (+ (:pass m) (:fail m) (:error m)) "assertions.")
  (println (:fail m) "failures," (:error m) "errors."))

;endregion

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

(deftest dirty-set-round-trip
  (async done
    (let [legend {:bibelot {:id {:type :long} :name {:type :string}}}]
      (reset! idb/dirty-chain (js/Promise.resolve nil))
      (-> (io/open "integration-dirty-1" legend)
          (.then (fn [idb]
                   (-> (idb/add-to-dirty-set! idb {1 :bibelot 2 :bibelot 3 :bibelot})
                       (.then (fn [_] (idb/read-dirty-set idb)))
                       (.then (fn [result]
                                (is (= {1 :bibelot 2 :bibelot 3 :bibelot} result))
                                (idb/add-to-dirty-set! idb {4 :bibelot})))
                       (.then (fn [_] (idb/read-dirty-set idb)))
                       (.then (fn [result]
                                (is (= {1 :bibelot 2 :bibelot 3 :bibelot 4 :bibelot} result))
                                (idb/remove-from-dirty-set! idb #{2 4})))
                       (.then (fn [_] (idb/read-dirty-set idb)))
                       (.then (fn [result]
                                (is (= {1 :bibelot 3 :bibelot} result))
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "integration-dirty-1"))))))
          (.then (fn [_] (done)))
          (.catch (fn [e] (is (nil? e) (str "Unexpected: " e)) (done)))))))

(deftest offline-tx-and-sync-lifecycle
  (async done
    (let [db       (api/create-db {:impl :indexeddb :db-name "integration-sync-1" :online? (constantly false)} [bibelot])
          received (atom nil)]
      (reset! idb/offline-id-counter 0)
      (reset! idb/dirty-chain (js/Promise.resolve nil))
      (-> (idb/init! db)
          (.then (fn [db]
                   (api/-tx db {:kind :bibelot :name "w1" :size 1})
                   (api/-tx db {:kind :bibelot :name "w2" :size 2})
                   (idb/sync! db (fn [entities] (reset! received entities)))))
          (.then (fn [_]
                   (is (= 2 (count @received)))
                   (is (= #{-1 -2} (into #{} (map :id) @received)))
                   (idb/sync-complete! db #{-1 -2} [{:kind :bibelot :id 9001 :name "w1" :size 1}
                                                     {:kind :bibelot :id 9002 :name "w2" :size 2}])))
          (.then (fn [_]
                   (is (= 0 (count (api/find-by- db :bibelot :id -1))))
                   (is (= 0 (count (api/find-by- db :bibelot :id -2))))
                   (is (= "w1" (:name (api/entity- db :bibelot 9001))))
                   (is (= "w2" (:name (api/entity- db :bibelot 9002))))
                   (idb/read-dirty-set @(.-idb-atom db))))
          (.then (fn [dirty]
                   (is (= {} dirty))
                   (api/close db)
                   (.deleteDatabase js/indexedDB "integration-sync-1")))
          (.then (fn [_] (done)))
          (.catch (fn [e] (is (nil? e) (str "Unexpected: " e)) (done)))))))

(deftest refresh-purges-and-replaces
  (async done
    (let [db (api/create-db {:impl :indexeddb :db-name "integration-refresh-1"} [bibelot])]
      (reset! idb/offline-id-counter 0)
      (-> (idb/init! db)
          (.then (fn [db]
                   (api/-tx db {:kind :bibelot :id -1 :name "offline-widget" :size 5})
                   (api/-tx db {:kind :bibelot :id 100 :name "server-widget" :size 10})
                   (idb/refresh! db [{:kind :bibelot :id 200 :name "fresh-widget" :size 20}])))
          (.then (fn [_]
                   (is (= 0 (count (filter #(neg? (:id %)) (api/find-by- db :bibelot :name "offline-widget")))))
                   (is (= "server-widget" (:name (api/entity- db :bibelot 100))))
                   (is (= "fresh-widget" (:name (api/entity- db :bibelot 200))))
                   ;; Rehydrate to verify IDB state matches memory
                   (reset! (.-store db) {:all {}})
                   (idb/rehydrate! db)))
          (.then (fn [db]
                   (is (= 0 (count (filter #(neg? (:id %)) (api/find-by- db :bibelot :name "offline-widget")))))
                   (is (some? (api/entity- db :bibelot 100)))
                   (is (some? (api/entity- db :bibelot 200)))
                   (api/close db)
                   (.deleteDatabase js/indexedDB "integration-refresh-1")))
          (.then (fn [_] (done)))
          (.catch (fn [e] (is (nil? e) (str "Unexpected: " e)) (done)))))))

(deftest rollback-on-idb-failure
  (async done
    (let [db           (api/create-db {:impl :indexeddb :db-name "integration-rollback-1"} [bibelot])
          _            (api/-tx db {:kind :bibelot :name "existing" :size 1})
          store-before @(.-store db)]
      ;; Point idb-atom to a broken mock to force IDB write failure
      (reset! (.-idb-atom db) #js {:transaction (fn [] (throw (js/Error. "closed")))})
      (api/-tx db {:kind :bibelot :name "should-rollback" :size 99})
      ;; Entity appears optimistically in memory
      (is (= 1 (count (api/find-by- db :bibelot :name "should-rollback"))))
      ;; After the promise rejects, store should roll back
      (js/setTimeout
        (fn []
          (is (= store-before @(.-store db)))
          (is (= 0 (count (api/find-by- db :bibelot :name "should-rollback"))))
          (done))
        100))))

(defn ^:export run []
  (cljs.test/run-tests 'c3kit.bucket.idb-integration-test))
