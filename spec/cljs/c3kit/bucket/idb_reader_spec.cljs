(ns c3kit.bucket.idb-reader-spec
  (:require-macros [speclj.core :refer [describe context it should= should-contain]])
  (:require [c3kit.bucket.idb-io :as io]
            [c3kit.bucket.idb-reader :as sut]
            [speclj.core]))

(def legend {:bibelot {:id {:type :long} :name {:type :string} :size {:type :long}}})

(defn- seed-entity-and-dirty! [idb entities dirty-entries]
  (let [tx    (.transaction idb #js ["bibelot" "_meta"] "readwrite")
        store (.objectStore tx "bibelot")
        meta  (.objectStore tx "_meta")]
    (doseq [entity entities]
      (.put store (io/clj->js-entity entity)))
    (.put meta (io/clj->js-entity {:id "dirty" :data dirty-entries}))
    (js/Promise.
     (fn [resolve _]
       (set! (.-oncomplete tx) #(resolve idb))))))

(describe "IDB Reader"

  (context "dirty-entities"

    (it "reads dirty entities from IDB"
      (-> (io/open "test-reader-1" legend)
          (.then (fn [idb]
                   (seed-entity-and-dirty! idb [{:id -1 :kind :bibelot :name "offline" :size 5}] {-1 :bibelot})))
          (.then (fn [idb]
                   (-> (sut/dirty-entities idb)
                       (.then (fn [result]
                                (should= 1 (count result))
                                (should= -1 (:id (first result)))
                                (should= "offline" (:name (first result)))
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "test-reader-1"))))))))

    (it "returns empty vector when nothing dirty"
      (-> (io/open "test-reader-2" legend)
          (.then (fn [idb]
                   (-> (sut/dirty-entities idb)
                       (.then (fn [result]
                                (should= [] result)
                                (io/close idb)
                                (.deleteDatabase js/indexedDB "test-reader-2")))))))))

  (context "clear-dirty!"

    (it "removes dirty IDs and their entities from IDB"
      (-> (io/open "test-reader-3" legend)
          (.then (fn [idb]
                   (seed-entity-and-dirty! idb
                                           [{:id -1 :kind :bibelot :name "first" :size 1}
                                            {:id -2 :kind :bibelot :name "second" :size 2}]
                                           {-1 :bibelot -2 :bibelot})))
          (.then (fn [idb]
                   (-> (sut/clear-dirty! idb #{-1})
                       (.then (fn [_]
                                ;; Check dirty set is now {-2 :bibelot}
                                (let [tx      (.transaction idb #js ["_meta"] "readonly")
                                      store   (.objectStore tx "_meta")
                                      request (.get store "dirty")]
                                  (js/Promise.
                                   (fn [resolve _]
                                     (set! (.-onsuccess request)
                                           (fn [event]
                                             (let [result (io/js->clj-entity (.-result (.-target event)))]
                                               (should= {-2 :bibelot} (:data result))
                                               (resolve idb)))))))))
                       (.then (fn [idb]
                                ;; Check entity -1 is gone
                                (let [tx      (.transaction idb #js ["bibelot"] "readonly")
                                      store   (.objectStore tx "bibelot")
                                      request (.get store -1)]
                                  (js/Promise.
                                   (fn [resolve _]
                                     (set! (.-onsuccess request)
                                           (fn [event]
                                             (should= nil (.-result (.-target event)))
                                             (resolve idb))))))))
                       (.then (fn [idb]
                                ;; Check entity -2 remains
                                (let [tx      (.transaction idb #js ["bibelot"] "readonly")
                                      store   (.objectStore tx "bibelot")
                                      request (.get store -2)]
                                  (js/Promise.
                                   (fn [resolve _]
                                     (set! (.-onsuccess request)
                                           (fn [event]
                                             (let [entity (io/js->clj-entity (.-result (.-target event)))]
                                               (should= "second" (:name entity))
                                               (io/close idb)
                                               (resolve (.deleteDatabase js/indexedDB "test-reader-3")))))))))))))))
  ))
