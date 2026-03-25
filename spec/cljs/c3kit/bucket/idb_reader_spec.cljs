(ns c3kit.bucket.idb-reader-spec
  (:require-macros [speclj.core :refer [describe context it should=]])
  (:require [c3kit.bucket.idb-common :as common]
            [c3kit.bucket.idb-reader :as sut]
            [speclj.core]))

(def legend {:bibelot {:id {:type :long} :name {:type :string} :size {:type :long}}})

(defn- seed-entity-and-dirty! [idb entities dirty-ids]
  (let [tx    (.transaction idb #js ["bibelot" "_meta"] "readwrite")
        store (.objectStore tx "bibelot")
        meta  (.objectStore tx "_meta")]
    (doseq [entity entities]
      (.put store (common/clj->js-entity entity)))
    (.put meta (common/clj->js-entity {:id "dirty" :data (set dirty-ids)}))
    (js/Promise.
      (fn [resolve _]
        (set! (.-oncomplete tx) #(resolve idb))))))

(defn- read-idb-dirty-set [idb]
  (js/Promise.
    (fn [resolve _]
      (let [tx      (.transaction idb #js ["_meta"] "readonly")
            store   (.objectStore tx "_meta")
            request (.get store "dirty")]
        (set! (.-onsuccess request)
              (fn [event]
                (let [result (.-result (.-target event))]
                  (resolve (if result (:data (common/js->clj-entity result)) #{})))))))))

(defn- read-idb-entity [idb store-name id]
  (js/Promise.
    (fn [resolve _]
      (let [tx      (.transaction idb #js [store-name] "readonly")
            store   (.objectStore tx store-name)
            request (.get store id)]
        (set! (.-onsuccess request)
              (fn [event]
                (resolve (common/js->clj-entity (.-result (.-target event))))))))))

(defn- cleanup! [idb db-name]
  (common/close idb)
  (.deleteDatabase js/indexedDB db-name))

(describe "IDB Reader"

  (context "dirty-entities"

    (it "reads dirty entities from IDB"
      (-> (common/open "test-reader-1" legend)
          (.then #(seed-entity-and-dirty! % [{:id -1 :kind :bibelot :name "offline" :size 5}] [-1]))
          (.then (fn [idb]
                   (-> (sut/dirty-entities idb)
                       (.then (fn [result]
                                (should= 1 (count result))
                                (should= -1 (:id (first result)))
                                (should= "offline" (:name (first result)))
                                (cleanup! idb "test-reader-1"))))))))

    (it "returns empty vector when nothing dirty"
      (-> (common/open "test-reader-2" legend)
          (.then (fn [idb]
                   (-> (sut/dirty-entities idb)
                       (.then (fn [result]
                                (should= [] result)
                                (cleanup! idb "test-reader-2")))))))))

  (context "clear-dirty!"

    (it "removes dirty IDs and their entities from IDB"
      (-> (common/open "test-reader-3" legend)
          (.then #(seed-entity-and-dirty! %
                                          [{:id -1 :kind :bibelot :name "first" :size 1}
                                           {:id -2 :kind :bibelot :name "second" :size 2}]
                                          [-1 -2]))
          (.then (fn [idb]
                   (-> (sut/clear-dirty! idb #{-1})
                       (.then (fn [_] (read-idb-dirty-set idb)))
                       (.then (fn [dirty-set]
                                (should= #{-2} dirty-set)
                                (read-idb-entity idb "bibelot" -1)))
                       (.then (fn [entity]
                                (should= nil entity)
                                (read-idb-entity idb "bibelot" -2)))
                       (.then (fn [entity]
                                (should= "second" (:name entity))
                                (cleanup! idb "test-reader-3"))))))))))
