(ns c3kit.bucket.memory-history-spec
  (:require [c3kit.bucket.api :as api #?(:clj :refer :cljs :refer-macros) [with-safety-off]]
            [c3kit.bucket.history :as history]
            [c3kit.bucket.impl-spec :as spec]
            [speclj.core #?(:clj :refer :cljs :refer-macros) [around context describe it should=]]
            #?(:clj [c3kit.bucket.memory-history]
               :cljs [c3kit.bucket.memory-history])))

(def config {:impl :memory-history :storage {:impl :memory}})
(def plain-memory-config {:impl :memory})

(describe "Memory History"

  (around [it] (with-safety-off (it)))

  (spec/history-specs config)
  (spec/excise-specs config)

  (context "decorator specifics"

    (it "plain memory does not support history"
      (let [db (api/create-db plain-memory-config [spec/bibelot])]
        (should= false (history/supported? db))))

    (it "history starts at wrap time"
      (let [store (atom {})
            inner (api/create-db {:impl :memory :store store} [spec/bibelot])
            pre   (api/tx- inner {:kind :bibelot :name "Pre" :size 1 :color "gray"})
            db    (api/create-db {:impl :memory-history :storage {:impl :memory :store store}} [spec/bibelot])]
        (should= [] (history/history- db pre))
        (let [post (api/tx- db (assoc pre :size 2))]
          (should= 1 (count (history/history- db post))))))

    (it "clear resets the log"
      (let [db (api/create-db config [spec/bibelot])
            e  (api/tx- db {:kind :bibelot :name "ClearMe" :size 1 :color "red"})]
        (api/clear- db)
        (should= [] (history/history- db e))))

    (it "delete-all records deletion markers"
      (let [db (api/create-db config [spec/bibelot])
            a  (api/tx- db {:kind :bibelot :name "A" :size 1 :color "red"})
            b  (api/tx- db {:kind :bibelot :name "B" :size 1 :color "blue"})]
        (api/delete-all- db :bibelot)
        (should= true (:db/deleted? (last (history/history- db a))))
        (should= true (:db/deleted? (last (history/history- db b))))))))
