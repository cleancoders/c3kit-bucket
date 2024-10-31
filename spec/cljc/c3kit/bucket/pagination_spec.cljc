(ns c3kit.bucket.pagination-spec
  (:require
   [c3kit.bucket.pagination :as sut]
   [speclj.core #?(:clj :refer :cljs :refer-macros) [describe context it should= with-stubs should-have-invoked stub]]))

(defn search-fn
  ([_] (search-fn))
  ([] (for [i (range 100)] {:i i})))

(describe "Pagination"
  (with-stubs)

  (context "has default settings"

    (it "passes the query to the search-fn"
      (with-redefs [search-fn (stub :search-fn)]
        (sut/paginate {:params {:q "blah"}} search-fn {})
        (should-have-invoked :search-fn {:with [{:q "blah" :page 1 :page-size 20}]})))

    (it "db returns entity maps"
      (let [result (sut/paginate {:params {:q "blah"}} search-fn {})]
        (should= 100 (:n result))
        (should= "blah" (:q result))
        (should= 1 (:page result))
        (should= 20 (:page-size result))
        (should= (take 20 (search-fn)) (:entities result))))

    (it "db returns queried vectors"
      (let [result (sut/paginate {:params {:q "blah"}} (fn [_] [[1 "z"] [2 "a"] [3 "b"] [4 "x"]]) {})]
        (should= 4 (:n result))
        (should= "blah" (:q result))
        (should= 1 (:page result))
        (should= 20 (:page-size result))
        (should= [1 4 3 2] (:entities result))))
    )

  (it "page 2"
    (let [result (sut/paginate {:params {:q "blah" :page 2}} search-fn {})]
      (should= 100 (:n result))
      (should= "blah" (:q result))
      (should= 2 (:page result))
      (should= 20 (:page-size result))
      (should= (take 20 (drop 20 (search-fn))) (:entities result))))

  (it "page-size 10"
    (let [result (sut/paginate {:params {:q "blah" :page-size 10}} search-fn {})]
      (should= 100 (:n result))
      (should= "blah" (:q result))
      (should= 1 (:page result))
      (should= 10 (:page-size result))
      (should= (take 10 (search-fn)) (:entities result))))

  (it "presented"
    (let [result (sut/paginate {:params {:q "blah"}} search-fn {:present-fn #(update % :i str)})]
      (should= 100 (:n result))
      (should= "blah" (:q result))
      (should= 1 (:page result))
      (should= 20 (:page-size result))
      (should= (map #(update % :i str) (take 20 (search-fn))) (:entities result))))
  )
