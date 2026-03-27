# Synchronous IndexedDB Test Redesign Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace broken async speclj tests with synchronous unit tests + a separate `cljs.test` integration suite.

**Architecture:** Speclj tests use `idb-atom = nil` to test memory-layer logic synchronously. A new `cljs.test` suite with `async`/`done` tests real IndexedDB operations via Playwright. The two suites run independently.

**Tech Stack:** ClojureScript, speclj (unit), cljs.test (integration), Playwright (browser runner), scaffold (compilation)

---

### Task 1: Set Up Integration Test Runner Infrastructure

**Files:**
- Create: `dev/config/cljs-integration.edn`
- Create: `dev/c3kit/bucket/idb_integration_runner.clj`
- Create: `dev/c3kit/bucket/idb_integration.html`
- Modify: `deps.edn`

This task creates the infrastructure to compile and run `cljs.test` integration tests
in a Playwright-controlled browser, completely separate from the speclj suite.

- [ ] **Step 1: Add the `:idb-integration` alias to `deps.edn`**

Add after the existing `:cljs` alias:

```clojure
:idb-integration {:main-opts ["-m" "c3kit.bucket.idb-integration-runner"]}
```

- [ ] **Step 2: Create the CLJS build config**

Create `dev/config/cljs-integration.edn`:

```clojure
{:cache-analysis true
 :optimizations  :none
 :output-dir     "target/cljs-integration/"
 :output-to      "target/cljs-integration/idb_integration.js"
 :pretty-print   true
 :source-map     true
 :sources        ["src/cljc" "src/cljs" "spec/cljs"]
 :verbose        false}
```

- [ ] **Step 3: Create the HTML page**

Create `dev/c3kit/bucket/idb_integration.html`:

```html
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8"/>
  <title>IDB Integration Tests</title>
  <script src="goog/base.js" type="text/javascript"></script>
  <script src="<!--OUTPUT-TO-->" type="text/javascript"></script>
  <script type="text/javascript">
    goog.require("c3kit.bucket.idb_integration_test");

    function runTests() {
      return c3kit.bucket.idb_integration_test.run();
    }
  </script>
</head>
<body>
  <h3 style="margin: 1em">IDB Integration Tests</h3>
</body>
</html>
```

The `<!--OUTPUT-TO-->` placeholder gets replaced by the runner with the actual file URL.

- [ ] **Step 4: Create the Clojure runner**

Create `dev/c3kit/bucket/idb_integration_runner.clj`:

```clojure
(ns c3kit.bucket.idb-integration-runner
  (:require [cljs.build.api :as cljs]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (com.microsoft.playwright ConsoleMessage Playwright)
           (java.util.function Consumer)))

(deftype FnConsumer [accept-fn]
  Consumer
  (accept [_ value] (accept-fn value)))

(def build-config
  (read-string (slurp (io/resource "config/cljs-integration.edn"))))

(defn- compile-cljs! []
  (println "Compiling ClojureScript for integration tests...")
  (cljs/build (apply cljs/inputs (:sources build-config)) build-config)
  (println "Compilation complete."))

(defn- generate-html! []
  (let [template (slurp (io/resource "c3kit/bucket/idb_integration.html"))
        output-to (-> (:output-to build-config) io/file .getAbsolutePath (str/replace "\\" "/"))
        html (str/replace template "<!--OUTPUT-TO-->" (str "file:" output-to))
        out-file (io/file (:output-dir build-config) "integration.html")]
    (spit out-file html)
    out-file))

(defn- run-tests! [html-file]
  (let [pw         (Playwright/create)
        browser    (-> pw .chromium .launch)
        page       (-> browser .newContext .newPage)
        result     (promise)
        errors     (atom [])
        on-console (fn [^ConsoleMessage m]
                     (let [text (.text m)]
                       (println text)
                       (when (re-find #"Ran \d+ tests containing" text)
                         (deliver result text))))
        on-error   (fn [error]
                     (let [msg (str "ERROR: " error)]
                       (swap! errors conj msg)
                       (println msg)))]
    (try
      (.onPageError page (FnConsumer. on-error))
      (.onConsoleMessage page (FnConsumer. on-console))
      (.navigate page (str "file:" (.getAbsolutePath html-file)))
      (.evaluate page "runTests()")
      (let [summary (deref result 30000 nil)]
        (cond
          (nil? summary)
          (do (println "TIMEOUT: Tests did not complete within 30 seconds.") 1)

          (re-find #"0 failures, 0 errors" summary) 0

          :else 1))
      (finally
        (.close browser)
        (.close pw)))))

(defn -main [& _args]
  (compile-cljs!)
  (let [html-file (generate-html!)
        exit-code (run-tests! html-file)]
    (System/exit exit-code)))
```

- [ ] **Step 5: Verify the runner compiles and launches (no tests yet)**

Run: `clj -M:test:idb-integration`

Expected: Compilation succeeds, Playwright opens, "TIMEOUT: Tests did not complete" (since
the test namespace doesn't exist yet). Exit code 1.

If you get import errors for Playwright, verify scaffold is on the classpath via `:test` alias.

- [ ] **Step 6: Commit**

```bash
git add deps.edn dev/config/cljs-integration.edn dev/c3kit/bucket/idb_integration_runner.clj dev/c3kit/bucket/idb_integration.html
git commit -m "add integration test runner infrastructure for cljs.test IDB tests"
```

---

### Task 2: Create Integration Test Namespace with Persistence Round-Trip Test

**Files:**
- Create: `spec/cljs/c3kit/bucket/idb_integration_test.cljs`

- [ ] **Step 1: Create the integration test namespace with a `run` entry point and first test**

Create `spec/cljs/c3kit/bucket/idb_integration_test.cljs`:

```clojure
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
```

- [ ] **Step 2: Run the integration tests**

Run: `clj -M:test:idb-integration`

Expected: Compilation, then output like:
```
Testing c3kit.bucket.idb-integration-test

Ran 1 tests containing 3 assertions.
0 failures, 0 errors.
```

Exit code 0.

- [ ] **Step 3: Commit**

```bash
git add spec/cljs/c3kit/bucket/idb_integration_test.cljs
git commit -m "add persistence round-trip integration test"
```

---

### Task 3: Add Dirty Set Round-Trip Integration Test

**Files:**
- Modify: `spec/cljs/c3kit/bucket/idb_integration_test.cljs`

- [ ] **Step 1: Add the dirty set round-trip test**

Add before the `run` function:

```clojure
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
```

- [ ] **Step 2: Run and verify**

Run: `clj -M:test:idb-integration`

Expected: 2 tests, 0 failures.

- [ ] **Step 3: Commit**

```bash
git add spec/cljs/c3kit/bucket/idb_integration_test.cljs
git commit -m "add dirty set round-trip integration test"
```

---

### Task 4: Add Offline Tx + Sync Lifecycle Integration Test

**Files:**
- Modify: `spec/cljs/c3kit/bucket/idb_integration_test.cljs`

- [ ] **Step 1: Add the sync lifecycle test**

Add before the `run` function:

```clojure
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
```

- [ ] **Step 2: Run and verify**

Run: `clj -M:test:idb-integration`

Expected: 3 tests, 0 failures.

- [ ] **Step 3: Commit**

```bash
git add spec/cljs/c3kit/bucket/idb_integration_test.cljs
git commit -m "add offline tx and sync lifecycle integration test"
```

---

### Task 5: Add Refresh and Rollback Integration Tests

**Files:**
- Modify: `spec/cljs/c3kit/bucket/idb_integration_test.cljs`

- [ ] **Step 1: Add the refresh integration test**

Add before the `run` function:

```clojure
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
```

- [ ] **Step 2: Add the rollback integration test**

Add before the `run` function:

```clojure
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
```

- [ ] **Step 3: Run and verify**

Run: `clj -M:test:idb-integration`

Expected: 5 tests, 0 failures.

- [ ] **Step 4: Commit**

```bash
git add spec/cljs/c3kit/bucket/idb_integration_test.cljs
git commit -m "add refresh and rollback integration tests"
```

---

### Task 6: Restructure idb_io_spec.cljs — Remove Async Tests

**Files:**
- Modify: `spec/cljs/c3kit/bucket/idb_io_spec.cljs`

- [ ] **Step 1: Remove the async `read-entity` context**

Remove lines 53-79 (the entire `"read-entity"` context with its two async tests).
The file should end after the `"serialization"` context's closing paren.

The resulting file:

```clojure
(ns c3kit.bucket.idb-io-spec
  (:require-macros [speclj.core :refer [before context describe it should should= should-not=]])
  (:require [c3kit.bucket.idb-io :as sut]
            [speclj.core]))

(describe "IDB IO"

  (context "schema-hash"
    (it "produces a consistent hash from a legend"
      (let [legend {:user {:id {:type :long} :name {:type :string}}}]
        (should= (sut/schema-hash legend) (sut/schema-hash legend))))

    (it "produces different hashes for different legends"
      (let [legend-1 {:user {:id {:type :long} :name {:type :string}}}
            legend-2 {:user {:id {:type :long} :name {:type :string} :email {:type :string}}}]
        (should-not= (sut/schema-hash legend-1) (sut/schema-hash legend-2)))))

  (context "idb-version"
    (before (.removeItem js/localStorage "test-db-schema-hash")
            (.removeItem js/localStorage "test-db-schema-ver"))

    (it "returns 1 for a new database"
      (let [legend {:user {:id {:type :long} :name {:type :string}}}]
        (should= 1 (sut/idb-version "test-db" legend))))

    (it "returns same version when legend unchanged"
      (let [legend {:user {:id {:type :long} :name {:type :string}}}]
        (sut/idb-version "test-db" legend)
        (should= 1 (sut/idb-version "test-db" legend))))

    (it "increments version when legend changes"
      (let [legend-1 {:user {:id {:type :long} :name {:type :string}}}
            legend-2 {:user {:id {:type :long} :name {:type :string} :email {:type :string}}}]
        (sut/idb-version "test-db" legend-1)
        (should= 2 (sut/idb-version "test-db" legend-2)))))

  (context "serialization"
    (it "round-trips keyword values"
      (let [entity {:id 1 :kind :bibelot :name "widget" :color "red"}]
        (should= entity (sut/js->clj-entity (sut/clj->js-entity entity)))))

    (it "round-trips entities with vector values"
      (let [entity {:id 2 :kind :doodad :names ["alice" "bob"]}]
        (should= entity (sut/js->clj-entity (sut/clj->js-entity entity)))))

    (it "round-trips entities with keyword vector values"
      (let [entity {:id 3 :kind :doodad :letters [:a :b :c]}]
        (should= entity (sut/js->clj-entity (sut/clj->js-entity entity)))))

    (it "round-trips nil for nil input"
      (should= nil (sut/js->clj-entity nil)))))
```

- [ ] **Step 2: Run the speclj tests to verify remaining tests pass**

Run: `clj -M:test:cljs once`

Expected: All remaining tests pass, no `ISwap.-swap!` errors from this file.

- [ ] **Step 3: Commit**

```bash
git add spec/cljs/c3kit/bucket/idb_io_spec.cljs
git commit -m "remove async read-entity tests from idb-io-spec (covered by integration)"
```

---

### Task 7: Restructure idb_common_spec.cljs — Synchronous Tests Only

**Files:**
- Modify: `spec/cljs/c3kit/bucket/idb_common_spec.cljs`

- [ ] **Step 1: Rewrite the file to remove all async tests and restructure sync! test**

Replace the entire file with:

```clojure
(ns c3kit.bucket.idb-common-spec
  (:require-macros [speclj.core :refer [before context describe it should= with-stubs]])
  (:require [c3kit.bucket.idb-common :as sut]
            [c3kit.bucket.indexeddb :as indexeddb]
            [c3kit.bucket.memory :as memory]
            [speclj.core]))

(def legend {:bibelot {:id {:type :long} :name {:type :string}}})

(describe "IDB Common"

  (context "sync!"

    (it "invokes callback with empty vector when idb-atom is nil"
      (let [db      (indexeddb/->IndexedDB (atom legend) (atom {}) (atom nil) "test" (constantly true) memory/entity memory/do-find)
            called? (atom false)]
        (sut/sync! db (fn [entities]
                        (reset! called? true)
                        (should= [] entities)))
        (should= true @called?))))

  (context "ensure-offline-id"
    (before (reset! sut/offline-id-counter 0))

    (it "assigns negative decrementing ID to entity without ID"
      (let [result (sut/ensure-offline-id {:kind :bibelot :name "thing"})]
        (should= -1 (:id result))))

    (it "decrements for each new entity"
      (sut/ensure-offline-id {:kind :bibelot :name "first"})
      (let [result (sut/ensure-offline-id {:kind :bibelot :name "second"})]
        (should= -2 (:id result))))

    (it "preserves existing positive ID"
      (let [result (sut/ensure-offline-id {:kind :bibelot :id 42 :name "existing"})]
        (should= 42 (:id result))))

    (it "preserves existing negative ID"
      (let [result (sut/ensure-offline-id {:kind :bibelot :id -5 :name "existing"})]
        (should= -5 (:id result)))))

  (context "offline-ensure-id"
    (with-stubs)
    (before (reset! sut/offline-id-counter 0))

    (it "uses negative ID when offline"
      (let [result (sut/offline-ensure-id (constantly false) {:kind :bibelot :name "offline"})]
        (should= -1 (:id result))))

    (it "uses positive ID when online"
      (let [result (sut/offline-ensure-id (constantly true) {:kind :bibelot :name "online"})]
        (should= true (pos? (:id result)))))))
```

Note: The `sync!` test with nil `idb-atom` calls `(js/Promise.resolve (callback []))`.
The callback fires synchronously inside `js/Promise.resolve`, so `@called?` is `true`
immediately after the call. No `.then` needed.

- [ ] **Step 2: Run the speclj tests**

Run: `clj -M:test:cljs once`

Expected: All tests pass, no `ISwap.-swap!` errors from this file.

- [ ] **Step 3: Commit**

```bash
git add spec/cljs/c3kit/bucket/idb_common_spec.cljs
git commit -m "restructure idb-common-spec to synchronous-only tests"
```

---

### Task 8: Restructure indexeddb_spec.cljs — Synchronous Unit Tests

**Files:**
- Modify: `spec/cljs/c3kit/bucket/indexeddb_spec.cljs`

- [ ] **Step 1: Rewrite the file, replacing async tests with synchronous equivalents**

Replace the entire file with:

```clojure
(ns c3kit.bucket.indexeddb-spec
  (:require-macros [speclj.core :refer [around before context describe it should should= should-not-be-nil]]
                   [c3kit.bucket.api :refer [with-safety-off]])
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as idb]
            [c3kit.bucket.indexeddb]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.memory-spec :as memory-spec]
            [c3kit.bucket.spec-helperc :as helperc]
            [c3kit.apron.schema :as s]
            [speclj.core]))

(def config {:impl :indexeddb :db-name "test-indexeddb"})

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

(describe "IndexedDB"

  (around [it] (with-safety-off (it)))

  (spec/crud-specs config)
  (spec/nil-value-specs config)
  (spec/count-specs config)
  (spec/find-specs config)
  (spec/filter-specs config)
  (spec/reduce-specs config)
  (spec/kind-in-entity-is-optional config)
  (spec/broken-in-datomic config)
  (spec/multi-value-fields config)
  (spec/cas config)
  (memory-spec/migrator-specs)

  (context "online-fn"

    (it "defaults to (constantly true) when :online? not provided"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-idb-online-1"} [bibelot])]
        (should= true ((.-online-fn db)))))

    (it "uses the provided :online? callback"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-idb-online-2" :online? #(deref online?)} [bibelot])]
        (should= true ((.-online-fn db)))
        (reset! online? false)
        (should= false ((.-online-fn db))))))

  (context "offline tx (memory effects)"

    (before (reset! idb/offline-id-counter 0))

    (it "offline create assigns negative ID"
      (let [db    (api/create-db {:impl :indexeddb :db-name "test-mem-1" :online? (constantly false)} [bibelot])
            saved (api/-tx db {:kind :bibelot :name "offline-widget"})]
        (should= -1 (:id saved))
        (should= "offline-widget" (:name saved))
        (should= 1 (count (api/find-by- db :bibelot :name "offline-widget")))))

    (it "online create uses positive ID"
      (let [db    (api/create-db {:impl :indexeddb :db-name "test-mem-2"} [bibelot])
            saved (api/-tx db {:kind :bibelot :name "online-widget"})]
        (should (pos? (:id saved)))))

    (it "offline update changes entity in memory"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-mem-3" :online? #(deref online?)} [bibelot])
            saved   (api/-tx db {:kind :bibelot :name "widget" :size 5})]
        (reset! online? false)
        (api/-tx db (assoc saved :size 10))
        (should= 10 (:size (api/entity- db :bibelot (:id saved))))))

    (it "offline delete removes entity from memory"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-mem-4" :online? #(deref online?)} [bibelot])
            saved   (api/-tx db {:kind :bibelot :name "widget" :size 5})]
        (reset! online? false)
        (api/-tx db (assoc saved :db/delete? true))
        (should= 0 (count (api/find-by- db :bibelot :name "widget")))))

    (it "offline delete of offline-created entity"
      (let [db    (api/create-db {:impl :indexeddb :db-name "test-mem-5" :online? (constantly false)} [bibelot])
            saved (api/-tx db {:kind :bibelot :name "offline-widget"})]
        (should= -1 (:id saved))
        (api/-tx db (assoc saved :db/delete? true))
        (should= 0 (count (api/find-by- db :bibelot :name "offline-widget")))))

    (it "offline batch create assigns negative IDs"
      (let [db      (api/create-db {:impl :indexeddb :db-name "test-mem-6" :online? (constantly false)} [bibelot])
            results (api/-tx* db [{:kind :bibelot :name "w1"} {:kind :bibelot :name "w2"}])]
        (should= -1 (:id (first results)))
        (should= -2 (:id (second results)))))

    (it "mixed creates and deletes in offline batch"
      (let [online? (atom true)
            db      (api/create-db {:impl :indexeddb :db-name "test-mem-7" :online? #(deref online?)} [bibelot])
            saved   (api/-tx db {:kind :bibelot :name "existing"})]
        (reset! online? false)
        (let [results (api/-tx* db [{:kind :bibelot :name "new-offline"} (assoc saved :db/delete? true)])]
          (should= 1 (count (api/find-by- db :bibelot :name "new-offline")))
          (should= 0 (count (api/find-by- db :bibelot :name "existing")))))))

  (context "sync lifecycle (memory effects)"

    (before (reset! idb/offline-id-counter 0))

    (it "sync! with nil idb invokes callback with empty vector"
      (let [db       (api/create-db {:impl :indexeddb :db-name "test-sync-mem-1"} [bibelot])
            received (atom nil)]
        (idb/sync! db (fn [entities] (reset! received entities)))
        (should= [] @received)))

    (it "sync-complete! soft-deletes negative IDs and adds server entities"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-sync-mem-2" :online? (constantly false)} [bibelot])]
        (api/-tx db {:kind :bibelot :name "offline-widget" :size 5})
        (idb/sync-complete! db #{-1} [{:kind :bibelot :id 9001 :name "offline-widget" :size 5}])
        (should= 0 (count (api/find-by- db :bibelot :id -1)))
        (should= "offline-widget" (:name (api/entity- db :bibelot 9001))))))

  (context "refresh! (memory effects)"

    (before (reset! idb/offline-id-counter 0))

    (it "purges negative-ID entities and loads fresh data"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-refresh-mem-1"} [bibelot])]
        (api/-tx db {:kind :bibelot :id -1 :name "offline-widget" :size 5})
        (api/-tx db {:kind :bibelot :id 100 :name "server-widget" :size 10})
        (idb/refresh! db [{:kind :bibelot :id 200 :name "fresh-widget" :size 20}])
        (should= 0 (count (filter #(neg? (:id %)) (api/find-by- db :bibelot :name "offline-widget"))))
        (should= "server-widget" (:name (api/entity- db :bibelot 100)))
        (should= "fresh-widget" (:name (api/entity- db :bibelot 200)))))

    (it "returns empty list for empty input"
      (let [db (api/create-db {:impl :indexeddb :db-name "test-refresh-mem-2" :online? (constantly false)} [bibelot])]
        (api/-tx db {:kind :bibelot :name "offline-widget" :size 5})
        (let [result (idb/refresh! db [])]
          (should= [] result)
          (should= 1 (count (api/find-by- db :bibelot :name "offline-widget"))))))))
```

Key changes:
- All DBs created with no `init!` call, so `idb-atom` stays nil — purely synchronous
- Removed persistence, rollback contexts (covered by integration tests)
- Offline tx tests assert memory state directly
- Sync lifecycle tests verify memory effects of `sync-complete!`
- Refresh tests verify memory purge/replace

- [ ] **Step 2: Run the speclj tests**

Run: `clj -M:test:cljs once`

Expected: All tests pass, no `ISwap.-swap!` errors from this file.

- [ ] **Step 3: Commit**

```bash
git add spec/cljs/c3kit/bucket/indexeddb_spec.cljs
git commit -m "restructure indexeddb-spec to synchronous memory-only tests"
```

---

### Task 9: Restructure re_indexeddb_spec.cljs — Synchronous Unit Tests

**Files:**
- Modify: `spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs`

- [ ] **Step 1: Rewrite the file, replacing async tests with synchronous equivalents**

Replace the entire file with:

```clojure
(ns c3kit.bucket.re-indexeddb-spec
  (:require-macros [speclj.core :refer [around before context describe it should should=]]
                   [c3kit.bucket.api :refer [with-safety-off]])
  (:require [c3kit.bucket.api :as api]
            [c3kit.bucket.idb-common :as idb]
            [c3kit.bucket.re-indexeddb]
            [c3kit.bucket.impl-spec :as spec]
            [c3kit.bucket.memory-spec :as memory-spec]
            [c3kit.bucket.spec-helperc :as helperc]
            [c3kit.apron.schema :as s]
            [reagent.core :as r]
            [speclj.core]))

(def config {:impl :re-indexeddb :db-name "test-re-indexeddb"})

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

(describe "ReIndexedDB"

  (around [it] (with-safety-off (it)))

  (spec/crud-specs config)
  (spec/nil-value-specs config)
  (spec/count-specs config)
  (spec/find-specs config)
  (spec/filter-specs config)
  (spec/reduce-specs config)
  (spec/kind-in-entity-is-optional config)
  (spec/broken-in-datomic config)
  (spec/multi-value-fields config)
  (spec/cas config)
  (memory-spec/migrator-specs)

  (it "uses reagent atom for store"
    (let [db (api/create-db {:impl :re-indexeddb :db-name "test-reidb-store"} [bibelot])]
      (should (satisfies? IAtom (.-store db)))))

  (context "online-fn"

    (it "defaults to (constantly true) when :online? not provided"
      (let [db (api/create-db {:impl :re-indexeddb :db-name "test-reidb-online-1"} [bibelot])]
        (should= true ((.-online-fn db)))))

    (it "uses the provided :online? callback"
      (let [online? (atom true)
            db      (api/create-db {:impl :re-indexeddb :db-name "test-reidb-online-2" :online? #(deref online?)} [bibelot])]
        (should= true ((.-online-fn db)))
        (reset! online? false)
        (should= false ((.-online-fn db))))))

  (context "offline tx (memory effects)"

    (before (reset! idb/offline-id-counter 0))

    (it "assigns negative ID on offline create"
      (let [online? (atom false)
            db      (api/create-db {:impl :re-indexeddb :db-name "test-reidb-mem-1" :online? #(deref online?)} [bibelot])
            saved   (api/-tx db {:kind :bibelot :name "offline-widget"})]
        (should= -1 (:id saved))
        (should= 1 (count (api/find-by- db :bibelot :name "offline-widget")))))

    (it "sync-complete! works with reagent store"
      (let [online? (atom false)
            db      (api/create-db {:impl :re-indexeddb :db-name "test-reidb-mem-2" :online? #(deref online?)} [bibelot])]
        (api/-tx db {:kind :bibelot :name "offline-widget"})
        (idb/sync-complete! db #{-1} [{:kind :bibelot :id 9001 :name "offline-widget"}])
        (should= 0 (count (api/find-by- db :bibelot :id -1)))
        (should= "offline-widget" (:name (api/entity- db :bibelot 9001)))))))
```

Key changes:
- Removed persistence context (covered by integration tests)
- Removed rollback context (covered by integration tests)
- Offline tx tests verify memory effects directly with nil `idb-atom`
- Removed `should-not-be-nil` and `around-all` from requires (no longer used)

- [ ] **Step 2: Run the speclj tests**

Run: `clj -M:test:cljs once`

Expected: All tests pass, no `ISwap.-swap!` errors from this file.

- [ ] **Step 3: Commit**

```bash
git add spec/cljs/c3kit/bucket/re_indexeddb_spec.cljs
git commit -m "restructure re-indexeddb-spec to synchronous memory-only tests"
```

---

### Task 10: Add Integration Tests to CI

**Files:**
- Modify: `.github/workflows/test.yml`

- [ ] **Step 1: Add integration test step to CI workflow**

Add after the existing "Run ClojureScript Tests" step:

```yaml
      - name: Run IDB Integration Tests
        run: clojure -M:test:idb-integration
```

- [ ] **Step 2: Verify the full CLJS test suite is clean**

Run locally:
```bash
clj -M:test:cljs once
```

Expected: All speclj tests pass with **zero** `ISwap.-swap!` errors.

Then:
```bash
clj -M:test:idb-integration
```

Expected: All 5 integration tests pass.

- [ ] **Step 3: Commit**

```bash
git add .github/workflows/test.yml
git commit -m "add IDB integration tests to CI workflow"
```
