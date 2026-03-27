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
                       (when (re-find #"\d+ failures, \d+ errors" text)
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
