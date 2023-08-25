(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.cleancoders.c3kit/bucket)
(def version (format "2.0.0"))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/copy-dir {:src-dirs ["src/clj" "src/cljc" "src/cljs"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))