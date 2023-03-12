(ns build
  (:require [clojure.tools.build.api :as b]))

(def build-folder "target")
(def class-dir (str build-folder "/classes"))

(def basis (delay (b/create-basis {:project "deps.edn"})))
(def version "0.0.1")
(def app-name "plauna")
(def uber-file-name (format "%s/%s-%s-standalone.jar" build-folder app-name version)) ; path for result uber file

(defn clean [_]
  (b/delete {:path "target"})
  (println (format "Build folder \"%s\" removed" build-folder)))

(defn uber [_]
  (clean nil)

  (b/copy-dir {:src-dirs   ["src" "resources"]
               :ignores [".*mbox" ".*eml" ".*txt"] ; ignore test files
               :target-dir class-dir})

  (b/compile-clj {:basis     @basis
                  :ns-compile '[plauna.entry]
                  :java-opts ["-Dclojure.compiler.direct-linking=true" ]
                  :class-dir class-dir})

  (b/uber {:class-dir class-dir
           :uber-file uber-file-name
           :basis     @basis
           :java-opts ["-Dclojure.compiler.direct-linking=true" ]
           :main      'plauna.entry})                ; here we specify the entry point for uberjar
  
  (println (format "Uber file created: \"%s\"" uber-file-name)))
