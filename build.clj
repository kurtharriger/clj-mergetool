(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.deps :as t]
            [clojure.tools.build.api :as b]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [babashka.process :refer [shell]]))

(def lib 'net.clojars.kurtharriger/kurtharriger.clj-mergetool)
(def main 'kurtharriger.clj-mergetool)
(def class-dir "target/classes")

(defn test "Run all the tests." [opts]
  (println "\nRunning tests...")
  (let [basis    (b/create-basis {:aliases [:test]})
        combined (t/combine-aliases basis [:test])
        cmds     (b/java-command
                  {:basis basis
                   :java-opts (:jvm-opts combined)
                   :main      'clojure.main
                   :main-args ["-m" "cognitect.test-runner"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)

(defn- uber-opts [opts]
  (assoc opts
         :lib lib :main main
         :uber-file (format "target/%s-%s.jar" lib (slurp "resources/VERSION"))
         :basis (b/create-basis {})
         :class-dir class-dir
         :src-dirs ["src"]
         :ns-compile [main]))

(defn native-image [opts]
  (println opts)
  (println "Compiling to native image with GraalVM...")
  (let [{jar-path :uber-file} (uber-opts opts)]
    (let [bin-path (str "target/clj-mergetool")]
      (shell "native-image"
             "--no-fallback"
             "--initialize-at-build-time"
             "-jar" jar-path
             "-H:Name=" bin-path))))

(defn extract-change-notes [changelog unreleased-header]
  (->> (string/split-lines changelog)
       (drop-while #(not= % unreleased-header))
       (drop 1)
       (drop-while empty?)
       (take-while #(empty? (re-find #"^(## \[|---)" %)))
       (vec)
       (rseq)
       (drop-while empty?)
       (reverse)
       (string/join "\n")))

(defn working-copy-dirty? [& files]
  (let [status (:out (apply shell {:out :string} "git" "status" "--porcelain" files))]
    (not (empty? status))))

(comment
  (working-copy-dirty? "CHANGELOG.md"))

(defn release-version [opts]
  (when (working-copy-dirty?)
    (println "commit changes before releasing")
    (System/exit 1))
  (let [changelog-path "CHANGELOG.md"
        changelog (slurp changelog-path)
        ;; match and capture version from header: ## 0.1.0 - [Unreleased]
        unreleased (re-seq   #"(?m)^##\s*(\d+)\.(\d+)\.(\d+)\s*-\s*\[Unreleased\]$" changelog)
        _ (assert (= 1 (count unreleased)) "Expected exactly one version tagged as unreleased in the changelog")
        [unreleased-header major minor patch] (first unreleased)
        ;; extract changlog notes
        changes (extract-change-notes changelog unreleased-header)

        ;; match and capture prior version from link  [Unreleased]: https://github.com/kurtharriger/clj-mergetool/compare/0.0.0...HEAD
        unreleased-link (re-seq   #"(?m)^\[Unreleased\].*\/(.*)\.\.\.HEAD$" changelog)
        _ (assert (count unreleased) "Expected exactly one [Unreleased] link in changelog")
        [unreleased-link prior-version] (first unreleased-link)

        ;;
        release-version (str major "." minor "." patch)
        release-date (format "%tF" (System/currentTimeMillis))
        release-header (str "## [" release-version "] - " release-date)
        next-version-header (str "## [" major "." (inc (parse-long minor)) "." patch "] - " release-date)

        release-link (-> unreleased-link
                         (string/replace "Unreleased" release-version)
                         (string/replace "HEAD" release-version))
        next-unreleased-link (string/replace unreleased-link prior-version release-version)

        updated-changelog (-> changelog
                              (string/replace unreleased-header (str next-version-header "\n\n...\n\n" release-header))
                              (string/replace unreleased-link (str next-unreleased-link "\n" release-link)))]
    (spit "resources/VERSION" release-version)
    (spit changelog-path updated-changelog)
    (shell "git" "add" "resources/VERSION" "CHANGELOG.md")
    (shell "git" "commit" "-m" (str "Release " release-version "\n\n" changes))
    (shell "git" "tag" release-version)))

(comment
  (release-version {})

  :rcf)

(defn ci "Run the CI pipeline of tests (and build the uberjar)." [opts]
  (test opts)
  (b/delete {:path "target"})
  (release-version opts)
  (let [opts (uber-opts opts)]
    (println "\nCopying source...")
    (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})
    (println (str "\nCompiling " main "..."))
    (b/compile-clj opts)
    (println "\nBuilding JAR...")
    (b/uber opts)))