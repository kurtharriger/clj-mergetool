(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.deps :as t]
            [clojure.tools.build.api :as b]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [babashka.fs :as fs]
            [babashka.process :refer [shell sh]]))

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


(defn can-build-native-image? []
  (let [os-windows? (.startsWith (System/getProperty "os.name") "Windows")
        proc (if os-windows? (sh "where" "native-image.cmd") (sh "which" "native-image"))]
    (= 0 (:exit proc))))

(comment
  (:out (sh {:out :string} "which" "native-image"))
  (can-build-native-image?)

  :rcf)

(defn- uber-opts [opts]
  (cond->
   (assoc opts
          :lib lib :main main
          :uber-file (format "target/%s.jar" lib)
          :basis (b/create-basis {})
          :class-dir class-dir
          :src-dirs ["src"]
          :ns-compile [main]
          :binary "target/clj-mergetool")))

(defn native-image [opts]
  (when-not (can-build-native-image?)
    (println "native-image not found. Verify GraalVM is installed and on the PATH.")
    (System/exit 1))
  (println "Compiling to native image with GraalVM...")
  (let [{jar-path :uber-file bin-path :binary} (uber-opts opts)]
    (shell "native-image"
           "--no-fallback"
           "--initialize-at-build-time"
           "-jar" jar-path
           "-H:+UnlockExperimentalVMOptions"
           "-H:Name=" bin-path)))

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
    (boolean (seq status))))

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
        next-version-header (str "## " major "." (inc (parse-long minor)) ".0 - [Unreleased]")

        release-link (-> unreleased-link
                         (string/replace "Unreleased" release-version)
                         (string/replace "HEAD" release-version))
        next-unreleased-link (string/replace unreleased-link prior-version release-version)

        updated-changelog (-> changelog
                              (string/replace unreleased-header (str next-version-header "\n\n...\n\n" release-header))
                              (string/replace unreleased-link (str next-unreleased-link "\n" release-link)))]
    (when (= "..." changes)
      (binding [*out* *err*]
        (println "Change log not updated")
        (System/exit 1)))
    (spit "resources/VERSION" release-version)
    (spit changelog-path updated-changelog)
    (shell "git" "add" "resources/VERSION" "CHANGELOG.md")
    (shell "git" "commit" "-m" (str "Release " release-version "\n\n" changes))
    (shell "git" "tag" release-version)
    (spit "RELEASE_NOTES.md" changes))
  nil)

(comment
  (release-version {})

  :rcf)

(defn make-executable-jar [jar out]
  (with-open [o (io/output-stream out)]
    (io/copy "#!/usr/bin/env java -jar\n" o)
    (io/copy (io/file jar) o))
  (shell "chmod" "+x" out))

(defn build [opts]
  (b/delete {:path "target"})
  (let [opts (uber-opts opts)
        uber-file (:uber-file opts)
        ; unable to read VERSION file via io/resource in the native image
        ; tried adding resources-config.json but doesn't seem to work
        ; so embeeded version in source with macro instead and commented this out
        ;resource-config-path (fs/file class-dir "META-INF" "kurtharriger" "clj-mergetool")
        ]

    (println "Buidling version " (slurp "resources/VERSION"))
    (println "\nCopying source...")
    (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})
    ;; (fs/create-dirs resource-config-path)
    ;; (fs/copy (fs/file "resources-config.json") resource-config-path)
    (println (str "\nCompiling " main "..."))
    (b/compile-clj opts)
    (println "\nBuilding JAR...")
    (b/uber opts)
    (if (can-build-native-image?)
      (do (println "Building native image...")
          (native-image opts))
      (make-executable-jar uber-file (:binary opts))))
  opts)


(defn install
  "Link clj-mergetool to ~/.local/bin"
  [{:keys [link] :as opts}]
  (let [install-dir (fs/file (System/getenv "HOME") ".local" "bin")
        bin-path (str (fs/file install-dir "clj-mergetool"))
        opts (uber-opts opts)]
    (fs/delete-if-exists bin-path)
    (if link
      (shell "ln" "-s" (str (fs/absolutize (fs/file "bin" "clj-mergetool"))) bin-path)
      (let [{:keys [binary]} (build opts)]
        (fs/copy binary bin-path)))))

(defn ci "Run the CI pipeline of tests (and build the uberjar)." [opts]
  (test opts)
  (build opts))