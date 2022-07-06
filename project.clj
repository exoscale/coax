(defproject exoscale/coax "1.0.0-alpha22-SNAPSHOT"
  :description "exo spec-coerce fork"

  :url "https://github.com/exoscale/coax"

  :license {:name "ISC"}

  :dependencies [[org.clojure/clojure "1.11.0"]]

  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [org.clojure/clojurescript "1.11.4"]]
                   :cljfmt {:remove-multiple-non-indenting-spaces? true}}}
  :pedantic? :warn

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ["deploy" "clojars"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]

  :global-vars {*warn-on-reflection* true})
