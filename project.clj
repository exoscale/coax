(defproject exoscale/coax "0.1.3-SNAPSHOT"
  :description "exo spec-coerce fork"
  :url "https://github.com/exoscale/coax"
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :profiles {:dev  {:dependencies [[org.clojure/test.check "0.9.0"]
                                   [org.clojure/clojurescript "1.9.946"]]}
             :test  {:dependencies []}}
  :pedantic? :warn
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ["deploy" "clojars"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]
  :jvm-opts ["-Duser.timezone=GMT"]
  :global-vars {*warn-on-reflection* true})
