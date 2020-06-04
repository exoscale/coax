(defproject exoscale/coax "1.0.0-alpha5-SNAPSHOT"
  :description "exo spec-coerce fork"

  :url "https://github.com/exoscale/coax"

  :license {:name "ISC"}

  :dependencies [[org.clojure/clojure "1.10.1"]]

  :profiles {:test {:dependencies [[org.clojure/test.check "0.9.0"]
                                   [org.clojure/clojurescript "1.10.758"]]
                    :plugins [[lein-cljsbuild "1.1.8" :exclusions [[org.clojure/clojure]]]
                              [lein-doo "0.1.10"]
                              [lein-figwheel "0.5.20"]]

                    :cljsbuild {:builds [{:id "test"
                                          :source-paths ["src" "test"]
                                          :compiler {:main exoscale.coax.cljs-test-runner
                                                     :optimizations :none
                                                     :output-to "resources/public/cljs/tests/all-tests.js"}}]}

                    :doo {:build "test"}}}
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
