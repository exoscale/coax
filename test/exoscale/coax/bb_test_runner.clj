#!/usr/bin/env bb
(ns exoscale.coax.bb-test-runner
  (:require [clojure.test :refer [run-tests]]
            [babashka.classpath :as cp]
            [babashka.deps :as deps]))

;; bb -f test/exoscale/coax/bb_test_runner.clj

(deps/add-deps {:deps '{org.babashka/spec.alpha {:git/url "https://github.com/babashka/spec.alpha"
                                                 :git/sha "1a841c4cc1d4f6dab7505a98ed2d532dd9d56b78"}}})
(cp/add-classpath "src:test")

(require 'exoscale.coax-test)

(run-tests 'exoscale.coax-test)