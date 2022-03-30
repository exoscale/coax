(ns exoscale.coax.cljs-test-runner
  (:require [cljs.test :refer-macros [run-tests]] ; new
            [exoscale.coax-test]))

(enable-console-print!)
(run-tests 'exoscale.coax-test)
