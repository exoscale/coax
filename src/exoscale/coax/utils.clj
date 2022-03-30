(ns exoscale.coax.utils)

(defmacro invalid-on-throw!
  [& body]
  `(try
     ~@body
     (catch Exception _#
       :exoscale.coax/invalid)))
