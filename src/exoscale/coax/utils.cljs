(ns exoscale.coax.utils)

(defmacro invalid-on-throw!
  [& body]
  `(try
     ~@body
     (catch :default _#
       :exoscale.coax/invalid)))
