(ns exoscale.coax.coercer
  (:refer-clojure :exclude [identity])
  (:require [clojure.string :as str]
            #?(:clj [clojure.instant]))
  #?(:clj (:import (java.util UUID)
                   (java.net URI))))

(defmacro invalid-on-throw!
  [& body]
  `(try
     ~@body
     (catch #?(:clj Exception :cljs :default) _#
       :exoscale.coax/invalid)))

(defn to-string
  [x _]
  ;; we should only try to coerce "scalars"
  (cond
    (string? x)
    x
    (or (number? x)
        (char? x)
        (boolean? x)
        (ident? x)
        (inst? x)
        (nil? x)
        (uuid? x)
        (uri? x))
    (str x)
    :else :exoscale.coax/invalid))

(defn to-long
  [x _]
  (invalid-on-throw!
   (cond (string? x)
         #?(:clj (Long/parseLong x)
            :cljs (if (= "NaN" x)
                    js/NaN
                    (let [v (js/parseInt x)]
                      (if (js/isNaN v) x v))))
         (number? x) (long x)
         :else :exoscale.coax/invalid)))

(defn to-double
  [x _]
  (invalid-on-throw!
   (cond (string? x)
         #?(:clj  (case x
                    "##-Inf" ##-Inf
                    "##Inf" ##Inf
                    "##NaN" ##NaN
                    "NaN" ##NaN
                    "Infinity" ##Inf
                    "-Infinity" ##-Inf
                    (Double/parseDouble x))
            :cljs (if (= "NaN" x)
                    js/NaN
                    (let [v (js/parseFloat x)]
                      (if (js/isNaN v) x v))))
         (number? x) (double x)
         :else :exoscale.coax/invalid)))

(defn to-number
  [x opts]
  (if (number? x)
    x
    (let [l (to-long x opts)
          d (to-double x opts)]
      (if (and (every? number? [l d])
               (== d l))
        l
        d))))

(defn to-uuid
  [x _]
  (cond
    (uuid? x)
    x
    (string? x)
    (invalid-on-throw!
     #?(:clj (UUID/fromString x)
        :cljs (uuid x)))
    :else :exoscale.coax/invalid))

(defn to-inst
  [x _]
  (cond
    (inst? x)
    x
    (string? x)
    (invalid-on-throw!
     #?(:clj (clojure.instant/read-instant-date x)
        :cljs (cljs.reader/parse-timestamp x)))
    :else :exoscale.coax/invalid))

(defn to-boolean
  [x _]
  (case x
    (true "true") true
    (false "false") false
    :exoscale.coax/invalid))

(defn to-keyword
  [x _]
  (cond
    (keyword? x)
    x

    (string? x)
    (keyword (cond-> x
               (str/starts-with? x ":")
               (subs 1)))

    (symbol? x)
    (keyword x)

    :else :exoscale.coax/invalid))

(defn to-symbol
  [x _]
  (cond
    (symbol? x)
    x
    (string? x)
    (symbol x)
    :else :exoscale.coax/invalid))

(defn to-ident
  [x opts]
  (cond
    (string? x)
    (if (str/starts-with? x ":")
      (to-keyword x opts)
      (symbol x))
    (ident? x)
    x
    :else :exoscale.coax/invalid))

#?(:clj
   (defn to-decimal
     [x _]
     (invalid-on-throw!
      (if (and (string? x)
               (str/ends-with? x "M"))
        (bigdec (subs x 0 (dec (count x))))
        (bigdec x)))))

#?(:clj
   (defn to-uri
     [x _]
     (cond
       (uri? x) x
       (string? x)
       (URI. x)
       :else :exoscale.coax/invalid)))

(defn identity
  [x _]
  x)
