(ns exoscale.coax.coercer
  (:refer-clojure :exclude [identity])
  (:require [clojure.string :as str]
            #?(:clj [clojure.instant]))
  #?(:clj (:import (java.util Date TimeZone UUID)
                   (java.net URI)
                   (java.time LocalDate LocalDateTime ZoneId)
                   (java.time.format DateTimeFormatter))))

(defn to-string
  [x _]
  (str x))

(defn to-long
  [x _]
  (cond (string? x)
        (try
          #?(:clj (Long/parseLong x)
             :cljs (if (= "NaN" x)
                     js/NaN
                     (let [v (js/parseInt x)]
                       (if (js/isNaN v) x v))))
          (catch #?(:clj Exception :cljs :default) _
            x))
        (number? x) (long x)
        :else x))

(defn to-double
  [x _]
  (cond (string? x)
        (try
          #?(:clj  (case x
                     "##-Inf"    ##-Inf
                     "##Inf"     ##Inf
                     "##NaN"     ##NaN
                     "NaN"       ##NaN
                     "Infinity"  ##Inf
                     "-Infinity" ##-Inf
                     (Double/parseDouble x))
             :cljs (if (= "NaN" x)
                     js/NaN
                     (let [v (js/parseFloat x)]
                       (if (js/isNaN v) x v))))
          (catch #?(:clj Exception :cljs :default) _
            x))
        (number? x) (double x)
        :else       x))

(defn to-uuid
  [x _]
  (if (string? x)
    (try
      #?(:clj  (UUID/fromString x)
         :cljs (uuid x))
      (catch #?(:clj Exception :cljs :default) _
        x))
    x))

#?(:clj (def ^:dynamic *inst-formats*
          ["yyyy/M/d H:m:s" "yyyy/M/d H:m" "yyyy/M/d"
           "M/d/yyyy H:m:s" "M/d/yyyy H:m" "M/d/yyyy"
           "yyyy-M-d H:m:s" "yyyy-M-d H:m" "yyyy-M-d"
           "M-d-yyyy H:m:s" "M-d-yyyy H:m" "M-d-yyyy"
           "EEE MMM dd HH:mm:ss zzz yyyy"]))

#?(:clj
   (defn- flexible-parse-inst [x]
     (try
       (clojure.instant/read-instant-timestamp x)
       (catch Exception _
         (let [zone (ZoneId/of (.getID (TimeZone/getDefault)))]
           (or (some #(try
                        (Date/from
                         (.toInstant
                          (.atZone
                           (LocalDateTime/parse x (DateTimeFormatter/ofPattern %))
                           zone)))
                        (catch Exception _)) *inst-formats*)
               (some #(try
                        (Date/from
                         (.toInstant
                          (.atStartOfDay
                           (LocalDate/parse x (DateTimeFormatter/ofPattern %))
                           zone)))
                        (catch Exception _)) *inst-formats*)
               x))))))

(defn to-inst
  [x _]
  (if (string? x)
    (try
      #?(:clj (flexible-parse-inst x)
         :cljs (js/Date. x))
      (catch #?(:clj Exception :cljs :default) _
        x))
    x))

(defn to-boolean
  [x _]
  (case x
    "true" true
    "false" false
    x))

(defn to-keyword
  [x _]
  (cond (string? x)
        (keyword (cond-> x
                   (str/starts-with? x ":")
                   (subs 1)))
        (symbol? x) (keyword x)
        :else x))

(defn to-symbol
  [x _]
  (cond-> x
    (string? x)
    symbol))

(defn to-ident
  [x opts]
  (if (string? x)
    (if (str/starts-with? x ":")
      (to-keyword x opts)
      (symbol x))
    x))

#?(:clj
   (defn to-decimal
     [x _]
     (try
       (if (and (string? x) (str/ends-with? x "M"))
         (bigdec (subs x 0 (dec (count x))))
         (bigdec x))
       (catch Exception _ x))))

#?(:clj
   (defn to-uri
     [x _]
     (cond-> x
       (string? x)
       (URI.))))

(defn identity
  [x _]
  x)
