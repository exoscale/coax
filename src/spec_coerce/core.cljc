(ns spec-coerce.core
  (:refer-clojure :exclude [def])
  (:require [com.wsscode.spec-inspec :as si]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]
            [clojure.string :as str]
            #?(:clj
               [clojure.instant]))
  #?(:clj
     (:import (java.util Date TimeZone UUID)
              (java.net URI)
              (java.time LocalDate LocalDateTime ZoneId)
              (java.time.format DateTimeFormatter))))

(declare coerce)

(defonce ^:private registry-ref (atom {}))

(def ^:dynamic ^:deprecated
  *overrides*
  "Allows overriding of specs in the registry within a local binding
  context.

  (binding [sc/*overrides* {::my-key my-coerce-fn}]
    (sc/coerce ::my/spec data))"
  {})

(defn parse-string
  ([x] (parse-string x nil))
  ([x _] (str x)))

(defn parse-long
  ([x] (parse-long x nil))
  ([x _]
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
         :else       x)))

(defn parse-double
  ([x] (parse-double x nil))
  ([x _]
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
         :else       x)))

(defn parse-uuid
  ([x] (parse-uuid x nil))
  ([x _]
   (if (string? x)
     (try
       #?(:clj  (UUID/fromString x)
          :cljs (uuid x))
       (catch #?(:clj Exception :cljs :default) _
         x))
     x)))

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

(defn parse-inst
  ([x] (parse-inst x nil))
  ([x _]
   (if (string? x)
     (try
       #?(:clj  (flexible-parse-inst x)
          :cljs (js/Date. x))
       (catch #?(:clj Exception :cljs :default) _
         x))
     x)))

(defn parse-boolean
  ([x] (parse-boolean x nil))
  ([x _]
   (case x
     "true" true
     "false" false
     x)))

(defn parse-keyword
  ([x] (parse-keyword x nil))
  ([x _]
   (cond (string? x)
         (if (str/starts-with? x ":")
           (keyword (subs x 1))
           (keyword x))
         (symbol? x) (keyword x)
         :else       x)))

(defn parse-symbol
  ([x] (parse-symbol x nil))
  ([x _]
   (cond-> x
     (string? x)
     symbol)))

(defn parse-ident
  ([x] (parse-ident x nil))
  ([x opts]
   (if (string? x)
     (if (str/starts-with? x ":")
       (parse-keyword x opts)
       (symbol x))
     x)))

(defn parse-nil
  ([x] (parse-nil x nil))
  ([x _]
   (case x
     ("nil" "null") nil
     x)))

(defn parse-or [[_ & pairs]]
  (fn [x opts]
    (reduce (fn [x [_ pred]]
              (let [coerced (coerce pred x opts)]
                (if (= x coerced)
                  x
                  (reduced coerced))))
            x
            (partition 2 pairs))))

(defn- map-seq
  [fun x]
  (if (vector? x)
    (mapv fun x)
    (map fun x)))

(defn parse-coll-of [[_ pred & _]]
  (fn [x opts]
    (cond->> x
      (sequential? x)
      (map-seq #(coerce pred % opts)))))

(defn parse-map-of [[_ kpred vpred & _]]
  (fn [x opts]
    (cond->> x
      (associative? x)
      (into (empty x) ;; ensure we copy meta
            (map (fn [[k v]]
                   [(coerce kpred k opts)
                    (coerce vpred v opts)]))))))

(defn parse-tuple [[_ & preds]]
  (fn [x opts]
    (cond->> x
      (sequential? x)
      (mapv #(coerce %1 %2 opts)
            preds))))

(defn parse-multi-spec
  [[_ f retag & _]]
  (let [f (resolve f)]
    (fn [x opts]
      (if (associative? x)
        (coerce (s/form (f (retag x)))
                x
                opts)
        x))))

#?(:clj
   (defn parse-decimal
     ([x] (parse-decimal x nil))
     ([x _]
      (try
        (if (and (string? x) (str/ends-with? x "M"))
          (bigdec (subs x 0 (dec (count x))))
          (bigdec x))
        (catch Exception _ x)))))

#?(:clj
   (defn parse-uri
     ([x] (parse-uri x nil))
     ([x _]
      (if (string? x)
        (URI. x)
        x))))

(defn type->sym [x]
  (cond (int? x)     `integer?
        (float? x)   `float?
        (boolean? x) `boolean?   ;; pointless but valid
        ;;(symbol? x)  `symbol?  ;; doesn't work.
        ;;(ident? x)   `ident?   ;; doesn't work.
        (string? x)  `string?
        (keyword? x) `keyword?
        (uuid? x)    `uuid?
        (nil? x)     `nil?       ;; even more pointless but stil valid

        ;;#?(:clj (uri? x))     #?(:clj `uri?) ;; doesn't work.
        #?(:clj (decimal? x)) #?(:clj `decimal?)))

(defn spec-is-homogeneous-set? [x]
  "If the spec is given as a set, and every member of the set is the same type,
  then we can infer a coercion from that shared type."
  (when (set? x)
    (let [x0 (first x)
          t (type x0)]
      (reduce (fn [_ y]
                (or (= t (type y))
                    (reduced false)))
              true
              x))))

(defmulti sym->coercer
  (fn [x]
    (cond (spec-is-homogeneous-set? x)
          (-> x first type->sym)
          (sequential? x)  (first x)
          :else            x)))

(defn passthrough-parser
  [x _]
  x)

(defmethod sym->coercer `string? [_] parse-string)
(defmethod sym->coercer `number? [_] parse-double)
(defmethod sym->coercer `integer? [_] parse-long)
(defmethod sym->coercer `int? [_] parse-long)
(defmethod sym->coercer `pos-int? [_] parse-long)
(defmethod sym->coercer `neg-int? [_] parse-long)
(defmethod sym->coercer `nat-int? [_] parse-long)
(defmethod sym->coercer `even? [_] parse-long)
(defmethod sym->coercer `odd? [_] parse-long)
(defmethod sym->coercer `float? [_] parse-double)
(defmethod sym->coercer `double? [_] parse-double)
(defmethod sym->coercer `boolean? [_] parse-boolean)
(defmethod sym->coercer `ident? [_] parse-ident)
(defmethod sym->coercer `simple-ident? [_] parse-ident)
(defmethod sym->coercer `qualified-ident? [_] parse-ident)
(defmethod sym->coercer `keyword? [_] parse-keyword)
(defmethod sym->coercer `simple-keyword? [_] parse-keyword)
(defmethod sym->coercer `qualified-keyword? [_] parse-keyword)
(defmethod sym->coercer `symbol? [_] parse-symbol)
(defmethod sym->coercer `simple-symbol? [_] parse-symbol)
(defmethod sym->coercer `qualified-symbol? [_] parse-symbol)
(defmethod sym->coercer `uuid? [_] parse-uuid)
(defmethod sym->coercer `inst? [_] parse-inst)
(defmethod sym->coercer `nil? [_] parse-nil)
(defmethod sym->coercer `false? [_] parse-boolean)
(defmethod sym->coercer `true? [_] parse-boolean)
(defmethod sym->coercer `zero? [_] parse-long)
(defmethod sym->coercer `s/or [form] (parse-or form))
(defmethod sym->coercer `s/coll-of [form] (parse-coll-of form))
(defmethod sym->coercer `s/map-of [form] (parse-map-of form))
(defmethod sym->coercer `s/tuple [form] (parse-tuple form))
(defmethod sym->coercer `s/multi-spec [form] (parse-multi-spec form))

#?(:clj (defmethod sym->coercer `uri? [_] parse-uri))
#?(:clj (defmethod sym->coercer `decimal? [_] parse-decimal))

(defmethod sym->coercer :default [_] passthrough-parser)

(defn- keys-parser
  [[_ & {:keys [req-un opt-un]}]]
  (let [unnest (comp (filter keyword?)
                     (map #(vector (keyword (name %)) %)))
        keys-mapping (into {} unnest (flatten (concat req-un opt-un)))]
    (fn [x opts]
      (cond->> x
        (associative? x)
        (reduce-kv (fn [m k v]
                     (assoc m
                            k
                            (coerce (or (keys-mapping k) k)
                                    v
                                    opts)))
                   (empty x))))))

(defmethod sym->coercer `s/keys
  [form]
  (keys-parser form))

(defn parse-merge
  [[_ & pred-forms]]
  (fn [x opts]
    (if (associative? x)
      (reduce (fn [m pred-form]
                ;; for every pred-form coerce to new value;
                ;; we need to compare key by key what changed so that
                ;; defaults do not overwrite coerced values
                (into m
                      (keep (fn [[k v]]
                              (let [new-val (coerce k v opts)]
                                ;; new-val doesn't match default, keep it
                                (when-not (= (get x k) new-val)
                                  [k new-val]))))
                      (coerce pred-form x opts)))
              x
              pred-forms)
      x)))

(defmethod sym->coercer `s/merge [form]
  (parse-merge form))

(defn nilable-spec? [spec]
  (and (seq? spec)
       (= `s/nilable (first spec))))

(defn pull-nilable [spec]
  (if (nilable-spec? spec)
    (second spec)
    spec))

(defn gen-nilable-coercer
  [coercer]
  (fn [x opts]
    (some-> x
            (parse-nil opts)
            (coercer opts))))

(defn spec->coercion [root-spec]
  (-> root-spec
      pull-nilable
      sym->coercer
      (cond-> (nilable-spec? root-spec)
        gen-nilable-coercer)))

(defn nilable-spec->coercion
  "Pulling out nilable so we can get a real function to get a coercer "
  [root-spec]
  (-> root-spec
      pull-nilable
      si/spec->root-sym
      sym->coercer
      (cond-> (nilable-spec? root-spec)
        gen-nilable-coercer)))

(defn infer-coercion
  "Infer a coercer function from a given spec."
  [k]
  (let [root-spec (si/spec->root-sym k)]
    (if (nilable-spec? root-spec)
      (nilable-spec->coercion root-spec)
      (spec->coercion root-spec))))

(defn coerce-fn
  "Get the coercing function from a given key. First it tries to lookup
  the coercion on the registry, otherwise try to infer from the
  specs. In case nothing is found, identity function is returned."
  ([k] (coerce-fn k {::overrides *overrides*}))
  ([k {::keys [overrides] :as opts}]
   (or (when (qualified-keyword? k)
         (si/registry-lookup (merge @registry-ref overrides) k))
       (infer-coercion k))))

(defn coerce
  "Coerce a value x using coercer k. This function will first try to
  use a coercer from the registry, otherwise it will try to infer a
  coercer from the spec with the same name. Coercion will only be
  tried if x is a string.  Returns original value in case a coercer
  can't be found."
  ([k x] (coerce k x {::overrides *overrides*}))
  ([k x opts]
   (if-let [coerce-fn (coerce-fn k opts)]
     (coerce-fn x opts)
     x)))

(defn coerce!
  "Like coerce, but will call s/assert on the result, making it throw an error if value
  doesn't comply after coercion."
  ([k x] (coerce! k x {::overrides *overrides*}))
  ([k x opts]
   (if (simple-keyword? k)
     x
     (let [coerced (coerce k x opts)]
       (if (s/valid? k coerced)
         coerced
         (throw (ex-info "Failed to coerce value" {:spec  k
                                                   :value x})))))))

(defn conform
  "Like coerce, and will call s/conform on the result."
  ([k x] (conform k x {::overrides *overrides*}))
  ([k x opts]
   (s/conform k (coerce k x opts))))

(defn ^:skip-wiki def-impl [k coerce-fn]
  (assert (and (ident? k) (namespace k)) "k must be namespaced keyword")
  (swap! registry-ref assoc k coerce-fn)
  k)

(s/fdef def-impl
  :args (s/cat :k qualified-keyword?
               :coercion ifn?)
  :ret any?)

(defmacro def
  "Given a namespace-qualified keyword, and a coerce function, makes an entry in the
  registry mapping k to the coerce function."
  [k coercion]
  `(def-impl '~k ~coercion))

(s/fdef def
  :args (s/cat :k qualified-keyword?
               :coercion any?)
  :ret qualified-keyword?)

(defn coerce-structure
  "Recursively coerce map values on a structure."
  ([x] (coerce-structure x {}))
  ([x {::keys [overrides op]
       :or    {op coerce}
       :as    opts}]
   (walk/prewalk (fn [x]
                   (cond->> x
                     (map? x)
                     (into (empty x)
                           (map (fn [[k v]]
                                  (let [coercion (get overrides k k)]
                                    [k (op coercion v opts)]))))))
                 x)))

(s/def ::foo (s/nilable string?))
(coerce ::foo nil)
(coerce ::foo "")

(s/def ::nilable (s/nilable number?))
(coerce ::nilable "42") ; => 42
(coerce ::nilable "nil") ; => nil
(coerce ::nilable "foo") ; => "foo"
(coerce ::nilable nil)
