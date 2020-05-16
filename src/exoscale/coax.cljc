(ns exoscale.coax
  (:refer-clojure :exclude [def])
  (:require [exoscale.coax.inspect :as si]
            [exoscale.coax.parser :as p]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk])
  (:import (clojure.lang Keyword)
           #?@(:clj
               ((java.util Date UUID)
                (java.time Instant)
                (java.net URI)
                (java.time.format DateTimeFormatter)))))

(declare coerce)

(defn gen-parse-or [[_ & pairs]]
  (fn [x opts]
    (reduce (fn [x [_ pred]]
              (let [coerced (coerce pred x opts)]
                (if (= x coerced)
                  x
                  (reduced coerced))))
            x
            (partition 2 pairs))))

(defn gen-parse-and [[_ & [pred]]]
  (fn [x opts]
    (coerce pred x opts)))

(defn gen-parse-keys
  [[_ & {:keys [req-un opt-un]}]]
  (let [keys-mapping (into {}
                           (comp (filter keyword?)
                                 (map #(vector (keyword (name %)) %)))
                           (flatten (concat req-un opt-un)))]
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

(defn- map-seq
  [fun x]
  (if (vector? x)
    (mapv fun x)
    (map fun x)))

(defn gen-parse-coll-of [[_ pred & _]]
  (fn [x opts]
    (cond->> x
      (sequential? x)
      (map-seq #(coerce pred % opts)))))

(defn gen-parse-map-of [[_ kpred vpred & _]]
  (fn [x opts]
    (cond->> x
      (associative? x)
      (into (empty x) ;; ensure we copy meta
            (map (fn [[k v]]
                   [(coerce kpred k opts)
                    (coerce vpred v opts)]))))))

(defn gen-parse-tuple [[_ & preds]]
  (fn [x opts]
    (cond->> x
      (sequential? x)
      (mapv #(coerce %1 %2 opts)
            preds))))

(defn gen-parse-multi-spec
  [[_ f retag & _]]
  (let [f (resolve f)]
    (fn [x opts]
      (if (associative? x)
        (coerce (s/form (f (retag x)))
                x
                opts)
        x))))

(defn gen-parse-merge
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

(defn gen-parse-nilable
  [[_ pred]]
  (fn [x opts]
    (when (some? x)
      (coerce pred x opts))))

(defprotocol EnumKey
  (enum-key [x]
    "takes enum value `x` and returns matching predicate to resolve
    parser from registry"))

(defonce ^:no-doc registry
  (atom {::form
         {`s/or gen-parse-or
          `s/and gen-parse-and
          `s/nilable gen-parse-nilable
          `s/coll-of gen-parse-coll-of
          `s/map-of gen-parse-map-of
          `s/tuple gen-parse-tuple
          `s/multi-spec gen-parse-multi-spec
          `s/keys gen-parse-keys
          `s/merge gen-parse-merge}
         ::ident
         {`string? p/parse-string
          `number? p/parse-double
          `integer? p/parse-long
          `int? p/parse-long
          `pos-int? p/parse-long
          `neg-int? p/parse-long
          `nat-int? p/parse-long
          `even? p/parse-long
          `odd? p/parse-long
          `float? p/parse-double
          `double? p/parse-double
          `boolean? p/parse-boolean
          `ident? p/parse-ident
          `simple-ident? p/parse-ident
          `qualified-ident? p/parse-ident
          `keyword? p/parse-keyword
          `simple-keyword? p/parse-keyword
          `qualified-keyword? p/parse-keyword
          `symbol? p/parse-symbol
          `simple-symbol? p/parse-symbol
          `qualified-symbol? p/parse-symbol
          `uuid? p/parse-uuid
          `inst? p/parse-inst
          `false? p/parse-boolean
          `true? p/parse-boolean
          `zero? p/parse-long}
         ::enum #'enum-key}))

;; @registry

#?(:clj (swap! registry
               update ::ident
               assoc
               `uri? p/parse-uri
               `decimal? p/parse-decimal))

(extend-protocol EnumKey
  Number
  (enum-key [x] `number?)

  Long
  (enum-key [x] `int?)

  Double
  (enum-key [x] `double?)

  Float
  (enum-key [x] `float?)

  String
  (enum-key [x] `string?)

  Boolean
  (enum-key [x] `boolean?)

  Keyword
  (enum-key [x] `keyword?)

  UUID
  (enum-key [x] `uuid?)

  nil
  (enum-key [x] `nil?)

  Object
  (enum-key [x] nil))

#?(:clj
   (extend-protocol EnumKey
     Instant
     (enum-key [x] `inst?)
     Date
     (enum-key [x] `inst?)
     URI
     (enum-key [x] `uri?)
     BigDecimal
     (enum-key [x] `decimal?)))

(defn enum? [x]
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

(defn find-coercer
  "Tries to find coercer by looking into registry.
  First looking at ::ident if value is a qualified-keyword or
  qualified symbol, or checking if the value is an enum
  value (homogeneous set) and lastly if it's a s-exp form that
  indicates a spec form likely it will return it's generated parser
  from registry ::form , otherwise the it returns the identity parser"
  [x {:as opts ::keys [enum]}]
  (let [{:as reg ::keys [ident]}
        (-> @registry
            (update ::ident merge (::ident opts))
            (update ::form merge (::form opts))
            (cond-> enum (assoc ::enum enum)))]
    (or (cond (qualified-ident? x)
              (get ident x)

              (enum? x)
              (get ident ((::enum reg) (first x)))

              (sequential? x)
              ((get-in reg [::form (first x)]) x))
        p/identity-parser)))

(defn infer-coercion
  "Infer a coercer function from a given spec."
  [k opts]
  (find-coercer (si/spec->root-sym k)
                root-spec opts))

(defn coerce-fn
  "Get the coercing function from a given key. First it tries to lookup
  the coercion on the registry, otherwise try to infer from the
  specs. In case nothing is found, identity function is returned."
  ([k] (coerce-fn k {}))
  ([k {::keys [ident] :as opts}]
   (or (when (qualified-keyword? k)
         (si/registry-lookup (merge (::ident @registry)
                                    ident) k))
       (infer-coercion k opts))))

(defn coerce
  "Coerce a value x using coercer k. This function will first try to
  use a coercer from the registry, otherwise it will try to infer a
  coercer from the spec with the same name. Coercion will only be
  tried if x is a string.  Returns original value in case a coercer
  can't be found."
  ([k x] (coerce k x {}))
  ([k x opts]
   (if-let [coerce-fn (coerce-fn k opts)]
     (coerce-fn x opts)
     x)))

(defn coerce!
  "Like coerce, but will call s/assert on the result, making it throw an error if value
  doesn't comply after coercion."
  ([k x] (coerce! k x {}))
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
  ([k x] (conform k x {}))
  ([k x opts]
   (s/conform k (coerce k x opts))))

(defn ^:skip-wiki def-impl [k coerce-fn]
  (assert (and (ident? k) (namespace k)) "k must be namespaced keyword")
  (swap! registry assoc-in [::ident k] coerce-fn)
  k)

(s/fdef def-impl
  :args (s/cat :k qualified-ident?
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
  ([x {::keys [ident op]
       :or {op coerce}
       :as opts}]
   (walk/prewalk (fn [x]
                   (cond->> x
                     (map? x)
                     (into (empty x)
                           (map (fn [[k v]]
                                  (let [coercion (get ident k k)]
                                    [k (op coercion v opts)]))))))
                 x)))
