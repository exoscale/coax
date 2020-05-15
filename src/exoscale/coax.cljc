(ns exoscale.coax
  (:refer-clojure :exclude [def])
  (:require [exoscale.coax.inspect :as si]
            [exoscale.coax.parser :as p]
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

;;; form parsers

(defn gen-parse-or [[_ & pairs]]
  (fn [x opts]
    (reduce (fn [x [_ pred]]
              (let [coerced (coerce pred x opts)]
                (if (= x coerced)
                  x
                  (reduced coerced))))
            x
            (partition 2 pairs))))

(defn gen-parse-keys
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

(defonce ^:no-doc registry
  (atom {::form
         {`s/or gen-parse-or
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
          `zero? p/parse-long}}))

#?(:clj (swap! registry
               update ::ident
               assoc
               `uri? p/parse-uri
               `decimal? p/parse-decimal))

(defn set-type
  "Could be a protocol"
  [x]
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

(defn homogeneous-set? [x]
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
  [x opts]
  (let [reg (-> @registry
                (update ::ident merge (::overrides opts))
                (update ::form merge (::form-overrides opts)))]
    (or (cond (qualified-ident? x)
              (get-in reg [::ident x])

              (homogeneous-set? x)
              (get-in reg [::ident (set-type (first x))])

              (sequential? x)
              ((get-in reg
                       [::form (first x)])
               x))
        p/identity-parser)))

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
    (some-> x (coercer opts))))

(defn spec->coercion [root-spec opts]
  (-> root-spec
      pull-nilable
      (find-coercer opts)
      (cond-> (nilable-spec? root-spec)
        gen-nilable-coercer)))

(defn nilable-spec->coercion
  "Pulling out nilable so we can get a real function to get a coercer "
  [root-spec opts]
  (-> root-spec
      pull-nilable
      si/spec->root-sym
      (find-coercer opts)
      (cond-> (nilable-spec? root-spec)
        gen-nilable-coercer)))

(defn infer-coercion
  "Infer a coercer function from a given spec."
  [k opts]
  (let [root-spec (si/spec->root-sym k)]
    (if (nilable-spec? root-spec)
      (nilable-spec->coercion root-spec opts)
      (spec->coercion root-spec opts))))

(defn coerce-fn
  "Get the coercing function from a given key. First it tries to lookup
  the coercion on the registry, otherwise try to infer from the
  specs. In case nothing is found, identity function is returned."
  ([k] (coerce-fn k {}))
  ([k {::keys [overrides] :as opts}]
   (or (when (qualified-keyword? k)
         (si/registry-lookup (merge (::ident @registry)
                                    overrides) k))
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
  ([x {::keys [overrides op]
       :or {op coerce}
       :as opts}]
   (walk/prewalk (fn [x]
                   (cond->> x
                     (map? x)
                     (into (empty x)
                           (map (fn [[k v]]
                                  (let [coercion (get overrides k k)]
                                    [k (op coercion v opts)]))))))
                 x)))
