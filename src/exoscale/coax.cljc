(ns exoscale.coax
  (:refer-clojure :exclude [def])
  (:require [exoscale.coax.inspect :as si]
            [exoscale.coax.coercer :as c]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk])
  #?(:clj
     (:import (clojure.lang Keyword)
              (java.util Date UUID)
              (java.time Instant)
              (java.net URI))))

(declare coerce coerce*)

(defn gen-coerce-or [[_ & pairs]]
  (fn [x opts]
    (let [xs (into []
                   (comp (partition-all 2)
                         (map #(coerce* (second %) x opts))
                         (remove #{:exoscale.coax/invalid}))
                   pairs)]
      ;; return first val that's either matching input or not invalid
      (or (reduce (fn [_ x']
                    (when (= x x')
                      (reduced x)))
                  nil
                  xs)
          (first xs)))))

(defn gen-coerce-and [[_ & [spec]]]
  (fn [x opts]
    (coerce spec x opts)))

(defn gen-coerce-keys
  [[_ & {:keys [req-un opt-un]}]]
  (let [keys-mapping (into {}
                           (comp (filter keyword?)
                                 (map #(vector (keyword (name %)) %)))
                           (flatten (concat req-un opt-un)))]
    (fn [x opts]
      (if (associative? x)
        (reduce-kv (fn [m k v]
                     (assoc m
                            k
                            (let [s (or (keys-mapping k) k)]
                              ;; only try to coerce registered specs
                              ;; from mapping
                              (if (qualified-ident? s)
                                (coerce s
                                        v
                                        opts)
                                v))))
                   (empty x)
                   x)
        :exoscale.coax/invalid))))

(defn gen-coerce-coll-of [[_ spec & {:as _opts :keys [kind]}]]
  (fn [x opts]
    (if (sequential? x)
      ;; either we have a `:kind` and coerce to that, or we
      ;; just `empty` the original
      (let [xs (into (condp = kind
                       `vector? []
                       `set? #{}
                       `coll? '()
                       `list? '()
                       ;; else
                       (empty x))
                     (map #(coerce spec % opts))
                     x)]
        (cond-> xs (list? xs)
                reverse))
      :exoscale.coax/invalid)))

(defn gen-coerce-map-of [[_ kspec vspec & _]]
  (fn [x opts]
    (if (associative? x)
      (into (empty x) ;; ensure we copy meta
            (map (fn [[k v]]
                   [(coerce kspec k opts)
                    (coerce vspec v opts)]))
            x)
      :exoscale.coax/invalid)))

(defn gen-coerce-tuple [[_ & specs]]
  (fn [x opts]
    (if (sequential? x)
      (mapv #(coerce %1 %2 opts)
            specs
            x)
      :exoscale.coax/invalid)))

(defn gen-coerce-multi-spec
  [[_ f retag & _ :as spec-expr]]
  (let [f #?(:clj (resolve f)
             ;; wall-hack, inspired by spec-tools internals until we
             ;; get a better way to do it
             :cljs (->> (s/registry)
                        vals
                        (filter #(= spec-expr (s/form %)))
                        first
                        .-mmvar))]
    (fn [x opts]
      (if (associative? x)
        (coerce (s/form (f x))
                x
                opts)
        :exoscale.coax/invalid))))

(defn gen-coerce-merge
  [[_ & spec-forms]]
  (fn [x opts]
    (if (associative? x)
      (reduce (fn [m spec-form]
                ;; for every spec-form coerce to new value;
                ;; we need to compare key by key what changed so that
                ;; defaults do not overwrite coerced values
                (into m
                      (keep (fn [[spec v]]
                              (let [new-val (if (qualified-ident? spec)
                                              (coerce spec v opts)
                                              v)]
                                ;; new-val doesn't match default, keep it
                                (when-not (= (get x spec) new-val)
                                  [spec new-val]))))
                      (coerce spec-form x opts)))
              x
              spec-forms)
      :exoscale.coax/invalid)))

(defn gen-coerce-nilable
  [[_ spec]]
  (fn [x opts]
    (when (some? x)
      (coerce spec x opts))))

(defprotocol EnumKey
  (enum-key [x]
    "takes enum value `x` and returns matching predicate to resolve
    coercer from registry"))

(defonce ^:private registry-ref
  (atom {:exoscale.coax/forms
         {`s/or gen-coerce-or
          `s/and gen-coerce-and
          `s/nilable gen-coerce-nilable
          `s/coll-of gen-coerce-coll-of
          `s/every gen-coerce-coll-of
          `s/map-of gen-coerce-map-of
          `s/every-kv gen-coerce-map-of
          `s/tuple gen-coerce-tuple
          `s/multi-spec gen-coerce-multi-spec
          `s/keys gen-coerce-keys
          `s/merge gen-coerce-merge
          `s/inst-in (constantly c/to-inst)
          `s/int-in (constantly c/to-long)
          `s/double-in (constantly c/to-double)}
         :exoscale.coax/idents
         {`string? c/to-string
          `number? c/to-number
          `integer? c/to-long
          `int? c/to-long
          `pos-int? c/to-long
          `neg-int? c/to-long
          `nat-int? c/to-long
          `even? c/to-long
          `odd? c/to-long
          `float? c/to-double
          `double? c/to-double
          `boolean? c/to-boolean
          `ident? c/to-ident
          `simple-ident? c/to-ident
          `qualified-ident? c/to-ident
          `keyword? c/to-keyword
          `simple-keyword? c/to-keyword
          `qualified-keyword? c/to-keyword
          `symbol? c/to-symbol
          `simple-symbol? c/to-symbol
          `qualified-symbol? c/to-symbol
          `uuid? c/to-uuid
          `inst? c/to-inst
          `false? c/to-boolean
          `true? c/to-boolean
          `zero? c/to-long}
         :exoscale.coax/enums #'enum-key}))

(defn registry
  "returns the registry map, prefer 'get-spec' to lookup a spec by name"
  []
  @registry-ref)

#?(:clj (swap! registry-ref
               update :exoscale.coax/idents
               assoc
               `uri? c/to-uri
               `decimal? c/to-decimal))

(extend-protocol EnumKey
  #?(:clj Number :cljs number)
  (enum-key [x] `number?)

  #?(:clj Double :cljs double)
  (enum-key [x] `double?)

  #?(:clj String :cljs string)
  (enum-key [x] `string?)

  #?(:clj Boolean :cljs boolean)
  (enum-key [x] `boolean?)

  Keyword
  (enum-key [x] `keyword?)

  UUID
  (enum-key [x] `uuid?)

  nil
  (enum-key [x] `nil?)

  #?(:clj Object :cljs default)
  (enum-key [x] nil))

#?(:clj
   (extend-protocol EnumKey
     Float
     (enum-key [x] `float?)
     Long
     (enum-key [x] `int?)
     Instant
     (enum-key [x] `inst?)
     Date
     (enum-key [x] `inst?)
     URI
     (enum-key [x] `uri?)
     BigDecimal
     (enum-key [x] `decimal?)))

(defn enum?
  "If the spec is given as a set, and every member of the set is the same type,
  then we can infer a coercion from that shared type."
  [x]
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
  First looking at :exoscale.coax/idents if value is a
  qualified-keyword or qualified symbol, or checking if the value is
  an enum value (homogeneous set) and lastly if it's a s-exp form that
  indicates a spec form likely it will return it's generated coercer
  from registry :exoscale.coax/form , otherwise the it returns the
  identity coercer"
  [spec-exp {:as opts :exoscale.coax/keys [enums]}]
  (let [{:as reg :exoscale.coax/keys [idents]} (-> @registry-ref
                                                   (update :exoscale.coax/idents
                                                           merge
                                                           (:exoscale.coax/idents opts))
                                                   (update :exoscale.coax/forms
                                                           merge
                                                           (:exoscale.coax/forms opts))
                                                   (cond-> enums
                                                     (assoc :exoscale.coax/enums enums)))]
    (or (cond (qualified-ident? spec-exp)
              (get idents spec-exp)

              (enum? spec-exp)
              (when-let [f (:exoscale.coax/enums reg)]
                (get idents (f (first spec-exp))))

              (sequential? spec-exp)
              (when-let [f (get-in reg [:exoscale.coax/forms (first spec-exp)])]
                (f spec-exp)))
        c/identity)))

(defn coerce-fn
  "Get the coercing function from a given key. First it tries to lookup
  the coercion on the registry, otherwise try to infer from the
  specs. In case nothing is found, identity function is returned."
  ([spec] (coerce-fn spec {}))
  ([spec {:exoscale.coax/keys [idents] :as opts}]
   (or (when (qualified-keyword? spec)
         (si/registry-lookup (merge (:exoscale.coax/idents @registry-ref)
                                    idents)
                             spec))
       (find-coercer (si/spec-root spec)
                     opts))))

(defn coerce*
  "Like coerce, but if it can't find a way to coerce the original value
  will return `:exoscale.coax/invalid`. Mostly useful for
  implementation of special forms like s/or."
  [spec x opts]
  (if-let [coerce-fn (coerce-fn spec opts)]
    (coerce-fn x opts)
    x))

(s/def ::opts map?)
(s/def ::symbolic-spec (s/or :spec-sym symbol?
                             :spec-sym-form (s/cat :h symbol?
                                                   :more (s/* any?))))
(s/def ::reg-spec qualified-keyword?)
(s/def ::spec (s/or :reg-spec ::reg-spec
                    :symbolic-spec ::symbolic-spec
                    :set-spec set?))

(s/fdef coerce
  :args (s/cat :spec ::spec
               :x any?
               :opts (s/? (s/nilable ::opts))))
(defn coerce
  "Coerce a value `x` using spec/coercer `spec`. This function will
  first try to use a coercer from the registry, otherwise it will try
  to infer a coercer from the spec with the same name. Returns
  original value in case a coercer can't be found."
  ([spec x] (coerce spec x {}))
  ([spec x opts]
   (let [x' (coerce* spec x opts)]
     (if (= :exoscale.coax/invalid x')
       x
       x'))))

(s/fdef coerce!
  :args (s/cat :spec ::reg-spec
               :x any?
               :opts (s/? (s/nilable ::opts))))
(defn coerce!
  "Like coerce, but will call s/assert on the result, making it throw an
  error if value doesn't comply after coercion. Only works with
  registered specs"
  ([spec x] (coerce! spec x {}))
  ([spec x opts]
   (let [coerced (coerce spec x opts)]
     (if (s/valid? spec coerced)
       coerced
       (throw (ex-info "Invalid coerced value"
                       {:type :exoscale.coax/invalid-coerced-value
                        :val x
                        :coerced coerced
                        :spec spec}))))))

(s/fdef conform
  :args (s/cat :spec ::reg-spec
               :x any?
               :opts (s/? (s/nilable ::opts))))
(defn conform
  "Like coerce, and will call s/conform on the result. Only works with
  registered specs"
  ([spec x] (conform spec x {}))
  ([spec x opts]
   (s/conform spec (coerce spec x opts))))

(defn ^:no-doc def-impl
  [k coerce-fn]
  (swap! registry-ref assoc-in [:exoscale.coax/idents k] coerce-fn)
  k)

(s/fdef def
  :args (s/cat :k ::reg-spec
               :coercion any?)
  :ret qualified-keyword?)
(defmacro def
  "Given a namespace-qualified keyword, and a coerce function, makes an
  entry in the registry mapping k to the coerce function."
  [k coercion]
  `(def-impl '~k ~coercion))

(s/fdef coerce-structure
  :args (s/cat :x any?
               :opts (s/? (s/nilable ::opts))))
(defn coerce-structure
  "Recursively coerce map values on a structure."
  ([x] (coerce-structure x {}))
  ([x {:exoscale.coax/keys [idents op]
       :or {op coerce}
       :as opts}]
   (walk/prewalk (fn [x]
                   (cond->> x
                     (map? x)
                     (into (empty x)
                           (map (fn [[k v]]
                                  (if (qualified-keyword? k)
                                    [k (op (get idents k k) v opts)]
                                    [k v]))))))
                 x)))
