(ns exoscale.coax.inspect
  (:require [clojure.spec.alpha :as s]))

(defn- accept-keyword [x]
  (when (qualified-keyword? x)
    x))

(defn- accept-symbol [x]
  (when
      (qualified-symbol? x) x))

(defn- accept-symbol-call [spec]
  (when (and (seq? spec)
             (symbol? (first spec)))
    spec))

(defn safe-form
  "Return the spec form or nil."
  [spec]
  (when (contains? (s/registry) spec)
    (s/form spec)))

(defn spec->root-sym
  "Determine the main spec symbol from a spec form."
  [spec]
  (let [spec-def (or (safe-form spec)
                     (accept-symbol spec)
                     (accept-symbol-call spec))]
    (cond-> spec-def
      (qualified-keyword? spec-def)
      recur)))

(defn parent-spec
  "Look up for the parent coercer using the spec hierarchy."
  [k]
  (or (-> (s/get-spec k) accept-keyword)
      (-> (safe-form k) accept-keyword)))

(defn registry-lookup
  "Look for the key in registry, if not found try key spec parent recursively."
  [registry k]
  (if-let [c (get registry k)]
    c
    (when-let [parent (-> (parent-spec k)
                          accept-keyword)]
      (recur registry parent))))

(s/fdef registry-lookup
  :args (s/cat :registry map? :k qualified-keyword?)
  :ret any?)
