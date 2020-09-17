(ns exoscale.coax.inspect
  (:require [clojure.spec.alpha :as s]))

(defn- accept-keyword [x]
  (when (qualified-keyword? x)
    x))

(defn- accept-symbol [x]
  (when (qualified-symbol? x)
    x))

(defn- accept-set [x]
  (when (set? x)
    x))

(defn- accept-symbol-call [spec]
  (when (and (seq? spec)
             (symbol? (first spec)))
    spec))

(defn spec-form
  "Return the spec form or nil."
  [spec]
  (some-> spec s/get-spec s/form))

(defn spec-root
  "Determine the main spec root from a spec form."
  [spec]
  (let [spec-def (or (spec-form spec)
                     (accept-symbol spec)
                     (accept-symbol-call spec)
                     (accept-set spec))]
    (cond-> spec-def
      (qualified-keyword? spec-def)
      recur)))

(defn parent-spec
  "Look up for the parent coercer using the spec hierarchy."
  [k]
  (or (accept-keyword (s/get-spec k))
      (accept-keyword (spec-form k))))

(s/fdef registry-lookup
  :args (s/cat :registry map? :k qualified-keyword?)
  :ret any?)
(defn registry-lookup
  "Look for the key in registry, if not found try key spec parent recursively."
  [registry k]
  (if-let [c (get registry k)]
    c
    (when-let [parent (-> (parent-spec k) accept-keyword)]
      (recur registry parent))))
