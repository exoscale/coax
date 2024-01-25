# coax

Clojure.spec coercion library for clj(s)

Most Coax "public" functions take a spec and a value and try to return a new
value that conforms to that spec by altering the input value when possible and
most importantly when it makes sense.

Coax is centered around its own registry for coercion rules, when a coercion is
not registered it can infer in most cases what to do to coerce a value into
something that conforms to a spec. It also supports "coerce time" options to
enable custom coercion from any spec type, including spec `forms` (like
s/coll-of & co) or just `idents` (predicates, registered specs).

Coax initially started as a fork of spec-coerce, but nowadays the internals and
the API are totally different. Wilker Lúcio's approach gave us a nice outline of
how such library could expose its functionality.

## What

The typical (inferred) example would be :

```clj
(s/def ::foo keyword?)
(c/coerce ::foo "bar") -> :bar
```

### registering coercers

You can register a coercer per spec if needed

```clj
(s/def ::foo string?)
(c/def ::foo (fn [x opts] (str "from-registry: " x)))
(c/coerce ::foo "bar") -> "from-registry: bar"

```

### Overrides

Overrides allow to change the defaults, essentially all the internal
conversion rules are open via the options to `coerce`, they will be
merged with the internal registry at coerce time.

```clj
(s/def ::foo keyword?)
(c/coerce ::foo "bar" {:idents {::foo (fn [x opts] (str "keyword:" x))}}) -> "keyword:bar"
```

Coercers are functions of 2 args, the value, and the options coerce
received. They return either a coerced value or
`:exoscale.coax/invalid`, which indicates we didn't know how to coerce
that value, in which case s/coerce will set the value to the input.

Overrides also works on any qualified-ident (registered specs or
symbols/fns), which is something spec-coerce cannot do currently.

The typical example would be :

```clj
(s/def ::foo (s/coll-of keyword?))
;; we'll namespace all keywords in that coll-of
(c/coerce ::foo ["a" "b"] {:idents {`keyword? (fn [x opts] (keyword "foo" x))}}) -> [foo/a foo/b]
```

You can specify multiple overrides per coerce call.

Another thing we added is the ability to reach and change the
behaviour of coercer generators via ::c/forms, essentially allowing
you to support any spec form like inst-in, coll-of, .... You could
easily for instance generate open-api definitions using these.

```clj
(c/coerce ::foo (s/coll-of keyword?)
          {:forms {`s/coll-of (fn [[_ spec]] (fn [x opts] (do-something-crazy-with-spec+the-value spec x opts)))}})
```

### Closed maps

`coax` also allows to *close* maps specced with `s/keys`. 

If you call `coerce` using the option `{:exoscale.coax/closed true ...}` if a value
corresponding to a `s/keys` spec is encountered it will effectively remove all
unknown keys from the returned value.

``` clj
(s/def ::foo string?)
(s/def ::bar string?)
(s/def ::z string?)

(s/def ::m (s/keys :req-un [::foo ::bar]))

(c/coerce ::m {:foo "f" :bar "b" :baz "z"} {:closed true}) ; baz is not on the spec
-> {:foo "f" :bar "b"} ; it gets removed

;; this works in any s/keys matching spec in the data passed:
(coerce `(s/coll-of ::m) [{:foo "f" :bar "b" :baz "x"}] {:closed true})
-> [{:foo "f" :bar "b"}]

;; also plays nice with s/merge, multi-spec & co
(coerce `(s/merge ::m (s/keys :req-un [::z]))
        {:foo "f" :bar "b" :baz "x" :z "z"} {:closed true}) 
        
-> {:foo "f" :bar "b" :z "z"}
```

## Documentation

[![cljdocbadge](https://cljdoc.xyz/badge/exoscale/coax)](https://cljdoc.org/d/exoscale/coax/CURRENT/api/exoscale.coax)

## Installation

coax is [available on Clojars](https://clojars.org/exoscale/coax).

Add this to your dependencies:

[![Clojars Project](https://img.shields.io/clojars/v/exoscale/coax.svg)](https://clojars.org/exoscale/coax)


## More Usage examples (taken directly from spec-coerce)

Learn by example:

```clojure
(ns exoscale.coax.example
  (:require
    [clojure.spec.alpha :as s]
    [exoscale.coax :as c]))

; Define a spec as usual
(s/def ::number int?)

; Call the coerce method passing the spec and the value to be coerced
(c/coerce ::number "42") ; => 42

; Like spec generators, when using `and` it will use the first item as the inference source
(s/def ::odd-number (s/and int? odd?))
(c/coerce ::odd-number "5") ; => 5

; When inferring the coercion, it tries to resolve the upmost spec in the definition
(s/def ::extended (s/and ::odd-number #(> % 10)))
(c/coerce ::extended "11") ; => 11

; Nilables are considered
(s/def ::nilable (s/nilable ::number))
(c/coerce ::nilable "42") ; => 42
(c/coerce ::nilable "foo") ; => "foo"

; The coercion can even be automatically inferred from specs given explicitly as sets of a homogeneous type
(s/def ::enum #{:a :b :c})
(c/coerce ::enum ":a") ; => :a

; If you wanna play around or use a specific coercion, you can pass the predicate symbol directly
(c/coerce `int? "40") ; => 40

; Parsers are written to be safe to call, when unable to coerce they will return the original value
(c/coerce `int? "40.2") ; => "40.2"
(c/coerce `inst? "date") ; => "date"

; To leverage map keys and coerce a composed structure, use coerce-structure
(c/coerce-structure {::number      "42"
                      ::not-defined "bla"
                      :sub          {::odd-number "45"}})
; => {::number      42
;     ::not-defined "bla"
;     :sub          {::odd-number 45}}

; coerce-structure supports overrides, so you can set a custom coercer for a specific context
(c/coerce-structure {::number      "42"
                      ::not-defined "bla"
                      :sub          {::odd-number "45"}}
                     {::c/idents {::not-defined `keyword?
; => {::number      42
;     ::not-defined :bla
;     :sub          {::odd-number 45}}

; If you want to set a custom coercer for a given spec, use the exoscale.coax registry
(defrecord SomeClass [x])
(s/def ::my-custom-attr #(instance? SomeClass %))
(c/def ::my-custom-attr #(map->SomeClass {:x %}))

; Custom registered keywords always takes precedence over inference
(c/coerce ::my-custom-attr "Z") ; => #user.SomeClass{:x "Z"}

(c/coerce ::my-custom-attr "Z") {::c/idents {::my-custom-attr keyword}}) ; => :Z
```

Examples from predicate to coerced value:

```clojure
; Numbers
(c/coerce `number? "42")                                   ; => 42.0
(c/coerce `integer? "42")                                  ; => 42
(c/coerce `int? "42")                                      ; => 42
(c/coerce `pos-int? "42")                                  ; => 42
(c/coerce `neg-int? "-42")                                 ; => -42
(c/coerce `nat-int? "10")                                  ; => 10
(c/coerce `even? "10")                                     ; => 10
(c/coerce `odd? "9")                                       ; => 9
(c/coerce `float? "42.42")                                 ; => 42.42
(c/coerce `double? "42.42")                                ; => 42.42
(c/coerce `zero? "0")                                      ; => 0

; Numbers on CLJS
(c/coerce `int? "NaN")                                     ; => js/NaN
(c/coerce `double? "NaN")                                  ; => js/NaN

; Booleans
(c/coerce `boolean? "true")                                ; => true
(c/coerce `boolean? "false")                               ; => false
(c/coerce `true? "true")                                   ; => true
(c/coerce `false? "false")                                 ; => false

; Idents
(c/coerce `ident? ":foo/bar")                              ; => :foo/bar
(c/coerce `ident? "foo/bar")                               ; => 'foo/bar
(c/coerce `simple-ident? ":foo")                           ; => :foo
(c/coerce `qualified-ident? ":foo/baz")                    ; => :foo/baz
(c/coerce `keyword? "keyword")                             ; => :keyword
(c/coerce `keyword? ":keyword")                            ; => :keyword
(c/coerce `simple-keyword? ":simple-keyword")              ; => :simple-keyword
(c/coerce `qualified-keyword? ":qualified/keyword")        ; => :qualified/keyword
(c/coerce `symbol? "sym")                                  ; => 'sym
(c/coerce `simple-symbol? "simple-sym")                    ; => 'simple-sym
(c/coerce `qualified-symbol? "qualified/sym")              ; => 'qualified/sym

; Collections
(c/coerce `(s/coll-of int?) ["5" "11" "42"])               ; => [5 11 42]
(c/coerce `(s/coll-of int?) ["5" "11.3" "42"])             ; => [5 "11.3" 42]
(c/coerce `(s/map-of keyword? int?) {"foo" "42" "bar" "31"})
; => {:foo 42 :bar 31}

; Branching
; tests are realized in order
(c/coerce `(s/or :int int? :bool boolean?) "40")           ; 40
(c/coerce `(s/or :int int? :bool boolean?) "true")         ; true
; returns original value when no options can handle
(c/coerce `(s/or :int int? :bool boolean?) "foo")          ; "foo"

; Tuple
(c/coerce `(s/tuple int? string?) ["0" 1])                 ; => [0 "1"]

; Others
(c/coerce `uuid? "d6e73cc5-95bc-496a-951c-87f11af0d839")   ; => #uuid "d6e73cc5-95bc-496a-951c-87f11af0d839"
(c/coerce `inst? "2017-07-21")                             ; => #inst "2017-07-21T00:00:00.000000000-00:00"
(c/coerce `nil? "foo")                                     ; => "foo"
(c/coerce `nil? nil)                                       ; => nil

;; Clojure only:
(c/coerce `uri? "http://site.com") ; => (URI. "http://site.com")
(c/coerce `decimal? "42.42") ; => 42.42M
(c/coerce `decimal? "42.42M") ; => 42.42M

;; Throw exception when coercion fails
(c/coerce! ::number "abc") ; => throws (ex-info "Failed to coerce value" {:spec ::number :val "abc" ...})
(c/coerce! :simple-keyword "abc") ; => "abc", coerce! doesn't do anything on simple keywords

;; Conform the result after coerce
(c/conform ::number "40")          ; 40

;; Throw on coerce structure
(c/coerce-structure {::number "42"} {::c/op c/coerce!})

;; Conform on coerce structure
(c/coerce-structure {::number "42"} {::c/op c/conform})
```

## Caching

Coax applies caching of coercers function to cut the cost of walking
specs and generating coercers per call, it makes the coercion process
orders of magnitude faster once cached (depends on what the coercion
does of course). It is `on` by default. The cache is under
`exoscale.coax/coercer-cache`, it's just an atom holding a map of
`[spec, options] -> coercer`. In most cases you shouldn't have to care
about this, for instance when you define static coercers via
`coax/def` we'll make sure the cache is updated accordingly. But
during development you might need to be aware of the existence of that
cache (ex if you defined a bugged coercer, or while doing REPL dev).

In any case you can turn off the cache by passing
`:exoscale.coax/cache false` to the options of
coerce/conform/coerce-structure, alternatively you can manually fiddle
with the cache under `exoscale.coax/coercer-cache`, for instance via
`(reset! exoscale.coax/coercer-cache {})`.

## License

* License Copyright © 2020 Exoscale - Distributed under ISC License

* [spec-coerce original license](https://github.com/wilkerlucio/spec-coerce/blob/master/LICENSE)
  Copyright © 2017 Wilker Lúcio -  Distributed under the MIT License.
