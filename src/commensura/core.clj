;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.core
  "Public verbs for combining and converting quantities. All are plain vars
  (autocomplete-friendly, no clojure.core shadowing) and interoperate with plain
  numbers as dimensionless scalars.

    (require '[commensura.units :as u]
             '[commensura.core :refer [by per plus minus pow to ratio]])

    (to (by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons)
    ;=> 552960/77 gallon ≈ 7181.30 [volume]   ; str/println form; prn wraps it in a
    ;                                            #commensura/quantity tagged literal

  Comparison verbs — the everyday `lt?`/`le?`/`gt?`/`ge?`/`eq?`/`ne?` (a physical,
  dimension-checked order that throws on overlapping intervals) plus the interval-aware
  `certainly-…?`/`possibly-…?` operators — order quantities by base magnitude, so
  `(eq? (u/inch 12) (u/foot 1))` is true.

  The verbs also accept Intervals (see commensura.interval); an interval accessor treats a
  bare quantity/number as a degenerate interval, so no scalar promotion is needed."
  (:require [commensura.quantity :as q]
            [commensura.interval :as iv]
            [commensura.registry :as registry]
            [clojurewerkz.money.amounts :as ma]
            [clojurewerkz.money.currencies :as mc])
  (:import [org.joda.money CurrencyUnit IllegalCurrencyException]
           [java.math RoundingMode]))

(defn by
  "Product of quantities/numbers/intervals (dimensions add). Variadic."
  ([x] x)
  ([x y] (if (or (iv/interval? x) (iv/interval? y)) (iv/iby x y) (q/qmul x y)))
  ([x y & more] (reduce by (by x y) more)))

(defn per
  "Quotient, left-associative: (per a b c) = a/b/c (dimensions subtract)."
  ([x] x)
  ([x y] (if (or (iv/interval? x) (iv/interval? y)) (iv/iper x y) (q/qdiv x y)))
  ([x y & more] (reduce per (per x y) more)))

(defn plus
  "Sum of same-dimension quantities/intervals. Variadic."
  ([x] x)
  ([x y] (if (or (iv/interval? x) (iv/interval? y)) (iv/iplus x y) (q/qadd x y)))
  ([x y & more] (reduce plus (plus x y) more)))

(defn minus
  "Difference of same-dimension quantities/intervals; unary form negates."
  ([x] (if (iv/interval? x) (iv/inegate x) (q/qmul (q/scalar -1) x)))
  ([x y] (if (or (iv/interval? x) (iv/interval? y)) (iv/iminus x y) (q/qsub x y)))
  ([x y & more] (reduce minus (minus x y) more)))

(defn pow
  "Raise a quantity/interval to an integer power."
  [x n] (if (iv/interval? x) (iv/ipow x n) (q/qpow x n)))

(defn to
  "Re-express a quantity/interval in a target unit (dimension-preserving)."
  [x target] (if (iv/interval? x) (iv/ito x target) (q/to x target)))

(defn ratio
  "Dimensionless count: how many of target fit in x (quantity or interval)."
  [x target] (if (iv/interval? x) (iv/iratio x target) (q/ratio x target)))

;; ---- comparison ----
;; A comparison orders values by the *range* each one spans. An interval spans [lo, hi]; a plain
;; quantity or number is a single point — its own low and high bound. `iv/lo-or-identity` /
;; `iv/hi-or-identity` read that range (an interval's bound, or the value itself), so the
;; operators below read as ordinary bound comparisons and handle a scalar with no special case.
;; `q/qcompare` does the ordering (conformance-checked, by base magnitude, approx-aware), so
;; every operator works on quantities, plain numbers, and intervals uniformly.

;; Certainly-* : the relation holds for *every* pair of values, one from x and one from y.
(defn certainly-lt?
  "x's high bound is below y's low bound: every value of x < every value of y."
  [x y]
  (neg? (q/qcompare (iv/hi-or-identity x) (iv/lo-or-identity y))))

(defn certainly-le?
  "x's high bound <= y's low bound: every value of x <= every value of y."
  [x y]
  (<= (q/qcompare (iv/hi-or-identity x) (iv/lo-or-identity y)) 0))

(defn certainly-gt?
  "x's low bound is above y's high bound: every value of x > every value of y."
  [x y]
  (pos? (q/qcompare (iv/lo-or-identity x) (iv/hi-or-identity y))))

(defn certainly-ge?
  "x's low bound >= y's high bound: every value of x >= every value of y."
  [x y]
  (>= (q/qcompare (iv/lo-or-identity x) (iv/hi-or-identity y)) 0))

(defn certainly-eq?
  "x and y are the same single point (both collapse to one shared value)."
  [x y]
  (and (zero? (q/qcompare (iv/lo-or-identity x) (iv/lo-or-identity y)))  ; cross-compare: enforces conformance
       (zero? (q/qcompare (iv/hi-or-identity x) (iv/hi-or-identity y)))
       (zero? (q/qcompare (iv/lo-or-identity x) (iv/hi-or-identity x))))) ; …and x is a point (so, with the above, is y)

(defn certainly-ne?
  "x and y are disjoint: their ranges share no value."
  [x y]
  (or (neg? (q/qcompare (iv/hi-or-identity x) (iv/lo-or-identity y)))
      (neg? (q/qcompare (iv/hi-or-identity y) (iv/lo-or-identity x)))))

;; Possibly-* : the relation holds for *some* pair. Frink's property — the possibly operator is
;; the negation of the opposite certainly operator — so each is correct by construction.
(defn possibly-lt?
  "Some value of x < some value of y."
  [x y]
  (not (certainly-ge? x y)))

(defn possibly-le?
  "Some value of x <= some value of y."
  [x y]
  (not (certainly-gt? x y)))

(defn possibly-gt?
  "Some value of x > some value of y."
  [x y]
  (not (certainly-le? x y)))

(defn possibly-ge?
  "Some value of x >= some value of y."
  [x y]
  (not (certainly-lt? x y)))

(defn possibly-eq?
  "x and y overlap: their ranges share a value."
  [x y]
  (not (certainly-ne? x y)))

(defn possibly-ne?
  "x and y are not a single shared point."
  [x y]
  (not (certainly-eq? x y)))

;; Plain relational : the everyday operators. They return the unambiguous answer, and throw on
;; overlapping intervals (Frink's rule). On quantities/numbers they never throw — a total order.
(defn- ambiguous! [op x y]
  (throw (ex-info (str "ambiguous " op " on overlapping intervals; use the certainly-/possibly- "
                       "comparison operators instead")
                  {:op op :x x :y y})))

(defn lt?
  "Less than; throws on overlapping intervals."
  [x y]
  (cond (certainly-lt? x y) true
        (certainly-ge? x y) false
        :else (ambiguous! "<" x y)))

(defn le?
  "Less-or-equal; throws on overlap."
  [x y]
  (cond (certainly-le? x y) true
        (certainly-gt? x y) false
        :else (ambiguous! "<=" x y)))

(defn gt?
  "Greater than; throws on overlap."
  [x y]
  (cond (certainly-gt? x y) true
        (certainly-le? x y) false
        :else (ambiguous! ">" x y)))

(defn ge?
  "Greater-or-equal; throws on overlap."
  [x y]
  (cond (certainly-ge? x y) true
        (certainly-lt? x y) false
        :else (ambiguous! ">=" x y)))

(defn eq?
  "Physical equality; throws on overlap."
  [x y]
  (cond (certainly-eq? x y) true
        (certainly-ne? x y) false
        :else (ambiguous! "=" x y)))

(defn ne?
  "Physical inequality; throws on overlap."
  [x y]
  (not (eq? x y)))

(defn register-dimension!
  "Give a human name to a dimension map, so quantities of that dimension print it
  in the trailing `[..]` slot (overriding any builtin). Returns the name.

    (register-dimension! {:length 4} \"quaternary space\")
    (pow u/meter 4)   ;=> 1 meter^4 ≈ 1.0 [quaternary space]"
  [dims nm]
  (registry/register-dimension! dims nm))

(defn register-unit-resolver!
  "Install a resolver for a *family* of units derivable from their name rather than registered one by
  one (the extension seam behind commensura's own historical currencies). `pred` is `name -> boolean`;
  `dispatch` is `name -> unit`, invoked when a name isn't in the unit table and `pred` matches. So a
  `#commensura/unit \"…\"` literal (and `to`/`ratio`) reify the whole family on demand. Resolvers are
  tried in registration order, first match wins. Complements `defunit` (a single fixed unit)."
  [pred dispatch]
  (registry/register-unit-resolver! pred dispatch))

(defmacro defunit
  "Define a callable `Unit` var. The bound value is the new unit name: it prints
  under its own name, scales when called, and serves as a `to`/`ratio` target.

  Four caller-facing forms — the 3-argument shape dispatches on whether its middle
  form is a number (⇒ literal) or not (⇒ docstring):
  - `(defunit sym expr)` — the everyday form: `expr` is any quantity/number
    expression; its magnitude and dimensions become the unit's.
  - `(defunit sym doc expr)` — as above, with a leading docstring on the var.
  - `(defunit sym magnitude dims)` — a literal base-SI magnitude and dimension map,
    with no evaluation; the form the generated builtins emit.
  - `(defunit sym doc magnitude dims)` — the literal form with a leading docstring
    (a generated builtin that carries a comment).

  Examples:

    (defunit beer (by (u/floz 12) (u/percent 3.2) (per u/water u/alcohol)))
    (beer 5)                                  ;=> 5 beer
    (to (by u/magnum (u/percent 13.5)) beer)  ;=> …how many beers in a magnum
    (ratio some-volume beer)                  ;=> …bare count of beers

    (defunit smoot \"Oliver Smoot's height (an MIT prank unit)\" (u/inch 67))

    (defunit foot 381/1250 {:length 1})       ; literal form
    (defunit gee \"standard gravity\" 196133/20000 {:length 1 :time -2})"
  ([sym expr] `(defunit ~sym nil ~expr))
  ([sym a b]
   (if (number? a)                                    ; (defunit sym magnitude dims)
     `(def ~sym (registry/register-unit! ~(str sym)
                  (vary-meta (q/unit ~(str sym) ~a ~b) assoc :ns (ns-name *ns*))))
     (let [doc (when (string? a) a)                   ; (defunit sym [doc] expr)
           vn  (gensym "val")
           un  (gensym "unit")]
       `(def ~sym ~@(when doc [doc])
          (let [~vn (q/scalar ~b)
                ~un (q/unit ~(str sym) (q/magnitude ~vn) (q/dims ~vn))]
            ;; carry the defining ns (and docstring, if any) as unit metadata — for discovery
            (registry/register-unit! ~(str sym)
              ~(if doc
                 `(vary-meta ~un assoc :ns (ns-name *ns*) :doc ~doc)
                 `(vary-meta ~un assoc :ns (ns-name *ns*)))))))))
  ([sym doc mag dims]                                 ; (defunit sym doc magnitude dims)
   `(def ~sym ~doc (registry/register-unit! ~(str sym)
                     (vary-meta (q/unit ~(str sym) ~mag ~dims) assoc :ns (ns-name *ns*) :doc ~doc)))))

;; ---- Money bridge (clojurewerkz/money · Joda-Money) ----------------------------------------------
;; Convert a currency quantity to a Money. This is the one verb that LEAVES commensura's exact tower:
;; Money is a fixed-scale decimal, so the amount is rounded to the currency's minor unit. We build the
;; Money from integer *minor units* (of-minor) — the scale-robust path across ISO currencies.

(def ^:private currency-aliases
  "commensura unit-name → ISO-4217 code, for builtin currency units whose name isn't the ISO code."
  {"dollar" "USD"})

(defn- currency-code
  "The ISO code `qty` is denominated in — its display unit's name (aliased, e.g. `dollar` → `USD`).
  Throws unless `qty` carries a currency dimension of exponent 1 (a single amount of money)."
  [qty]
  (when-not (= 1 (:currency (q/dims qty)))
    (throw (ex-info "->money expects a currency quantity (dimension :currency 1)"
                    {:dims (q/dims qty)})))
  (let [nm (:unit-name (first (q/formula qty)))]
    (get currency-aliases nm nm)))

(defn- currency-unit
  ^CurrencyUnit [code]
  (try (mc/for-code code)
       (catch IllegalCurrencyException _
         (throw (ex-info (str "not an ISO-4217 currency: " (pr-str code)
                              " — ->money needs an ISO currency (convert to one first, e.g. via `to`)")
                         {:code code})))))

(defn ->money
  "Convert a currency quantity to an `org.joda.money.Money` (via clojurewerkz/money). `qty` must have
  dimensions `{:currency 1}`; its ISO code is the display unit's name (`dollar` → `USD`). The amount is
  rounded to the currency's decimal places — `HALF_EVEN` by default, or pass a `java.math.RoundingMode`.

  This deliberately **leaves commensura's exact tower**: Money is a fixed-scale decimal, so an amount
  finer than the currency's minor unit is rounded. Throws on non-currency dimensions or a non-ISO code.

    (->money (cur/USD 19.99))                       ;=> USD 19.99
    (->money (cur/USD 19.995))                      ;=> USD 20.00   (HALF_EVEN)
    (->money (cur/USD 19.995) RoundingMode/FLOOR)   ;=> USD 19.99"
  (^org.joda.money.Money [qty] (->money qty RoundingMode/HALF_EVEN))
  (^org.joda.money.Money [qty ^RoundingMode mode]
   (let [cu     (currency-unit (currency-code qty))
         dp     (int (.getDecimalPlaces cu))
         amount (q/display-value qty)                         ; exact Ratio/integer coefficient
         n      (if (ratio? amount) (numerator amount) amount)
         d      (if (ratio? amount) (denominator amount) 1)
         scaled (.divide (bigdec n) (bigdec d) dp mode)       ; exact rational → BigDecimal at scale
         minor  (parse-long (str (.movePointRight scaled dp)))] ; whole minor units (cents, …)
     (ma/of-minor cu minor))))
