;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.math
  "Math functions over quantities and intervals — the ones that *carry dimensions*.

    (require '[commensura.math :as m])
    (m/sqrt (c/pow (u/meter 1) 2))  ;=> 1 meter        (exact when a perfect root)
    (m/abs  (u/meter -5))           ;=> 5 meter
    (m/min  (u/foot 1) (u/inch 6))  ;=> 6 inch          (physical order, keeps the unit)
    (m/mod  (u/hour 25) (u/hour 24));=> 1 hour

  Only functions that touch dimensions live here — preserving them (`abs`/`mod`/`min`/`max`/
  `floor`/`ceil`/`round`), scaling them (`sqrt`/`root`/`pow`), or crossing the boundary (`sign`:
  dimensioned → dimensionless). Transcendentals (`exp`/`ln`/`sin`/…) are intentionally absent:
  they only ever map dimensionless → dimensionless, so they belong to plain numeric code.

  Over intervals, the monotone functions lift by mapping the endpoints — `sign`/`floor`/`ceil`/`round`
  directly, `abs` with a special case when the interval spans zero — and `sqrt`/`root`/`pow` scale
  through. `mod`/`rem` are the exception: **scalar-only, and they reject an interval argument**, because
  modular reduction is discontinuous and cannot be soundly lifted (`[23,25] mod 24` is `{23} ∪ [0,1]`,
  not a single interval). Names shadow `clojure.core`, so use this namespace qualified (`m/abs`), never
  `:refer`. Comparisons come from `commensura.core`."
  (:refer-clojure :exclude [abs min max mod rem])
  (:require [commensura.quantity :as q]
            [commensura.interval :as iv]
            [commensura.uncertain :as un]
            [commensura.core :as c])
  (:import [java.math RoundingMode]))

;; ---- roots & rational powers (dimensions scale; exact when a perfect root, else approx) ----
;; With provenance recording on, the value-producing fns here are `defstep`s (via
;; `commensura.core/defstep`), so each records as one node under its own `#'var` — `sqrt` shows as
;; `sqrt`, not the `pow` it calls underneath. `pow` stays a thin pass-through to `c/pow` (itself a step),
;; so it records as `core/pow`; `sign` isn't stepped (it returns a bare number, which can't carry a node).
(defn pow
  "Raise to an integer or rational exponent."
  [x n]
  (c/pow x n))

(c/defstep sqrt
  "Square root."
  [x]
  (c/pow x 1/2))

(c/defstep root
  "The q-th root."
  [x q]
  (c/pow x (/ 1 q)))

;; ---- shared helpers ----
;; Compared by *central* value, so an Uncertain is ordered by its estimate and returned whole (its σ
;; kept); a plain quantity/interval-bound passes through `value-or-identity` unchanged.
(defn- pick-lo [x y] (if (pos? (q/qcompare (un/value-or-identity x) (un/value-or-identity y))) y x))
(defn- pick-hi [x y] (if (neg? (q/qcompare (un/value-or-identity x) (un/value-or-identity y))) y x))

(defn- lift-monotone
  "Apply a monotone scalar function `f` to a value, or (for an interval) to each endpoint and the
  main value — `iv/interval` re-orders, so it works for increasing or decreasing `f`."
  [f x]
  (if (iv/interval? x)
    (if-let [m (iv/main-value x)]
      (iv/interval (f (iv/lo x)) (f m) (f (iv/hi x)))
      (iv/interval (f (iv/lo x)) (f (iv/hi x))))
    (f x)))

(defn- lift-uncertain
  "Apply a dimension-preserving scalar fn `f` to an uncertain's central value, keeping its spread."
  [f x]
  (un/plus-minus (f (un/value x)) (un/sigma x)))

(defn- lift
  "Like `lift-monotone`, but an Uncertain keeps its spread while `f` maps its central value."
  [f x]
  (if (un/uncertain? x) (lift-uncertain f x) (lift-monotone f x)))

;; ---- abs (dimension-preserving; zero-spanning intervals reach 0) ----
(defn- abs-scalar [x]
  (let [m (q/magnitude x)]
    (q/quantity (if (neg? m) (- m) m) (q/formula x))))

(c/defstep abs
  "Absolute value; dimension-preserving. Over a zero-spanning interval the lower bound is 0; on an
  Uncertain, |value| carries the spread unchanged."
  [x]
  (cond
    (un/uncertain? x) (lift-uncertain abs-scalar x)
    (not (iv/interval? x)) (abs-scalar x)
    :else
    (let [lo (iv/lo x), hi (iv/hi x)
          alo (abs-scalar lo), ahi (abs-scalar hi)
          spans? (and (not (pos? (q/magnitude lo))) (not (neg? (q/magnitude hi))))
          new-lo (if spans? (q/qmul (q/scalar 0) lo) (pick-lo alo ahi))
          new-hi (pick-hi alo ahi)]
      (if-let [m (iv/main-value x)]
        (iv/interval new-lo (abs-scalar m) new-hi)
        (iv/interval new-lo new-hi)))))

;; ---- sign (dimensioned → dimensionless -1/0/1) ----
(defn- sign-scalar [x]
  (let [m (q/magnitude x)]
    (cond (neg? m) -1 (pos? m) 1 :else 0)))

(defn sign
  "Sign of a value: a plain -1, 0, or 1, for a value of any dimension. On an Uncertain, the sign of
  the central value (the spread does not carry — this is a classification, not a measurement)."
  [x]
  (if (un/uncertain? x)
    (sign-scalar (un/value x))
    (lift-monotone sign-scalar x)))

;; ---- floor / ceil / round (dimension-preserving; operate on the display value) ----
(defn- floor-int
  "Exact floor of a real magnitude (Ratio/Long/BigInt/BigDecimal) → an integer."
  [x]
  (cond
    (integer? x) x
    (decimal? x) (bigint (.toBigInteger (.setScale x 0 RoundingMode/FLOOR)))
    :else (let [n (numerator x), d (denominator x)]                 ; Ratio, d > 0
            (if (>= n 0) (quot n d)
                (if (zero? (clojure.core/rem n d)) (quot n d) (dec (quot n d)))))))

(defn- ceil-int [x] (- (floor-int (- x))))

(defn- round-int [x]                                                ; half rounds toward +∞
  (if (decimal? x)
    (bigint (.toBigInteger (.setScale (.add x 0.5M) 0 RoundingMode/FLOOR)))
    (floor-int (+ x 1/2))))

(defn- round-with
  "Round the display value with `to-int`, keeping the quantity's unit: reconstruct the rounded
  value times the display unit's factor. (`display-value = magnitude / formula-factor`.)"
  [to-int x]
  (let [dv (q/display-value x)
        dv (if (or (integer? dv) (ratio? dv) (decimal? dv)) dv (rationalize dv))]  ; exact-ify a bare Double
    (q/quantity (* (to-int dv) (q/formula-factor (q/formula x))) (q/formula x))))

(c/defstep floor
  "Largest integer ≤ x, in x's unit. On an Uncertain, rounds the central value and keeps the spread."
  [x]
  (lift #(round-with floor-int %) x))

(c/defstep ceil
  "Smallest integer ≥ x, in x's unit. On an Uncertain, rounds the central value and keeps the spread."
  [x]
  (lift #(round-with ceil-int %) x))

(c/defstep round
  "Nearest integer (half → +∞), in x's unit. On an Uncertain, rounds the central value, keeping σ."
  [x]
  (lift #(round-with round-int %) x))

;; ---- mod / rem (conforming, dimension-preserving; scalar-only — intervals rejected) ----
(defn- conform! [op x y]
  (when-not (q/conforms? x y)
    (throw (ex-info (str op ": non-conforming dimensions") {:a (q/dims x) :b (q/dims y)}))))

(defn- scalar-only! [op x y]
  ;; Modular reduction is discontinuous, so it can't be soundly lifted over an interval (mapping the
  ;; endpoints would silently return a bracketing interval that lies) nor propagated over an
  ;; Uncertain's spread. Reject both explicitly instead.
  (when (or (iv/interval? x) (iv/interval? y) (un/uncertain? x) (un/uncertain? y))
    (throw (ex-info (str op " is not defined on intervals or uncertains: modular reduction is "
                         "discontinuous, so it cannot be soundly lifted — apply it to a point value")
                    {:op op :x x :y y}))))

(c/defstep mod
  "x modulo y — conforming, dimension-preserving (keeps x's unit). Scalar-only: an interval or
  uncertain argument is rejected (modular reduction is discontinuous, so it cannot be soundly lifted)."
  [x y]
  (scalar-only! "mod" x y)
  (conform! "mod" x y)
  (q/quantity (clojure.core/mod (q/magnitude x) (q/magnitude y)) (q/formula x)))

(c/defstep rem
  "Remainder of x by y — conforming, dimension-preserving. Scalar-only: an interval or uncertain
  argument is rejected (see `mod`)."
  [x y]
  (scalar-only! "rem" x y)
  (conform! "rem" x y)
  (q/quantity (clojure.core/rem (q/magnitude x) (q/magnitude y)) (q/formula x)))

;; ---- min / max (conforming, dimension-preserving; interval versions are componentwise) ----
(defn- extreme [pick x y]
  (cond
    ;; An Uncertain is compared by central value and returned whole (its σ kept); mixing it with an
    ;; Interval is rejected, matching the core verbs (two different notions of spread).
    (or (un/uncertain? x) (un/uncertain? y))
    (if (or (iv/interval? x) (iv/interval? y))
      (throw (ex-info "commensura: cannot mix an Uncertain and an Interval in min/max" {:x x :y y}))
      (pick x y))
    (or (iv/interval? x) (iv/interval? y))
    (iv/interval (pick (iv/lo-or-identity x) (iv/lo-or-identity y))
                 (pick (iv/hi-or-identity x) (iv/hi-or-identity y)))
    :else (pick x y)))

(c/defstep min
  "The physically smaller value (variadic); keeps the winner's unit (and, for an Uncertain, its σ).
  Conforming."
  ([x] x)
  ([x y] (extreme pick-lo x y))
  ([x y & more] (reduce min (min x y) more)))

(c/defstep max
  "The physically larger value (variadic); keeps the winner's unit (and, for an Uncertain, its σ).
  Conforming."
  ([x] x)
  ([x y] (extreme pick-hi x y))
  ([x y & more] (reduce max (max x y) more)))
