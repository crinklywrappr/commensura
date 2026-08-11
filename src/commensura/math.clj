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

  The monotone functions are interval-aware (mapped over the endpoints, with `abs` special-cased
  when the interval spans zero); `mod`/`rem` are scalar-only. Names shadow `clojure.core`, so use
  this namespace qualified (`m/abs`), never `:refer`. Comparisons come from `commensura.core`."
  (:refer-clojure :exclude [abs min max mod rem])
  (:require [commensura.quantity :as q]
            [commensura.interval :as iv]
            [commensura.core :as c])
  (:import [java.math RoundingMode]))

;; ---- roots & rational powers (dimensions scale; exact when a perfect root, else approx) ----
;; `c/pow` dispatches quantity vs interval and now takes rational exponents (generalized qpow/ipow).
(defn pow
  "Raise to an integer or rational exponent."
  [x n]
  (c/pow x n))

(defn sqrt
  "Square root."
  [x]
  (c/pow x 1/2))

(defn root
  "The q-th root."
  [x q]
  (c/pow x (/ 1 q)))

;; ---- shared helpers ----
(defn- pick-lo [x y] (if (pos? (q/qcompare x y)) y x))   ; the smaller of two conforming values
(defn- pick-hi [x y] (if (neg? (q/qcompare x y)) y x))   ; the larger

(defn- lift-monotone
  "Apply a monotone scalar function `f` to a value, or (for an interval) to each endpoint and the
  main value — `iv/interval` re-orders, so it works for increasing or decreasing `f`."
  [f x]
  (if (iv/interval? x)
    (if-let [m (iv/main-value x)]
      (iv/interval (f (iv/lo x)) (f m) (f (iv/hi x)))
      (iv/interval (f (iv/lo x)) (f (iv/hi x))))
    (f x)))

;; ---- abs (dimension-preserving; zero-spanning intervals reach 0) ----
(defn- abs-scalar [x]
  (let [m (q/magnitude x)]
    (q/quantity (if (neg? m) (- m) m) (q/formula x))))

(defn abs
  "Absolute value; dimension-preserving. Over a zero-spanning interval the lower bound is 0."
  [x]
  (if-not (iv/interval? x)
    (abs-scalar x)
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
  "Sign of a value: a plain -1, 0, or 1, for a value of any dimension."
  [x]
  (lift-monotone sign-scalar x))

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

(defn floor
  "Largest integer ≤ x, in x's unit."
  [x]
  (lift-monotone #(round-with floor-int %) x))

(defn ceil
  "Smallest integer ≥ x, in x's unit."
  [x]
  (lift-monotone #(round-with ceil-int %) x))

(defn round
  "Nearest integer (half → +∞), in x's unit."
  [x]
  (lift-monotone #(round-with round-int %) x))

;; ---- mod / rem (conforming, dimension-preserving; scalar-only) ----
(defn- conform! [op x y]
  (when-not (q/conforms? x y)
    (throw (ex-info (str op ": non-conforming dimensions") {:a (q/dims x) :b (q/dims y)}))))

(defn mod
  "x modulo y — conforming, dimension-preserving (keeps x's unit)."
  [x y]
  (conform! "mod" x y)
  (q/quantity (clojure.core/mod (q/magnitude x) (q/magnitude y)) (q/formula x)))

(defn rem
  "Remainder of x by y — conforming, dimension-preserving."
  [x y]
  (conform! "rem" x y)
  (q/quantity (clojure.core/rem (q/magnitude x) (q/magnitude y)) (q/formula x)))

;; ---- min / max (conforming, dimension-preserving; interval versions are componentwise) ----
(defn- extreme [pick x y]
  (if (or (iv/interval? x) (iv/interval? y))
    (iv/interval (pick (iv/lo-or-identity x) (iv/lo-or-identity y))
                 (pick (iv/hi-or-identity x) (iv/hi-or-identity y)))
    (pick x y)))

(defn min
  "The physically smaller value (variadic); keeps the winner's unit. Conforming."
  ([x] x)
  ([x y] (extreme pick-lo x y))
  ([x y & more] (reduce min (min x y) more)))

(defn max
  "The physically larger value (variadic); keeps the winner's unit. Conforming."
  ([x] x)
  ([x y] (extreme pick-hi x y))
  ([x y & more] (reduce max (max x y) more)))
