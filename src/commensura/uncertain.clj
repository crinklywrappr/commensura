;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.uncertain
  "Measurement uncertainty over conforming Quantities — a value paired with a 1σ (sigma) *spread* that
  propagates through the arithmetic. An `Uncertain` is the **epistemic** sibling of `Interval`
  (commensura.interval): an interval denotes every value between two endpoints; an uncertain is one
  best estimate with a standard deviation. It is about how well the value is *known*, and rides the
  core verbs the way intervals do.

    (require '[commensura.uncertain :as un :refer [plus-minus]]
             '[commensura.units :as u]
             '[commensura.core :refer [by per plus minus pow]])

    (plus-minus (u/meter 5) (u/cm 2))     ;=> 5 meter ± 2 centimeter [length]
    (by (plus-minus (u/meter 5) (u/cm 2))
        (plus-minus (u/meter 3) (u/cm 1))) ; area, with the spread propagated in quadrature

  **Propagation is algebraic — no calculus.** Errors combine as independent Gaussians:
    * `plus`/`minus` — absolute σ in quadrature: σ = √(σx² + σy²).
    * `by`/`per`/`ratio` — *relative* σ in quadrature: (σ/v) = √((σx/x)² + (σy/y)²).
    * `pow` — relative σ scales by |exponent|; so `sqrt` (exponent ½) halves it and `pow`/`sqrt`
      round-trip. These are the only nonlinear verbs, and since commensura excludes transcendentals,
      that's the whole story — no derivatives needed.
    * `to` — a linear re-expression: value and σ both move to the target basis.

  The central value stays a normal exact quantity; σ is likewise an ordinary quantity carried
  through the standard tower (it typically goes *approximate* under a quadrature √, exactly as any
  irrational magnitude does). A plain quantity/number entering a verb is treated as σ=0 (an exact
  value), so uncertain and plain operands mix freely — but an uncertain and an `Interval` do not
  (they are different notions of spread): the core verbs throw on that mix.

  **Caveat — independence assumed.** Quadrature presumes the operands are uncorrelated, so
  `(by x x)` mis-propagates (it treats the two `x`s as independent); use `(pow x 2)` for that.

  Like `Interval`, an `Uncertain` has **no reader tag** — `pr` emits the raw record (its `value`/
  `sigma` fields are their own `#commensura/quantity` literals, so it round-trips), while `str`
  gives the friendly `value ± sigma [dimension]` form."
  (:require [commensura.quantity :as q]))

;; ---- shape + protocol -----------------------------------------------------------------------------
(defprotocol IUncertain
  (value [x] "The central best-estimate quantity.")
  (sigma [x] "The 1σ spread — a non-negative quantity conforming to the value."))

(defrecord Uncertain [value sigma]
  IUncertain
  (value [_] value)
  (sigma [_] sigma)
  Object
  ;; friendly `str`; `pr` still emits the raw record (round-trips via the field literals).
  (toString [_] (str (q/without-dimension value) " ± " (q/display-string sigma))))

(defn uncertain? [x] (satisfies? IUncertain x))

;; Scalar-tolerant accessors, mirroring interval's `*-or-identity`: a plain quantity/number is an
;; exactly-known value, so it *is* its own central value and carries a zero spread of its dimension.
(defn value-or-identity
  "An uncertain's central value; any other value is returned as-is (it's exactly known)."
  [x]
  (if (uncertain? x) (value x) x))

(defn- qabs
  "Absolute value of a quantity/number, dimension-preserving (a non-negative spread helper)."
  [x]
  (let [m (q/magnitude x)]
    (q/quantity (if (neg? m) (- m) m) (q/formula x))))

(defn sigma-or-zero
  "An uncertain's spread; for a plain quantity/number, a zero spread carrying its dimension."
  [x]
  (if (uncertain? x) (sigma x) (q/qmul x (q/scalar 0))))

;; ---- construction ---------------------------------------------------------------------------------
(defn plus-minus
  "Pair a central value with a 1σ absolute spread: `(plus-minus (u/meter 5) (u/cm 2))` ⇒
  `5 meter ± 2 centimeter`. The spread must conform to the value and be non-negative; both are kept
  exact (bare numbers are wrapped as dimensionless quantities)."
  [value spread]
  (let [v (q/scalar value)
        s (q/scalar spread)]
    (when-not (q/conforms? v s)
      (throw (ex-info "plus-minus: spread must conform to the value"
                      {:value (q/dims v) :spread (q/dims s)})))
    (when (neg? (q/magnitude s))
      (throw (ex-info "plus-minus: spread must be non-negative" {:spread s})))
    (->Uncertain v s)))

(defn plus-minus-rel
  "Like `plus-minus`, but the spread is a dimensionless *relative* uncertainty: σ = |value|·frac.
  `(plus-minus-rel (u/meter 5) 1/100)` ⇒ a 1%-uncertain 5 meter."
  [value frac]
  (let [v (q/scalar value)]
    (when (neg? frac)
      (throw (ex-info "plus-minus-rel: relative uncertainty must be non-negative" {:frac frac})))
    (->Uncertain v (q/qmul (qabs v) (q/scalar frac)))))

;; ---- accessors ------------------------------------------------------------------------------------
(defn relative
  "The relative (fractional) uncertainty |σ / value| — a non-negative dimensionless quantity."
  [x]
  (qabs (q/qdiv (sigma x) (value x))))

;; ---- quadrature helpers ---------------------------------------------------------------------------
(defn- q-square [x] (q/qpow x 2))

(defn- quad-abs
  "Absolute-error quadrature √(a² + b²) of two conforming spreads (dimension preserved)."
  [a b]
  (q/qpow (q/qadd (q-square a) (q-square b)) 1/2))

(defn- quad-rel
  "Relative-error quadrature √((σx/x)² + (σy/y)²) of two operands — a dimensionless factor."
  [x y]
  (let [rx (q/qdiv (sigma-or-zero x) (value-or-identity x))
        ry (q/qdiv (sigma-or-zero y) (value-or-identity y))]
    (q/qpow (q/qadd (q-square rx) (q-square ry)) 1/2)))

(defn- rel->sigma
  "Turn a relative uncertainty (dimensionless) into an absolute spread for value `v`: σ = |v|·rel."
  [v rel]
  (q/qmul (qabs v) rel))

;; ---- arithmetic (value via the exact tower; σ propagated per the ns docstring) ---------------------
(defn unegate [x]
  ;; negation flips the value; the spread is unchanged (still non-negative).
  (->Uncertain (q/qmul (q/scalar -1) (value-or-identity x)) (sigma-or-zero x)))

(defn uplus [x y]
  (->Uncertain (q/qadd (value-or-identity x) (value-or-identity y))
               (quad-abs (sigma-or-zero x) (sigma-or-zero y))))

(defn uminus [x y]
  (->Uncertain (q/qsub (value-or-identity x) (value-or-identity y))
               (quad-abs (sigma-or-zero x) (sigma-or-zero y))))

(defn uby [x y]
  (let [v (q/qmul (value-or-identity x) (value-or-identity y))]
    (->Uncertain v (rel->sigma v (quad-rel x y)))))

(defn uper [x y]
  (let [v (q/qdiv (value-or-identity x) (value-or-identity y))]
    (->Uncertain v (rel->sigma v (quad-rel x y)))))

(defn upow [x n]
  ;; value = x^n; relative σ scales by |n| (single operand, so no quadrature): σ = |x^n|·|n|·|σx/x|.
  (let [vx  (value-or-identity x)
        v   (q/qpow vx n)
        rel (q/qmul (q/scalar (if (neg? n) (- n) n))
                    (qabs (q/qdiv (sigma-or-zero x) vx)))]
    (->Uncertain v (rel->sigma v rel))))

(defn uto [x target]
  ;; linear re-expression: value and spread both move to the target basis (magnitudes unchanged).
  (->Uncertain (q/to (value-or-identity x) target)
               (q/to (sigma-or-zero x) target)))

(defn uratio [x target]
  ;; dimensionless count; relative σ combines x and target in quadrature (a plain target has σ=0).
  (let [v (q/ratio (value-or-identity x) target)]
    (->Uncertain v (rel->sigma v (quad-rel x target)))))

;; ---- statistical comparison (the epistemic analogue of certainly-/possibly-) ----------------------
(defn combined-sigma
  "Quadrature of the two spreads, √(σx² + σy²) — the 1σ scale for comparing x and y."
  [x y]
  (quad-abs (sigma-or-zero x) (sigma-or-zero y)))

(defn within?
  "Do `x` and `y` agree to within `n` combined standard deviations?
  `|value x − value y| ≤ n · √(σx² + σy²)`. Plain quantities carry σ=0, so this also compares a
  measurement against an exact reference."
  [x y n]
  (let [diff (qabs (q/qsub (value-or-identity x) (value-or-identity y)))
        tol  (q/qmul (q/scalar n) (combined-sigma x y))]
    (not (pos? (q/qcompare diff tol)))))

(defn consistent?
  "Agreement at the conventional 2σ (~95%) level — `(within? x y 2)`. Use `within?` for another
  threshold (1σ ≈ 68%, 3σ ≈ 99.7%)."
  [x y]
  (within? x y 2))
