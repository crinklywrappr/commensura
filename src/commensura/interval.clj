;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.interval
  "Interval arithmetic over conforming Quantities. An interval denotes every value between
  its endpoints; arithmetic yields the tightest interval containing every result (the
  interval-inclusion property). Endpoints are Quantities (or plain numbers), kept exact.

  Two shapes, unified by the `IInterval` protocol (`lo`/`hi`/`main-value`):
    * `Interval2 [lo hi]`      — a plain range.
    * `Interval3 [lo main hi]` — a range with a *main value* (Frink's `mainValue`): a
      best-known estimate that need not be the center. It propagates through arithmetic (the
      same op applied to the operands' mains) and is dropped the moment any operand lacks one,
      so `[2,2.5,3] * [7,8.2,9.4] = [14, 20.5, 28.2]`.

  Per Frink, a bare number/Quantity is *not* an interval. Only the two records implement
  `IInterval`; the `*-or-identity` accessors add the degenerate case (`mainValue[5]=5`) by
  returning a non-interval unchanged (its own bound), which is why the arithmetic and the
  comparison operators need no scalar 'promotion'. `interval?` is false for a scalar.

  An interval prints as its raw record (endpoints as their own `#commensura/quantity`
  literals) — no bespoke tagged literal — which is readable and round-trips losslessly.

  The `commensura.core` verbs (`by`/`per`/`plus`/`minus`/`pow`/`to`/`ratio`) accept intervals
  transparently."
  (:require [commensura.quantity :as q]))

;; ---- shapes + protocol ----
(defprotocol IInterval
  (lo [x] "Lower bound (a bare value is its own bound).")
  (hi [x] "Upper bound.")
  (main-value [x] "Best-known estimate (Frink's `mainValue`), or nil for a plain interval."))

(defrecord Interval2 [lo hi]
  IInterval
  (lo [_] lo)
  (hi [_] hi)
  (main-value [_] nil))

(defrecord Interval3 [lo main hi]
  IInterval
  (lo [_] lo)
  (hi [_] hi)
  (main-value [_] main))

(defn interval? [x] (satisfies? IInterval x))

;; Scalar-tolerant accessors. Only the two records implement `IInterval`; a non-interval (a
;; quantity or plain number) is a single point, so it is its own bound and is returned
;; unchanged. The arithmetic and comparison operators read operands through these, so a scalar
;; needs no 'promotion' to a degenerate interval.
(defn lo-or-identity
  "An interval's lower bound; any other value is returned as-is (a point is its own bound)."
  [x]
  (if (interval? x) (lo x) x))

(defn hi-or-identity
  "An interval's upper bound; any other value is returned as-is."
  [x]
  (if (interval? x) (hi x) x))

(defn main-value-or-identity
  "An interval's main value (nil for a plain Interval2); any other value is returned as-is."
  [x]
  (if (interval? x) (main-value x) x))

;; ---- construction ----
(defn- conform! [what a b]
  (when-not (= (q/dims a) (q/dims b))
    (throw (ex-info (str "interval: non-conforming " what) {:a (q/dims a) :b (q/dims b)}))))

(defn- norm [x] (if (number? x) (rationalize x) x))   ; keep the exact tower: 2.5 -> 5/2

(defn interval
  "Construct from two conforming endpoints (auto-ordered by magnitude; bare decimals are
  rationalized, like the rest of commensura). The 3-arity adds a *main value* `m` — the
  best-known estimate; it must conform and lie within [lo, hi], but need not be the center,
  and propagates through arithmetic."
  ([a b]
   (let [a (norm a), b (norm b)]
     (conform! "endpoints" a b)
     (->Interval2 (min-key q/magnitude a b) (max-key q/magnitude a b))))
  ([a m b]
   (let [a (norm a), m (norm m), b (norm b)]
     (conform! "endpoints" a b)
     (conform! "main value" a m)
     (let [lo (min-key q/magnitude a b), hi (max-key q/magnitude a b)]
       (when-not (<= (q/magnitude lo) (q/magnitude m) (q/magnitude hi))
         (throw (ex-info "interval: main value must lie within [lo, hi]"
                         {:lo lo :main m :hi hi})))
       (->Interval3 lo m hi)))))

;; ---- internals ----
;; The arithmetic reads each operand's endpoints through the `*-or-identity` accessors, so a
;; scalar operand acts as the degenerate interval [x, x] with no promotion.
(defn- spans-zero? [x]
  (<= (q/magnitude (lo-or-identity x)) 0 (q/magnitude (hi-or-identity x))))

(defn- build
  "Assemble a result from corner values — lo/hi are the min/max by magnitude — with an
  already-computed `main` (Interval3 when non-nil, else a plain Interval2)."
  [corners main]
  (let [l (apply min-key q/magnitude corners)
        h (apply max-key q/magnitude corners)]
    (if main (->Interval3 l main h) (->Interval2 l h))))

(defn- main-op
  "The result's main value from applying scalar `op` to the operands' mains — dropped (nil)
  unless *both* operands carry one (a scalar carries itself; a plain interval carries nil)."
  [op a b]
  (let [ma (main-value-or-identity a), mb (main-value-or-identity b)]
    (when (and ma mb) (op ma mb))))

;; ---- arithmetic (interval-inclusion; endpoints exact; main value propagates) ----
(defn inegate [x]
  (let [neg1 (q/scalar -1)]
    (build [(q/qmul neg1 (lo-or-identity x)) (q/qmul neg1 (hi-or-identity x))]
           (when-let [m (main-value-or-identity x)] (q/qmul neg1 m)))))

(defn iplus [x y]
  (build [(q/qadd (lo-or-identity x) (lo-or-identity y))
          (q/qadd (hi-or-identity x) (hi-or-identity y))]
         (main-op q/qadd x y)))

(defn iminus [x y]
  ;; [a b] - [c d] = [a-d, b-c]
  (build [(q/qsub (lo-or-identity x) (hi-or-identity y))
          (q/qsub (hi-or-identity x) (lo-or-identity y))]
         (main-op q/qsub x y)))

(defn iby [x y]
  ;; endpoints are the min/max over the four corner products
  (let [xl (lo-or-identity x), xh (hi-or-identity x)
        yl (lo-or-identity y), yh (hi-or-identity y)]
    (build [(q/qmul xl yl) (q/qmul xl yh) (q/qmul xh yl) (q/qmul xh yh)]
           (main-op q/qmul x y))))

(defn iper [x y]
  (when (spans-zero? y)
    (throw (ex-info "interval division by a value spanning zero" {:divisor y})))
  ;; x / [c d] = x * [1/d, 1/c]; iby then multiplies the mains
  (iby x (build [(q/qdiv (q/scalar 1) (lo-or-identity y))
                 (q/qdiv (q/scalar 1) (hi-or-identity y))]
                (when-let [m (main-value-or-identity y)] (q/qdiv (q/scalar 1) m)))))

(defn ipow [x n]
  ;; correct even/odd handling: x^even reaches 0 when the interval spans zero
  (cond
    (zero? n) (->Interval2 1 1)
    (and (neg? n) (spans-zero? x))
    (throw (ex-info "interval negative power spans zero" {:interval x}))
    :else
    (let [e1    (q/qpow (lo-or-identity x) n)
          e2    (q/qpow (hi-or-identity x) n)
          cands (if (and (even? n) (spans-zero? x))
                  [e1 e2 (assoc e1 :mag 0)]
                  [e1 e2])]
      (build cands (when-let [m (main-value-or-identity x)] (q/qpow m n))))))

(defn ito [x target]
  (build [(q/to (lo-or-identity x) target) (q/to (hi-or-identity x) target)]
         (when-let [m (main-value-or-identity x)] (q/to m target))))

(defn iratio [x target]
  (build [(q/ratio (lo-or-identity x) target) (q/ratio (hi-or-identity x) target)]
         (when-let [m (main-value-or-identity x)] (q/ratio m target))))
