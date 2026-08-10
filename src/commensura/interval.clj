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

  Per Frink, a bare number/Quantity is *not* an interval, but the accessors treat it as a
  degenerate one (`mainValue[5]=5`) — so the protocol extends to any value and the arithmetic
  needs no scalar 'promotion'. `interval?` is false for a scalar.

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

(defrecord Interval2 [lo hi])
(defrecord Interval3 [lo main hi])

(extend-protocol IInterval
  Interval2 (lo [i] (:lo i)) (hi [i] (:hi i)) (main-value [_] nil)
  Interval3 (lo [i] (:lo i)) (hi [i] (:hi i)) (main-value [i] (:main i))
  Object    (lo [x] x)       (hi [x] x)       (main-value [x] x))   ; a scalar is degenerate

(defn interval? [x] (or (instance? Interval2 x) (instance? Interval3 x)))

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
(defn- spans-zero? [x] (<= (q/magnitude (lo x)) 0 (q/magnitude (hi x))))

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
  (let [ma (main-value a), mb (main-value b)]
    (when (and ma mb) (op ma mb))))

;; ---- arithmetic (interval-inclusion; endpoints exact; main value propagates) ----
(defn inegate [x]
  (let [neg1 (q/scalar -1)]
    (build [(q/qmul neg1 (lo x)) (q/qmul neg1 (hi x))]
           (when-let [m (main-value x)] (q/qmul neg1 m)))))

(defn iplus [x y]
  (build [(q/qadd (lo x) (lo y)) (q/qadd (hi x) (hi y))]
         (main-op q/qadd x y)))

(defn iminus [x y]
  ;; [a b] - [c d] = [a-d, b-c]
  (build [(q/qsub (lo x) (hi y)) (q/qsub (hi x) (lo y))]
         (main-op q/qsub x y)))

(defn iby [x y]
  ;; endpoints are the min/max over the four corner products
  (build [(q/qmul (lo x) (lo y)) (q/qmul (lo x) (hi y))
          (q/qmul (hi x) (lo y)) (q/qmul (hi x) (hi y))]
         (main-op q/qmul x y)))

(defn iper [x y]
  (when (spans-zero? y)
    (throw (ex-info "interval division by a value spanning zero" {:divisor y})))
  ;; x / [c d] = x * [1/d, 1/c]; iby then multiplies the mains
  (iby x (build [(q/qdiv (q/scalar 1) (lo y)) (q/qdiv (q/scalar 1) (hi y))]
                (when-let [m (main-value y)] (q/qdiv (q/scalar 1) m)))))

(defn ipow [x n]
  ;; correct even/odd handling: x^even reaches 0 when the interval spans zero
  (cond
    (zero? n) (->Interval2 1 1)
    (and (neg? n) (spans-zero? x))
    (throw (ex-info "interval negative power spans zero" {:interval x}))
    :else
    (let [e1    (q/qpow (lo x) n)
          e2    (q/qpow (hi x) n)
          cands (if (and (even? n) (spans-zero? x))
                  [e1 e2 (assoc e1 :mag 0)]
                  [e1 e2])]
      (build cands (when-let [m (main-value x)] (q/qpow m n))))))

(defn ito [x target]
  (build [(q/to (lo x) target) (q/to (hi x) target)]
         (when-let [m (main-value x)] (q/to m target))))

(defn iratio [x target]
  (build [(q/ratio (lo x) target) (q/ratio (hi x) target)]
         (when-let [m (main-value x)] (q/ratio m target))))
