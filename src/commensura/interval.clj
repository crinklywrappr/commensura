;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.interval
  "Interval arithmetic over conforming Quantities. An Interval `[lo hi]` denotes
  every value between its endpoints; arithmetic produces the tightest interval
  containing every result (the interval-inclusion property). Endpoints are
  Quantities (or plain numbers), kept exact.

  The `commensura.core` verbs (`by`/`per`/`plus`/`minus`) accept Intervals
  transparently, promoting scalars to degenerate `[x x]` intervals."
  (:require [commensura.quantity :as q]
            [clojure.pprint :as pp]))

(defrecord Interval [lo hi])

(defn interval? [x] (instance? Interval x))

(defn- qmin [a b] (if (<= (q/magnitude a) (q/magnitude b)) a b))
(defn- qmax [a b] (if (>= (q/magnitude a) (q/magnitude b)) a b))

(defn interval
  "Construct from two conforming endpoints (ordered automatically)."
  [a b]
  (when-not (= (q/dims a) (q/dims b))
    (throw (ex-info "interval: non-conforming endpoints"
                    {:a (q/dims a) :b (q/dims b)})))
  (->Interval (qmin a b) (qmax a b)))

(defn interval-pm
  "Center ± uncertainty ⇒ [mid-delta, mid+delta]."
  [mid delta]
  (interval (q/qsub mid delta) (q/qadd mid delta)))

;; ---- accessors ----
(defn lo [iv] (:lo iv))
(defn hi [iv] (:hi iv))
(defn midpoint  [iv] (q/qmul (q/qadd (:lo iv) (:hi iv)) (q/scalar 1/2)))
(defn width     [iv] (q/qsub (:hi iv) (:lo iv)))
(defn radius    [iv] (q/qmul (width iv) (q/scalar 1/2)))
(def  half-width radius)

(defn within?
  "Is value x (a Quantity/number) inside the interval? (magnitude comparison)"
  [iv x]
  (<= (q/magnitude (:lo iv)) (q/magnitude x) (q/magnitude (:hi iv))))

(defn- promote [x] (if (interval? x) x (->Interval x x)))
(defn- spans-zero? [iv] (<= (q/magnitude (:lo iv)) 0 (q/magnitude (:hi iv))))

;; ---- arithmetic (interval-inclusion; endpoints exact) ----
(defn inegate [x]
  (let [a (promote x), neg1 (q/scalar -1)]
    (interval (q/qmul neg1 (:lo a)) (q/qmul neg1 (:hi a)))))

(defn iplus [x y]
  (let [a (promote x), b (promote y)]
    (interval (q/qadd (:lo a) (:lo b)) (q/qadd (:hi a) (:hi b)))))

(defn iminus [x y]
  ;; [a b] - [c d] = [a-d, b-c]
  (let [a (promote x), b (promote y)]
    (interval (q/qsub (:lo a) (:hi b)) (q/qsub (:hi a) (:lo b)))))

(defn iby [x y]
  ;; endpoints are the min/max over the four corner products
  (let [a (promote x), b (promote y)
        corners [(q/qmul (:lo a) (:lo b)) (q/qmul (:lo a) (:hi b))
                 (q/qmul (:hi a) (:lo b)) (q/qmul (:hi a) (:hi b))]]
    (->Interval (reduce qmin corners) (reduce qmax corners))))

(defn iper [x y]
  (let [a (promote x), b (promote y)]
    (when (spans-zero? b)
      (throw (ex-info "interval division by an interval spanning zero" {:divisor b})))
    ;; a / [c d] = a * [1/d, 1/c]   (iby reorders corners, so endpoint order is moot)
    (iby a (->Interval (q/qdiv (q/scalar 1) (:hi b))
                       (q/qdiv (q/scalar 1) (:lo b))))))

(defn ipow [x n]
  ;; correct even/odd handling: x^even reaches 0 when the interval spans zero
  (if (zero? n)
    (->Interval 1 1)
    (let [a (promote x)]
      (when (and (neg? n) (spans-zero? a))
        (throw (ex-info "interval negative power spans zero" {:interval a})))
      (let [e1    (q/qpow (:lo a) n)
            e2    (q/qpow (:hi a) n)
            cands (if (and (even? n) (spans-zero? a))
                    [e1 e2 (assoc e1 :mag 0)]
                    [e1 e2])]
        (->Interval (reduce qmin cands) (reduce qmax cands))))))

(defn ito [x target]
  (let [a (promote x)]
    (interval (q/to (:lo a) target) (q/to (:hi a) target))))

(defn iratio [x target]
  (let [a (promote x)]
    (interval (q/ratio (:lo a) target) (q/ratio (:hi a) target))))

(defmethod print-method Interval [iv ^java.io.Writer w]
  (.write w (str "#commensura/interval [" (q/display-value (:lo iv)) ", "
                 (q/display-value (:hi iv)) "]")))

;; keep clojure.pprint (CIDER/Calva) from dumping the raw record map
(defmethod pp/simple-dispatch Interval [iv] (print-method iv *out*))
