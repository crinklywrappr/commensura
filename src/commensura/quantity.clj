;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.quantity
  "The core Quantity value: an exact magnitude (in base SI units) plus a
  dimension map (base-dimension -> integer exponent), with an optional display
  unit (`sym` + `factor`) used only for printing / `to`.

  A unit var such as `commensura.units/foot` is itself a Quantity equal to *one*
  foot; because Quantity implements `IFn`, `(foot 10)` scales it to ten feet."
  (:require [commensura.dimensions :as dims]
            [clojure.pprint :as pp]))

(defn- dimension-name
  "Human name for a dimension map, e.g. {:length 3} -> \"volume\", or a
  base-dimension fallback ({:length 1} -> \"length\"), or nil."
  [d]
  (or (get dims/names d)
      (when (and (== 1 (count d)) (== 1 (val (first d))))
        (name (key (first d))))))

(declare scale magnitude dimensions display-value format-quantity)

;; mag    - exact magnitude in base units (Ratio/BigInt/Long/BigDecimal)
;; dims   - {base-dim exponent}, zero exponents removed
;; sym    - display unit name (or nil => base units)
;; factor - base-magnitude of one display unit (for `display-value`), or nil
(defrecord Quantity [mag dims sym factor]
  clojure.lang.IFn
  (invoke [this n] (scale this n))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))
  Object
  (toString [this] (format-quantity this)))

(defn quantity? [x] (instance? Quantity x))

(defn quantity
  "Construct a Quantity — the single construction path (the only caller of the
  record factory `->Quantity`). The magnitude is taken as already-exact; plain
  numbers are `rationalize`d at the input boundary (`scalar`/`scale`/`magnitude`),
  not here.
    [mag]                  dimensionless, no display unit
    [mag dims]             base-unit form (no display unit)
    [mag dims sym factor]  with a display unit (`sym` + one-unit base `factor`)"
  ([mag]                 (quantity mag {} nil nil))
  ([mag dims]            (quantity mag dims nil nil))
  ([mag dims sym factor] (->Quantity mag dims sym factor)))

(defn magnitude
  "Exact base-unit magnitude of x (a Quantity or plain number). Plain numbers are
  `rationalize`d so decimal literals like 3.2 become exact (16/5), matching Frink."
  [x]
  (if (quantity? x) (:mag x) (rationalize x)))

(defn dimensions
  "Dimension map of x; plain numbers are dimensionless ({})."
  [x]
  (if (quantity? x) (:dims x) {}))

(defn dimensionless? [x] (empty? (dimensions x)))
(defn conforms? [a b] (= (dimensions a) (dimensions b)))

(defn scalar
  "Wrap a plain number as a dimensionless Quantity (idempotent on Quantities)."
  [n]
  (if (quantity? n) n (quantity (rationalize n))))

(defn scale
  "Scale a unit/quantity's magnitude by a plain number — what `(u/feet 10)` does.
  Retains the display unit."
  [q n]
  (quantity (* (:mag q) (rationalize n)) (:dims q) (:sym q) (:factor q)))

(defn- clean [dims] (into {} (remove (comp zero? val)) dims))
(defn- negate-dims [dims] (into {} (map (fn [[k v]] [k (- v)])) dims))

(defn- sym-of [x] (when (quantity? x) (:sym x)))
(defn- factor-of [x] (when (quantity? x) (:factor x)))

(defn qmul [x y]
  ;; Multiplying by a bare dimensionless scalar preserves the other operand's
  ;; display unit, so (by 5 (foot 12)) still reads in feet.
  (let [[sym factor] (cond
                       (and (sym-of x) (not (quantity? y))) [(sym-of x) (factor-of x)]
                       (and (sym-of y) (not (quantity? x))) [(sym-of y) (factor-of y)]
                       :else [nil nil])]
    (quantity (* (magnitude x) (magnitude y))
              (clean (merge-with + (dimensions x) (dimensions y)))
              sym factor)))

(defn qdiv [x y]
  (quantity (/ (magnitude x) (magnitude y))
            (clean (merge-with + (dimensions x) (negate-dims (dimensions y))))))

(defn qadd [x y]
  (when-not (conforms? x y)
    (throw (ex-info "plus: non-conforming dimensions"
                    {:a (dimensions x) :b (dimensions y)})))
  ;; sum keeps the left operand's display unit
  (quantity (+ (magnitude x) (magnitude y)) (dimensions x) (sym-of x) (factor-of x)))

(defn qsub [x y]
  (when-not (conforms? x y)
    (throw (ex-info "minus: non-conforming dimensions"
                    {:a (dimensions x) :b (dimensions y)})))
  (quantity (- (magnitude x) (magnitude y)) (dimensions x) (sym-of x) (factor-of x)))

(defn- expt
  "Exact integer power (keeps the exact tower)."
  [base n]
  (cond
    (zero? n) 1
    (pos? n)  (reduce * (repeat n base))
    :else     (/ 1 (reduce * (repeat (- n) base)))))

(defn qpow [x n]
  (quantity (expt (magnitude x) n)
            (clean (into {} (map (fn [[k v]] [k (* v n)])) (dimensions x)))))

(defn to
  "Re-express q in the target unit basis (dimension-preserving). Requires matching
  dimensions; the physical magnitude is unchanged — only the display unit changes,
  so the printed value equals magnitude(q)/magnitude(target)."
  [q target]
  (when-not (conforms? q target)
    (throw (ex-info "to: non-conforming units"
                    {:from (dimensions q) :to (dimensions target)})))
  (quantity (magnitude q) (dimensions q) (:sym target) (magnitude target)))

(defn ratio
  "Bare dimensionless count: how many of target fit in q."
  [q target]
  (when-not (conforms? q target)
    (throw (ex-info "ratio: non-conforming units"
                    {:a (dimensions q) :b (dimensions target)})))
  (quantity (/ (magnitude q) (magnitude target))))

(defn display-value
  "The exact value shown to the user: base magnitude divided by the display
  unit's factor (or the base magnitude itself when there is no display unit)."
  [q]
  (if (quantity? q)
    (let [f (:factor q)]
      (if (and f (not (zero? f))) (/ (:mag q) f) (:mag q)))
    q))

(defn format-quantity
  "Human-readable form: `<exact> <unit> ≈ <approx> [dimension]`. Used by
  `toString`/`str`/`println`, and as the payload of the `#commensura/quantity` literal."
  [q]
  (let [dv    (display-value q)
        d     (:dims q)
        dname (dimension-name d)]
    (str (pr-str dv)
         (when (:sym q) (str " " (:sym q)))
         " ≈ " (double dv) " "
         (cond dname    (str "[" dname "]")
               (seq d)  (pr-str d)
               :else    "[dimensionless]"))))

;; `pr`/`prn`/the REPL emit a `#commensura/quantity` tagged literal whose payload is the
;; human-readable string; `str`/`println` (toString) emit the string itself.
(defmethod print-method Quantity [q ^java.io.Writer w]
  (.write w (str "#commensura/quantity " (pr-str (format-quantity q)))))

;; clojure.pprint (which CIDER/Calva use to render results) ignores print-method on
;; records and dumps the raw map — delegate it back to our print-method.
(defmethod pp/simple-dispatch Quantity [q] (print-method q *out*))
