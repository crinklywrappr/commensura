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

  The verbs also accept Intervals (see commensura.interval), promoting scalars to
  degenerate intervals as needed."
  (:require [commensura.quantity :as q]
            [commensura.interval :as iv]
            [commensura.registry :as registry]))

(defn- iv? [x] (iv/interval? x))

(defn by
  "Product of quantities/numbers/intervals (dimensions add). Variadic."
  ([x] x)
  ([x y] (if (or (iv? x) (iv? y)) (iv/iby x y) (q/qmul x y)))
  ([x y & more] (reduce by (by x y) more)))

(defn per
  "Quotient, left-associative: (per a b c) = a/b/c (dimensions subtract)."
  ([x] x)
  ([x y] (if (or (iv? x) (iv? y)) (iv/iper x y) (q/qdiv x y)))
  ([x y & more] (reduce per (per x y) more)))

(defn plus
  "Sum of same-dimension quantities/intervals. Variadic."
  ([x] x)
  ([x y] (if (or (iv? x) (iv? y)) (iv/iplus x y) (q/qadd x y)))
  ([x y & more] (reduce plus (plus x y) more)))

(defn minus
  "Difference of same-dimension quantities/intervals; unary form negates."
  ([x] (if (iv? x) (iv/inegate x) (q/qmul (q/scalar -1) x)))
  ([x y] (if (or (iv? x) (iv? y)) (iv/iminus x y) (q/qsub x y)))
  ([x y & more] (reduce minus (minus x y) more)))

(defn pow
  "Raise a quantity/interval to an integer power."
  [x n] (if (iv? x) (iv/ipow x n) (q/qpow x n)))

(defn to
  "Re-express a quantity/interval in a target unit (dimension-preserving)."
  [x target] (if (iv? x) (iv/ito x target) (q/to x target)))

(defn ratio
  "Dimensionless count: how many of target fit in x (quantity or interval)."
  [x target] (if (iv? x) (iv/iratio x target) (q/ratio x target)))

(defn register-dimension!
  "Give a human name to a dimension map, so quantities of that dimension print it
  in the trailing `[..]` slot (overriding any builtin). Returns the name.

    (register-dimension! {:length 4} \"quaternary space\")
    (pow u/meter 4)   ;=> 1 meter^4 ≈ 1.0 [quaternary space]"
  [dims nm]
  (registry/register-dimension! dims nm))

(defmacro defunit
  "Define a callable `Unit` var — the modern add-unit!. The bound value is one of
  the new unit, displays under its own name, and works as a `to` target. The body
  is either a quantity/number *expression* (reduced to this unit's magnitude and
  dimensions) or, for the generated builtins, a literal `magnitude dims` pair. An
  optional docstring may precede either form:

    (defunit beer (by (u/floz 12) (u/percent 3.2) (per u/water u/alcohol)))
    (beer 5)                        ;=> 5 beer
    (defunit foot 381/1250 {:length 1})    ; literal form (what the generator emits)"
  ([sym expr] `(defunit ~sym nil ~expr))
  ([sym a b]
   (if (number? a)                                    ; (defunit sym magnitude dims)
     `(def ~sym (registry/register! ~(str sym) (q/unit ~(str sym) ~a ~b)))
     `(def ~sym ~@(when (string? a) [a])              ; (defunit sym [doc] expr)
        (let [v# (q/scalar ~b)]
          (registry/register! ~(str sym)
            (q/unit ~(str sym) (q/magnitude v#) (q/dims v#)))))))
  ([sym doc mag dims]                                 ; (defunit sym doc magnitude dims)
   `(def ~sym ~doc (registry/register! ~(str sym) (q/unit ~(str sym) ~mag ~dims)))))
