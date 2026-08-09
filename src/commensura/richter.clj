;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.richter
  "The Richter scale — a nonlinear (log/exp) conversion between an earthquake's
  magnitude and its radiated seismic energy, ported from Frink's `Richter[n]`
  function (Choy–Boatwright 1995). Because the scale is logarithmic, every result
  is irrational ⇒ an `ApproxQuantity`.

    (require '[commensura.richter :as r] '[commensura.core :refer [to]])
    (r/magnitude->energy 7.0)                 ;=> ~2.0e15 joule
    (r/energy->magnitude (r/magnitude->energy 7.0))   ;=> ~7.0 richter

  `richter` is a dimensionless marker unit, so a computed magnitude prints as
  `6.9 richter [dimensionless]` rather than a bare number."
  (:require [commensura.quantity :as q]
            [commensura.core :as c]
            [commensura.units :as u]))

;; a dimensionless marker so magnitudes carry their scale in the printed formula
(c/defunit richter
  "A point on the Richter magnitude scale (dimensionless)."
  1 {})

;; Choy–Boatwright constants (Frink's decimal literals, kept as exact rationals)
(def ^:private energy-coeff 22387)          ; pre-exponential coefficient (→ joule)
(def ^:private energy-rate  345388/100000)  ; 3.45388 — magnitude → energy exponent
(def ^:private mag-offset   -29/10)         ; -2.9    — energy → magnitude intercept
(def ^:private mag-slope    28953/100000)   ; 0.28953 — energy → magnitude slope

;; SHA-256 of Frink's `Richter[…]` body in units.txt, pinned as var metadata below so
;; the drift test (commensura.units.drift-test) fails loudly if the upstream definition
;; changes out from under this hand-written translation.
(def ^:private frink-sha "5e00af06774ed6fa8ec60e37d7fe2b4cee128a4527ad6632a7a31b8d562be09b")

(defn magnitude->energy
  "A Richter magnitude (dimensionless number or `richter`-denominated quantity) →
  radiated seismic energy: `E = 22387 · e^(3.45388·m) J`."
  {:frink/fn "Richter" :frink/sha frink-sha}
  [m]
  (when-not (q/dimensionless? m)
    (throw (ex-info "magnitude->energy expects a dimensionless Richter magnitude"
                    {:m m :dims (q/dims m)})))
  (let [n (q/magnitude m)]                          ; the magnitude's value
    ;; Build the ApproxQuantity directly rather than via `q/quantity`: the scale is
    ;; empirical and logarithmic, so a result is definitionally inexact even when the
    ;; BigDecimal lands on a whole number — e.g. (magnitude->energy 0) = 22387·e^0 =
    ;; 22387 J, which `q/quantity` would wrongly re-exactify to a PreciseQuantity. The
    ;; magnitude is always a BigDecimal here (bexp × exact rationals), so this is safe.
    (q/->ApproxQuantity (* energy-coeff (q/bexp (* energy-rate n)))  ; E in base units (joule factor 1)
                        (q/formula u/joule))))

(defn energy->magnitude
  "Radiated seismic energy (any energy-dimensioned quantity) → its Richter
  magnitude: `m = -2.9 + 0.28953·ln(E/J)`."
  {:frink/fn "Richter" :frink/sha frink-sha}
  [e]
  (when-not (q/conforms? e u/joule)
    (throw (ex-info "energy->magnitude expects an energy quantity"
                    {:e e :dims (q/dims e)})))
  (let [x (q/magnitude (c/per e u/joule))]          ; the dimensionless ratio E/J
    (q/->ApproxQuantity (+ mag-offset (* mag-slope (q/bln x)))   ; force approx, as above
                        (q/formula richter))))
