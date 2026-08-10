;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.temperature
  "Absolute temperature-scale conversions — Frink's affine `Fahrenheit`/`Celsius`/`Reaumur`
  functions (`K = scale·x + offset`). Each is exact, and dispatches on its argument like
  Frink's `Name[x]`:

    (celsius 100)            ;=> 373.15 K   (a dimensionless reading → absolute temperature)
    (fahrenheit 32)          ;=> 273.15 K
    (celsius (fahrenheit 32));=> 0          (a temperature → its reading on this scale)

  These are ABSOLUTE temperatures — distinct from the multiplicative *degree* units
  `u/degcelsius` / `u/degreeFahrenheit`, which are temperature *differences* (ΔT):
  `(celsius 100)` is 373.15 K, but `(u/degcelsius 100)` is a 100 K difference. (Rankine
  has zero offset, so it is a plain unit, `u/Rankine`; no affine fn is needed.)"
  (:require [commensura.quantity :as q]
            [commensura.core :as c]
            [commensura.units :as u]))

(defn- affine
  "Bidirectional affine conversion `K = scale·x + offset` (both exact). A dimensionless `x`
  becomes an absolute temperature (kelvin); a temperature `x` becomes its scale reading
  `(K - offset)/scale` (a bare number)."
  [nm scale offset x]
  (cond
    (q/dimensionless? x)     (c/by (+ (* scale (q/magnitude x)) offset) u/kelvin)
    (q/conforms? x u/kelvin) (/ (- (q/magnitude x) offset) scale)
    :else (throw (ex-info (str nm ": expected a dimensionless degree reading or a temperature")
                          {:x x :dims (q/dims x)}))))

;; SHA-256 of each function's units.txt body (pinned as var metadata for the M2.7 drift test)
(def ^:private fahrenheit-sha "1552fcb3bc05bd472ed6efc67aa079c92e391707e2d0db8310eba2560af92eff")
(def ^:private celsius-sha    "e98a6b6a6b3b5efa3c07d1b90aaae42b02466171301029eab1eae10e103f9a22")
(def ^:private reaumur-sha    "87ebc650c24d2133ce4399713e903f5a0d61d2d85592f7eb3373db6c5e7c8580")

(defn celsius
  "Absolute Celsius ↔ temperature: `(celsius 100)` → 373.15 K; `(celsius <temp>)` → °C."
  {:frink/fn "Celsius" :frink/sha celsius-sha}
  [x] (affine "celsius" 1 5463/20 x))

(defn fahrenheit
  "Absolute Fahrenheit ↔ temperature: `(fahrenheit 32)` → 273.15 K; `(fahrenheit <temp>)` → °F."
  {:frink/fn "Fahrenheit" :frink/sha fahrenheit-sha}
  [x] (affine "fahrenheit" 5/9 45967/180 x))

(defn reaumur
  "Absolute Réaumur ↔ temperature: `(reaumur 80)` → 373.15 K; `(reaumur <temp>)` → °Ré."
  {:frink/fn "Reaumur" :frink/sha reaumur-sha}
  [x] (affine "reaumur" 5/4 5463/20 x))
