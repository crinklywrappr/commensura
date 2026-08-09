;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.units.manifest
  "DEV-ONLY manifest of Frink's nonlinear `Name[x] :=` functions — the bridge between
  what units.txt *defines* and what commensura *implements*. Every such function the
  converter catalogues (`:functions`) is classified here, so `commensura.units.drift-test`
  can (a) verify each hand-written translation still matches the units.txt body it was
  pinned against, and (b) flag any function that appears or disappears upstream.

  This map is also documentation: it says exactly where each real function lives.

  Statuses:
    :implemented — translated by the named runtime var(s); each var carries
                   {:frink/fn :frink/sha} metadata pinning the exact body (see the
                   fingerprint on the var, not here).
    :affine      — reduced by the converter's `affine-temps` table; its body SHA is
                   pinned here (no runtime var yet — see M2.7 in the plan).
    :deferred    — composite time/angle sugar (DMS/DM/HMS/DHMS), deliberately not built."
  (:import [java.security MessageDigest]))

(defn sha256
  "Lower-hex SHA-256 of a string — the fingerprint of a normalized Frink function body."
  [^String s]
  (->> (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8"))
       (map #(format "%02x" (bit-and % 0xff)))
       (apply str)))

(def functions
  "Frink function name -> classification. See the ns docstring for the statuses."
  {"Richter"    {:status :implemented
                 :vars  '[commensura.richter/magnitude->energy
                          commensura.richter/energy->magnitude]}
   "Fahrenheit" {:status :affine :sha "1552fcb3bc05bd472ed6efc67aa079c92e391707e2d0db8310eba2560af92eff"}
   "F"          {:status :affine :sha "1441af9995b5dae61ab61866bdb6a9932f557e5365773f5bed600dd7cde93eaa"}
   "Celsius"    {:status :affine :sha "e98a6b6a6b3b5efa3c07d1b90aaae42b02466171301029eab1eae10e103f9a22"}
   "C"          {:status :affine :sha "2d272634710a18169d2b8a7b58a5ac184049d98308a6e10dc99027ac938cc722"}
   "Reaumur"    {:status :affine :sha "87ebc650c24d2133ce4399713e903f5a0d61d2d85592f7eb3373db6c5e7c8580"}
   "DMS"        {:status :deferred}
   "DM"         {:status :deferred}
   "HMS"        {:status :deferred}
   "DHMS"       {:status :deferred}})

(defn classify
  "Partition Frink function names (the converter's `:functions` catalogue) by how each
  is handled: {:implemented #{…} :affine #{…} :deferred #{…} :unhandled #{…}}.
  `:unhandled` = catalogued names with no entry above — they need attention."
  [names]
  (reduce (fn [acc nm]
            (update acc (get-in functions [nm :status] :unhandled) (fnil conj #{}) nm))
          {:implemented #{} :affine #{} :deferred #{} :unhandled #{}}
          (set names)))
