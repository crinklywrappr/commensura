;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.reader
  "Reifies the `#commensura/quantity \"…\"` and `#commensura/unit \"…\"` tagged
  literals (registered in resources/data_readers.clj). A quantity literal rebuilds
  an anonymous Quantity from its display formula; a unit literal resolves the named
  Unit through the registry (so user `defunit`s reify too).

  The EXACT value in the literal is the source of truth. The printed approximation
  is re-derived from that exact value and checked against the literal's — throwing
  if they disagree beyond `*approx-tolerance*`. This rules out junk data and values
  that would come out differently under JVM/library drift."
  (:require [clojure.string :as str]
            [commensura.quantity :as q]
            [commensura.registry :as registry]
            [commensura.units]))   ; load so builtin units are registered

(def ^:dynamic *approx-tolerance*
  "Maximum *relative* difference tolerated between the literal's printed
  approximation and the value re-derived from its exact part. Full-precision
  doubles agree to ~15 digits; 1e-9 absorbs realistic JVM/libm/format drift while
  still catching junk data."
  1e-9)

(defn- verify-approx! [re-derived parsed literal]
  (let [re (double re-derived), p (double parsed)
        scale (max 1.0 (Math/abs re))]
    (when (> (Math/abs (- re p)) (* *approx-tolerance* scale))
      (throw (ex-info "#commensura/quantity approximation inconsistent with its exact value"
                      {:literal literal :in-literal p :re-derived re
                       :tolerance *approx-tolerance*})))))

(defn- parse-term
  "\"foot\" or \"foot^3\" -> [name exponent]."
  [t]
  (if-let [[_ nm e] (re-matches #"(.+)\^(-?\d+)" t)]
    [nm (Long/parseLong e)]
    [t 1]))

(defn- parse-formula
  "A display formula string (`meter celsius/minute/second`) -> seq of
  [name exponent]. The inverse of quantity/format-formula: the first `/`-segment
  is the numerator product (space-separated), each remaining segment is one
  divisor (its exponent negated)."
  [unit-str]
  (let [[numer & dens] (str/split unit-str #"/")
        num-terms (when (and (seq (str/trim numer)) (not= (str/trim numer) "1"))
                    (map parse-term (str/split (str/trim numer) #"\s+")))
        den-terms (map (fn [d] (let [[nm e] (parse-term (str/trim d))] [nm (- e)])) dens)]
    (concat num-terms den-terms)))

(defn- reconstruct
  "Rebuild the quantity: `exact` copies of the compound unit named by `unit-str`,
  resolving each unit name through the registry (so user `defunit`s reify too)."
  [exact unit-str]
  (let [compound (reduce (fn [acc [nm e]]
                           (let [u (or (registry/lookup nm)
                                       (throw (ex-info "unknown unit in #commensura/quantity literal"
                                                       {:unit nm :literal unit-str})))]
                             (q/qmul acc (q/qpow u e))))
                         (q/scalar 1)
                         (parse-formula unit-str))]
    (q/scale compound exact)))

(defn- parse-literal
  "Split \"<exact> <unit> ≈ <approx> [dim]\" into {:exact :unit-str :approx}. The
  trailing `[dim]` label is decorative (dims are carried by the formula) and not
  used for reconstruction."
  [s]
  (let [[lhs rhs] (str/split s #" ≈ " 2)]
    (when (nil? rhs) (throw (ex-info "malformed commensura literal" {:literal s})))
    (let [lhs      (str/trim lhs)
          sp       (str/index-of lhs " ")
          rhs      (str/trim rhs)
          i        (str/index-of rhs " ")]
      {:exact    (read-string (if sp (subs lhs 0 sp) lhs))
       :unit-str (when sp (str/trim (subs lhs (inc sp))))
       :approx   (Double/parseDouble (if i (subs rhs 0 i) rhs))})))

(defn read-quantity
  "Data-reader for `#commensura/quantity \"<printed form>\"` → an anonymous
  Quantity, rebuilt from its display formula (empty formula ⇒ dimensionless)."
  [s]
  (let [{:keys [exact unit-str approx]} (parse-literal s)
        qty (if (seq unit-str) (reconstruct exact unit-str) (q/scalar exact))]
    (verify-approx! (q/display-value qty) approx s)
    qty))

(defn read-unit
  "Data-reader for `#commensura/unit \"<printed form>\"` → the named Unit, resolved
  through the registry (its defining `defunit` must have run)."
  [s]
  (let [{:keys [unit-str approx]} (parse-literal s)
        u (or (and unit-str (registry/lookup unit-str))
              (throw (ex-info "unknown unit in #commensura/unit literal"
                              {:unit unit-str :literal s})))]
    (verify-approx! (q/display-value u) approx s)     ; a Unit is one of itself (1)
    u))
