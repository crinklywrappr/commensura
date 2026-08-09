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

  A leading `≈` in the payload marks an *approximate* value: for those, the BigDecimal
  in the literal is the source of truth (no re-check). For an *exact* quantity the exact
  value is the source of truth — its printed approximation is re-derived and checked
  against the literal's, throwing if they disagree beyond `*approx-tolerance*` (ruling
  out junk data or values that would come out differently under JVM/library drift)."
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
                           (let [u (or (registry/lookup-unit nm)
                                       (throw (ex-info "unknown unit in #commensura/quantity literal"
                                                       {:unit nm :literal unit-str})))]
                             (q/qmul acc (q/qpow u e))))
                         (q/scalar 1)
                         (parse-formula unit-str))]
    (q/scale compound exact)))

(defn- parse-literal
  "Split `<exact> <unit> [≈ <approx>] [dim]` into {:exact :unit-str :approx}. The
  ` ≈ <approx>` eyeball is present only for non-integer values; the trailing `[dim]`
  is decorative. `:approx` is nil when the literal carries no `≈`."
  [s]
  (let [[lhs rhs] (str/split s #" ≈ " 2)                ; rhs = \"<approx> [dim]\" | nil
        lhs       (str/trim (if rhs
                              lhs                        ; ≈ present: [dim] rides on rhs
                              (let [i (str/index-of lhs " [")]   ; no ≈: strip the [dim] here
                                (if i (subs lhs 0 i) lhs))))
        sp        (str/index-of lhs " ")]
    {:exact    (read-string (if sp (subs lhs 0 sp) lhs))
     :unit-str (when sp (str/trim (subs lhs (inc sp))))
     :approx   (when rhs
                 (let [rhs (str/trim rhs), i (str/index-of rhs " ")]
                   (Double/parseDouble (if i (subs rhs 0 i) rhs))))}))

(defn- read-precise
  "`<exact> <formula> [≈ <approx>] [dim]` → a PreciseQuantity, rebuilt from its display
  formula. When the literal carries a `≈ <approx>` eyeball (non-integer values) it is
  re-checked against the exact value; a whole number omits it, and skips the check."
  [s]
  (let [{:keys [exact unit-str approx]} (parse-literal s)
        qty (if (seq unit-str) (reconstruct exact unit-str) (q/scalar exact))]
    (when approx (verify-approx! (q/display-value qty) approx s))
    qty))

(defn- read-approx
  "`<bigdecimal> <formula> [dim]` (leading `≈` already stripped) → an ApproxQuantity.
  The value is parsed with `bigdec` from its string — not `read-string`, which would
  read a bare decimal as a lossy Double — and multiplied by the reconstructed compound;
  the approx operand promotes the product. No re-check: the value *is* the truth."
  [s]
  (let [i        (str/index-of s " [")               ; trailing [dim] is decorative
        lhs      (str/trim (if i (subs s 0 i) s))
        sp       (str/index-of lhs " ")
        value    (bigdec (if sp (subs lhs 0 sp) lhs))
        unit-str (when sp (str/trim (subs lhs (inc sp))))
        compound (if (seq unit-str) (reconstruct 1 unit-str) (q/scalar 1))]
    (q/qmul (q/quantity value []) compound)))

(defn read-quantity
  "Data-reader for `#commensura/quantity \"…\"` → an anonymous Quantity. A leading `≈`
  marks an approximate value (an ApproxQuantity from the literal's BigDecimal); else the
  exact value is the source of truth (a PreciseQuantity rebuilt from its display formula)."
  [s]
  (let [s (str/trim s)]
    (if (str/starts-with? s "≈ ")
      (read-approx (subs s 2))
      (read-precise s))))

(defn read-unit
  "Data-reader for `#commensura/unit \"[≈ ]<name> [dim]\"` → the named Unit (precise or
  approximate), resolved through the registry (its defining `defunit` must have run).
  The leading `≈` marker and trailing `[dim]` are decorative — only the name matters."
  [s]
  (let [s   (str/trim s)
        s   (if (str/starts-with? s "≈ ") (str/trim (subs s 2)) s)
        end (or (str/index-of s " [") (count s))
        nm  (str/trim (subs s 0 end))]
    (or (registry/lookup-unit nm)
        (throw (ex-info "unknown unit in #commensura/unit literal" {:unit nm :literal s})))))
