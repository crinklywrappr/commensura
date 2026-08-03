;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.reader
  "Reifies the `#commensura/quantity \"…\"` tagged literal (registered in
  resources/data_readers.clj) back into a Quantity.

  The EXACT value in the literal is the source of truth. The printed
  approximation is re-derived from that exact value and checked against the
  literal's — throwing if they disagree beyond `*approx-tolerance*`. This rules
  out junk data and values that would come out differently under JVM/library
  drift. (Only builtin units resolve by name; a user `defunit` name will not.)"
  (:require [clojure.string :as str]
            [commensura.quantity :as q]
            [commensura.dimensions :as dims]
            [commensura.registry :as registry]
            [commensura.units]))   ; load so builtin units are registered

(def ^:dynamic *approx-tolerance*
  "Maximum *relative* difference tolerated between the literal's printed
  approximation and the value re-derived from its exact part. Full-precision
  doubles agree to ~15 digits; 1e-9 absorbs realistic JVM/libm/format drift while
  still catching junk data."
  1e-9)

(def ^:private name->dims
  (delay (reduce-kv (fn [m d n] (assoc m n d)) {} dims/names)))

(defn- resolve-dims [dim-str]
  (cond
    (= dim-str "[dimensionless]") {}
    (str/starts-with? dim-str "{") (read-string dim-str)                 ; raw dims map
    (str/starts-with? dim-str "[") (let [nm (subs dim-str 1 (dec (count dim-str)))]
                                     (or (@name->dims nm) {(keyword nm) 1})) ; named / base dim
    :else (throw (ex-info "unrecognized dimension in #commensura/quantity literal"
                          {:dim dim-str}))))

(defn- verify-approx! [re-derived parsed literal]
  (let [re (double re-derived), p (double parsed)
        scale (max 1.0 (Math/abs re))]
    (when (> (Math/abs (- re p)) (* *approx-tolerance* scale))
      (throw (ex-info "#commensura/quantity approximation inconsistent with its exact value"
                      {:literal literal :in-literal p :re-derived re
                       :tolerance *approx-tolerance*})))))

(defn read-quantity
  "Data-reader for `#commensura/quantity \"<printed form>\"`."
  [s]
  (let [[lhs rhs] (str/split s #" ≈ " 2)]
    (when (nil? rhs) (throw (ex-info "malformed #commensura/quantity literal" {:literal s})))
    (let [ltoks   (str/split (str/trim lhs) #"\s+")
          exact   (read-string (first ltoks))
          sym     (second ltoks)
          rhs     (str/trim rhs)
          i       (str/index-of rhs " ")
          approx  (Double/parseDouble (subs rhs 0 i))
          dim-str (str/trim (subs rhs (inc i)))
          qty     (if sym
                    (if-let [u (registry/lookup sym)]
                      (u exact)                               ; exact display value in that unit
                      (throw (ex-info "unknown unit in #commensura/quantity literal"
                                      {:unit sym :literal s})))
                    (q/quantity exact (resolve-dims dim-str)))]
      (verify-approx! (q/display-value qty) approx s)
      qty)))
