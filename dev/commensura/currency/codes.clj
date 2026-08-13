;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.currency.codes
  "DEV-ONLY. Fetch CurrencyFreaks' supported-currencies (NO key required) into
  `resources/commensura/currency-codes.edn` — the set of AVAILABLE currency codes. This is public
  identifier metadata (not rates), and it drives both the generator (`commensura.currency.gen`) and
  the runtime `supported?` predicate."
  (:require [commensura.http :as http]
            [clojure.pprint :as pp]))

(def ^:private supported-url "https://api.currencyfreaks.com/v2.0/supported-currencies")
(def ^:private out-path "resources/commensura/currency-codes.edn")

(defn fetch-codes
  "The sorted set of AVAILABLE currency codes from CurrencyFreaks (no key)."
  []
  (into (sorted-set)
        (comp (filter #(= "AVAILABLE" (:status %))) (map :currencyCode))
        (vals (:supportedCurrenciesMap (http/get-json supported-url {})))))

(defn codes!
  "Build task entry (clojure -X:build currency-codes): regenerate currency-codes.edn."
  [& _]
  (let [codes (fetch-codes)]
    (spit out-path (with-out-str (pp/pprint codes)))
    (println "wrote" out-path "-" (count codes) "codes")
    codes))
