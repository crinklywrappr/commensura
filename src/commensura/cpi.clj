;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.cpi
  "Historical U.S. purchasing power (CPI). A *period dollar* is a unit worth
  `(CPI_current / CPI_period) · dollar`, so it composes with the core verbs and, because two of
  them share the `dollar` basis, `to` between them converts across years — inflation-adjustment:

    (require '[commensura.cpi :as cpi]
             '[commensura.core :as c]
             '[commensura.units :as u])
    (c/to (u/dollar 1250) (cpi/usd 1913))                 ; 1913 dollars → current dollars
    (c/to (c/by 50 (cpi/cent 1955 11)) (cpi/usd 1985 10)) ;=> 2.020446096654275 (exact)

  Data (BLS CPI-U series CPIAUCNS, via FRED) ships as `resources/commensura/cpi.edn` — exact
  rationals; `:current` is the latest shipped period. `dev/commensura/cpi/{fetch,frink}.clj`
  regenerate it (FRED API) / a Frink-parity oracle fixture. Live refresh is M4.2."
  (:require [commensura.quantity :as q]
            [commensura.registry :as registry]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def ^:private data (delay (edn/read-string (slurp (io/resource "commensura/cpi.edn")))))

(defn- cpi-current [] (get-in @data [:monthly (:current @data)]))

(defn- cpi-at
  "CPI index for a period: [year] → annual average, [year month] → that month; throws if absent."
  [period]
  (or (case (count period)
        1 (get-in @data [:annual (first period)])
        2 (get-in @data [:monthly period]))
      (throw (ex-info "no CPI data for that period"
                      {:period period
                       :range [(first (keys (:annual @data))) (:current @data)]}))))

(defn- factor
  "Base magnitude of a period dollar (in current dollars): CPI_current / CPI_period."
  [period]
  (/ (cpi-current) (cpi-at period)))

(defn- mint
  "The period unit named `nm`, reusing the registry as the cache: return it if already registered,
  else create and register it once (so it also round-trips from its `#commensura/unit` literal)."
  [nm period scale]
  (or (registry/lookup-unit nm)
      (registry/register-unit! nm (q/unit nm (* scale (factor period)) {:currency 1}))))

(defn usd
  "A historical U.S. dollar as a callable unit: `(usd 1960)` (annual average) or
  `(usd 1969 8)` (a specific month, 1913+). Equals `(CPI_current / CPI_period) · dollar`."
  ([year]       (mint (str "dollar_" year) [year] 1))
  ([year month] (mint (format "dollar_%d_%02d" year month) [year month] 1)))

(defn cent
  "A historical U.S. cent (= 1/100 · `usd`)."
  ([year]       (mint (str "cent_" year) [year] 1/100))
  ([year month] (mint (format "cent_%d_%02d" year month) [year month] 1/100)))

(def ^:private name-re #"(?i)(dollars?|usd|cents?)_(\d{4})(?:_(\d{2}))?")

(defn historical-name?
  "True if `s` is a historical-currency unit name — `dollar_1960` / `dollars_1960` / `USD_1960` /
  `cent_1910` / `cents_1965_10` (case-insensitive). The `#commensura/unit` reader uses this to
  reify these commensura-provided units on demand."
  [s]
  (boolean (re-matches name-re s)))

(defn unit
  "Reify a historical-currency name (see `historical-name?`) into the corresponding unit."
  [s]
  (if-let [[_ kind y m] (re-matches name-re s)]
    (let [f (if (str/starts-with? (str/lower-case kind) "c") cent usd)   ; only cent(s) starts with c
          y (parse-long y)]
      (if m (f y (parse-long m)) (f y)))
    (throw (ex-info "not a historical-currency name" {:name s}))))
