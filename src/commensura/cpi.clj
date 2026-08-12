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
            [commensura.cpi.fred :as fred]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

;; ---- data source (injectable; default = the shipped cpi.edn) ----
(def ^:private shipped
  (delay (edn/read-string (slurp (io/resource "commensura/cpi.edn")))))

(defn shipped-source
  "The default CPI source: the data shipped in `resources/commensura/cpi.edn`."
  []
  @shipped)

(def ^:dynamic *cpi-source*
  "The active CPI data source — a 0-arg fn returning the cpi data map. Defaults to the shipped
  snapshot; `use-live!` / `with-live` swap in live FRED data."
  shipped-source)

(defn- cpi-data
  "The active source's data map — falling back to the shipped snapshot if a live source errors."
  []
  (try (*cpi-source*)
       (catch clojure.lang.ExceptionInfo e
         (if (:cpi/source-error (ex-data e)) @shipped (throw e)))))

(defn- cpi-at
  "CPI index in `d` for a period: [year] → annual average, [year month] → that month; throws if absent."
  [d period]
  (or (case (count period)
        1 (get-in d [:annual (first period)])
        2 (get-in d [:monthly period]))
      (throw (ex-info "no CPI data for that period"
                      {:period period :range [(first (keys (:annual d))) (:current d)]}))))

(defn- factor
  "Base magnitude of a period dollar (in current dollars): CPI_current / CPI_period, taken from
  one consistent snapshot of the active source."
  [period]
  (let [d (cpi-data)]
    (/ (get-in d [:monthly (:current d)]) (cpi-at d period))))

(defn- mint
  "Create the period unit named `nm` from the *active* source. Deliberately uncached — the source
  (and its `:current`) can change, so each call reflects the current data. Cheap, transient, and
  round-trips via structural `=`."
  [nm period scale]
  (q/unit nm (* scale (factor period)) {:currency 1}))

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

;; ---- live source control ----
(defn use-live!
  "Globally switch to a cached live FRED source (their key; defaults to the `FRED_API_KEY` env var).
  A fetch failure falls back to the shipped snapshot. Historical months never change — only the
  latest available month (and thus `:current`) advances beyond what shipped."
  ([] (use-live! (System/getenv "FRED_API_KEY")))
  ([api-key]
   (when (str/blank? api-key)
     (throw (ex-info "no FRED API key (set FRED_API_KEY, or pass one)" {})))
   (alter-var-root #'*cpi-source* (constantly (fred/source api-key)))))

(defn use-shipped!
  "Revert to the shipped `cpi.edn` source (undo `use-live!`)."
  []
  (alter-var-root #'*cpi-source* (constantly shipped-source)))

(defmacro with-live
  "Evaluate `body` with a live FRED source bound for the current thread."
  [api-key & body]
  `(binding [*cpi-source* (fred/source ~api-key)] ~@body))
