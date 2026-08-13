;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.currency.rates
  "Live currency exchange rates from CurrencyFreaks (USD base). The machinery behind the generated
  `commensura.currency` unit fns: fetch (cached, TTL) → an exact `code -> USD-value` mapping → a unit.

  Live-only: rates aren't shipped (they're volatile and proprietary), so this needs the user's own
  key — set `CURRENCYFREAKS_API_KEY` (a free tier exists). A missing key or a fetch failure throws.
  Rates are kept exact (rationals); each currency is worth `(usd-value code) · dollar`."
  (:require [commensura.http :as http]
            [commensura.quantity :as q]
            [commensura.registry :as registry]
            [clojure.core.memoize :as memo]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def ^:private rates-url "https://api.currencyfreaks.com/v2.0/rates/latest")

(def ttl-ms
  "Rate-cache TTL in ms (default 12h). CurrencyFreaks' free tier is daily / 1,000 calls a month, so a
  long TTL keeps well under quota; redefine it if you have a faster paid plan."
  (* 12 60 60 1000))

(defn- fetch-rates
  "Fetch the current `{code -> rational}` map — units of `code` per 1 USD — from CurrencyFreaks."
  []
  (let [key (System/getenv "CURRENCYFREAKS_API_KEY")]
    (when (str/blank? key)
      (throw (ex-info "no CurrencyFreaks API key (set CURRENCYFREAKS_API_KEY)" {})))
    (into {} (keep (fn [[code v]] (when-let [r (http/decimal->ratio v)] [(name code) r]))
                   (:rates (http/get-json rates-url {"apikey" key}))))))

(def rates
  "The current `{code -> rate}` map (units of `code` per 1 USD), cached with a TTL via core.memoize.
  The memoized fn is the seam — `with-redefs` it in tests; a live fetch happens at most once per `ttl-ms`."
  (memo/ttl fetch-rates :ttl/threshold ttl-ms))

(defn usd-value
  "The USD value of one unit of `code` (= 1 / rate). `USD` is 1 without a fetch."
  [code]
  (if (= code "USD")
    1
    (if-let [r (get (rates) code)]
      (/ 1 r)
      (throw (ex-info "no exchange rate for currency" {:code code})))))

(defn unit
  "A currency Unit for `code` — callable, worth `(usd-value code) · dollar`, dims `{:currency 1}`.
  Minted fresh each call (the rate is live)."
  [code]
  (q/unit code (usd-value code) {:currency 1}))

(defn of
  "The current currency unit for `code`, or `amount` of it — what the generated `commensura.currency`
  fns delegate to. Also the way to reach a code that has no generated var."
  ([code]        (unit code))
  ([code amount] ((unit code) amount)))

(def ^:private codes
  "The shipped set of supported currency codes (drives `supported?`)."
  (delay (edn/read-string (slurp (io/resource "commensura/currency-codes.edn")))))

(defn supported?
  "Is `nm` a supported currency code?"
  [nm]
  (contains? @codes nm))

;; Register the resolver family so a currency code reifies on demand from the `#commensura/unit` /
;; `#commensura/quantity` reader (M4.3) — e.g. `#commensura/unit "EUR [currency]"`.
(registry/register-unit-resolver! supported? unit)
