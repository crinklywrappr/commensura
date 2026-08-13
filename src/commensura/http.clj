;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.http
  "Shared HTTP+JSON helpers for the live data sources (FRED CPI, CurrencyFreaks). http-kit for the
  request, `clojure.data.json` for the body. Failures throw an ex-info tagged `:remote-error` so
  callers can decide whether to fall back."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [org.httpkit.client :as http]))

(defn decimal->ratio
  "A decimal (or integer) value string -> an exact rational, or nil for anything that isn't a number
  (e.g. FRED's missing-value \".\"). Whole numbers may arrive without a decimal point (\"14\")."
  [s]
  (let [s (str/trim (or s ""))]
    (when (re-matches #"[-+]?\d+(?:\.\d+)?" s)
      (if (str/includes? s ".")
        (let [[_ sign whole frac] (re-matches #"([-+]?)(\d*)\.(\d+)" s)
              num (bigint (str whole frac))
              den (apply * 1N (repeat (count frac) 10N))
              v   (/ num den)]
          (if (= sign "-") (- v) v))
        (bigint s)))))

(defn get-json
  "GET `url` with `query-params` (a string->string map) and parse the JSON body into a Clojure map
  (keyword keys). Throws an ex-info tagged `:remote-error` on a transport error or non-200 status."
  [url query-params]
  (let [{:keys [status body error]} @(http/get url {:query-params query-params})]
    (cond
      error             (throw (ex-info "HTTP request failed" {:remote-error true :url url} error))
      (not= 200 status) (throw (ex-info "HTTP request failed" {:remote-error true :url url :status status}))
      :else             (json/read-str body :key-fn keyword))))
