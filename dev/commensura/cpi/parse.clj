;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.cpi.parse
  "DEV-ONLY. Shared parsing/building for the two CPI acquisition pipelines: the FRED 'Table
  Data' HTML (Frink's bundled backup, pipeline 2 — parsed with **jsoup**) and the FRED API JSON
  (pipeline 1 — parsed with **data.json**). Both reduce to a seq of `[[year month] rational]`
  observations, which `build-cpi` turns into the shipped `cpi.edn` shape. Values are kept **exact
  rationals** (CPIAUCNS is NSA — never revised)."
  (:require [clojure.string :as str]
            [clojure.data.json :as json])
  (:import [org.jsoup Jsoup]))

(defn value->ratio
  "A FRED CPI value string -> an exact rational, or nil for anything that isn't a number (FRED
  writes missing values as \".\"; header/blank cells appear in the HTML). Whole numbers arrive
  without a decimal point (\"14\"); decimals to 3 places (\"9.800\")."
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

(defn- date->period [s]
  (when-let [[_ y m] (re-matches #"(\d{4})-(\d{2})-\d{2}" s)]
    [(Long/parseLong y) (Long/parseLong m)]))

(defn parse-html
  "The pinned FRED 'Table Data' HTML -> seq of `[[year month] rational]`. Rows are
  `<tr><th>YYYY-MM-01</th><td>value</td></tr>`; parsed with jsoup and selected by tag."
  [html]
  (for [row  (.select (Jsoup/parse html) "tr")
        :let [period (some-> (.selectFirst row "th") .text str/trim date->period)
              value  (some-> (.selectFirst row "td") .text value->ratio)]
        :when (and period value)]
    [period value]))

(defn parse-fred-json
  "A FRED `/series/observations` JSON body -> seq of `[[year month] rational]` (missing values
  are dropped)."
  [body]
  (for [{:strs [date value]} (get (json/read-str body) "observations")
        :let [period (date->period date), r (value->ratio value)]
        :when (and period r)]
    [period r]))

(defn build-cpi
  "Observations (`[[y m] rational]`) -> the `cpi.edn` map: exact monthly series, annual averages
  (mean of a year's available months), and `:current` (the latest period = the 'current dollar')."
  [observations]
  (let [monthly (into (sorted-map) observations)
        by-year (group-by (comp first key) monthly)
        annual  (into (sorted-map)
                      (map (fn [[y kvs]] [y (/ (reduce + (map val kvs)) (count kvs))]))
                      by-year)]
    {:series  "CPIAUCNS"
     :source  "U.S. BLS via FRED (St. Louis Fed)"
     :current (key (last monthly))
     :monthly monthly
     :annual  annual}))
