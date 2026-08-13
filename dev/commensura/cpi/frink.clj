;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.cpi.frink
  "DEV-ONLY. Pipeline (2): parse the pinned Frink CPIAUCNS HTML (jsoup) into the committed oracle
  fixture `test/commensura/frink_cpi.edn`. Reuses `commensura.cpi.fred` for `value->ratio`/`build-cpi`.
  `bootstrap-cpi!` is an OFFLINE FALLBACK for `cpi.edn` (only as recent as Frink's backup, 1996);
  normally `cpi-fetch` (FRED) owns `cpi.edn`."
  (:require [commensura.cpi.fred :as fred]
            [commensura.http :as http]
            [clojure.string :as str]
            [clojure.pprint :as pp])
  (:import [org.jsoup Jsoup]))

(def ^:private default-html "dev-resources/frink/CPIAUCNS")
(def ^:private fixture-path "test/commensura/frink_cpi.edn")
(def ^:private cpi-path     "resources/commensura/cpi.edn")

(defn parse-html
  "The FRED 'Table Data' HTML -> seq of `[[year month] rational]`; rows are
  `<tr><th>YYYY-MM-01</th><td>value</td></tr>`, selected by tag."
  [html]
  (for [row  (.select (Jsoup/parse html) "tr")
        :let [period (some-> (.selectFirst row "th") .text str/trim fred/date->period)
              value  (some-> (.selectFirst row "td") .text http/decimal->ratio)]
        :when (and period value)]
    [period value]))

(defn- pp-spit [path x] (spit path (with-out-str (pp/pprint x))))

(defn gen-fixture!
  "Write the oracle fixture — the raw monthly `{[y m] rational}` from Frink's HTML."
  [& [html-path]]
  (let [monthly (into (sorted-map) (parse-html (slurp (or html-path default-html))))]
    (pp-spit fixture-path monthly)
    (println "wrote" fixture-path "-" (count monthly) "months,"
             (first (keys monthly)) "->" (last (keys monthly)))
    monthly))

(defn bootstrap-cpi!
  "OFFLINE FALLBACK ONLY: bootstrap `cpi.edn` from Frink's HTML when you can't reach FRED."
  [& [html-path]]
  (let [cpi (fred/build-cpi (parse-html (slurp (or html-path default-html))))]
    (pp-spit cpi-path cpi)
    (println "wrote" cpi-path "(offline bootstrap) -" (count (:monthly cpi)) "months, current" (:current cpi))
    cpi))

(defn frink!
  "Build task entry (clojure -X:build cpi-frink): regenerate the oracle fixture from Frink's HTML."
  [& _]
  (gen-fixture!))
