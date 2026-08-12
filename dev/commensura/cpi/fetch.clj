;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.cpi.fetch
  "DEV-ONLY. Pipeline (1): (re)generate `resources/commensura/cpi.edn` from the FRED API. This is a
  MAINTAINER action — it needs network and a free FRED key from the `FRED_API_KEY` env var (never
  committed). It reuses `commensura.cpi.fred` (the same fetch/parse the runtime live source uses),
  so the FRED numerics live in exactly one place."
  (:require [commensura.cpi.fred :as fred]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

(def ^:private cpi-path "resources/commensura/cpi.edn")

(defn fetch!
  "Build task entry (FRED_API_KEY=… clojure -X:build cpi-fetch): regenerate cpi.edn from FRED."
  [& _]
  (let [key (System/getenv "FRED_API_KEY")]
    (when (str/blank? key)
      (throw (ex-info "FRED_API_KEY is not set (get a free key at fredaccount.stlouisfed.org)" {})))
    (let [cpi (fred/fetch-cpi key)]
      (spit cpi-path (with-out-str (pp/pprint cpi)))
      (println "wrote" cpi-path "from FRED -" (count (:monthly cpi)) "months, current" (:current cpi))
      cpi)))
