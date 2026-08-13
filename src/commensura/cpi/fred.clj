;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.cpi.fred
  "Fetch CPI (BLS series CPIAUCNS) from the FRED API and build the cpi data map — shared by the dev
  refresh task (`commensura.cpi.fetch`) and the runtime live source (`commensura.cpi`). HTTP+JSON via
  `commensura.http`. Values are kept **exact rationals** (CPIAUCNS is not seasonally adjusted, so it is
  never revised). Fetch/parse failures throw an ex-info tagged `:cpi/source-error` so callers can fall
  back to the shipped data."
  (:require [commensura.http :as http]
            [taoensso.trove :as trove]))

(defn date->period
  "A \"YYYY-MM-DD\" date -> [year month], or nil."
  [s]
  (when-let [[_ y m] (re-matches #"(\d{4})-(\d{2})-\d{2}" s)]
    [(Long/parseLong y) (Long/parseLong m)]))

(defn build-cpi
  "Observations (`[[y m] rational]`) -> the cpi data map: exact monthly series, annual averages (mean
  of a year's available months), and `:current` (the latest period = the 'current dollar')."
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

(def ^:private observations-url "https://api.stlouisfed.org/fred/series/observations")

(defn fetch-cpi
  "Fetch the full CPIAUCNS series from FRED and build the cpi data map. Throws an ex-info tagged
  `:cpi/source-error` on any network/parse failure."
  [api-key]
  (try
    (->> (http/get-json observations-url {"series_id"         "CPIAUCNS"
                                          "file_type"         "json"
                                          "observation_start" "1913-01-01"
                                          "api_key"           api-key})
         :observations
         (keep (fn [{:keys [date value]}]
                 (when-let [period (date->period date)]
                   (when-let [r (http/decimal->ratio value)] [period r]))))
         build-cpi)
    (catch Exception e
      (throw (ex-info "FRED CPI fetch/parse failed" {:cpi/source-error true} e)))))

(def ^:private default-ttl-ms (* 6 60 60 1000))   ; CPI updates monthly — 6h is plenty

(defn source
  "A cached live-FRED CPI source: a 0-arg fn returning the cpi data map, re-fetching at most once per
  `ttl-ms` (default 6h). A fetch failure is negative-cached for the same window and re-thrown (tagged
  `:cpi/source-error`) so the caller can fall back to shipped data — without hammering FRED."
  ([api-key] (source api-key default-ttl-ms))
  ([api-key ttl-ms]
   (let [cache (atom nil)]                          ; {:at ms :data map|nil}
     (fn []
       (let [{:keys [at data]} @cache, now (System/currentTimeMillis)]
         (if (and at (< (- now at) ttl-ms))
           (or data (throw (ex-info "FRED CPI unavailable (cached)" {:cpi/source-error true})))
           (try
             (let [d (fetch-cpi api-key)] (reset! cache {:at now :data d}) d)
             ;; warn once per real fetch attempt (not on every cached-failure call), then re-throw
             ;; so the caller can fall back to shipped data
             (catch clojure.lang.ExceptionInfo e
               (trove/log! {:level :warn
                            :id    ::fetch-failed
                            :msg   "live CPI fetch failed; falling back to shipped data"
                            :data  {:error (ex-message e) :cause (ex-cause e)}})
               (reset! cache {:at now :data nil})
               (throw e)))))))))
