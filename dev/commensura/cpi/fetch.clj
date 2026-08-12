;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.cpi.fetch
  "DEV-ONLY. Pipeline (1): fetch the full CPIAUCNS series from the FRED API and (re)generate the
  shipped `resources/commensura/cpi.edn`. This is a MAINTAINER action — it needs network and a
  free FRED API key from the `FRED_API_KEY` env var (never committed). The FRED API returns the
  whole series (unlike Frink's 1000-row HTML backup), so this is what produces current data.

  Uses only JDK `java.net.http` + regex (no HTTP/JSON deps). The GET below uses the classic
  `?api_key=` form (which the 32-hex key uses); FRED v2's `Authorization: Bearer <key>` is an
  equivalent alternative — swap `auth` if you use it."
  (:require [commensura.cpi.parse :as p]
            [clojure.string :as str]
            [clojure.pprint :as pp])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]))

(def ^:private cpi-path "resources/commensura/cpi.edn")

(defn- get! [^String url]
  (let [req  (-> (HttpRequest/newBuilder (URI/create url)) (.GET) (.build))
        resp (.send (HttpClient/newHttpClient) req (HttpResponse$BodyHandlers/ofString))]
    (when-not (= 200 (.statusCode resp))
      (throw (ex-info "FRED request failed" {:status (.statusCode resp) :body (subs (.body resp) 0 (min 300 (count (.body resp))))})))
    (.body resp)))

(defn fetch-observations
  "The FRED CPIAUCNS observations JSON body (full series from 1913)."
  [api-key]
  (get! (str "https://api.stlouisfed.org/fred/series/observations"
             "?series_id=CPIAUCNS&file_type=json&observation_start=1913-01-01"
             "&api_key=" api-key)))

(defn fetch!
  "Build task entry (FRED_API_KEY=… clojure -X:build cpi-fetch): regenerate cpi.edn from FRED."
  [& _]
  (let [key (System/getenv "FRED_API_KEY")]
    (when (str/blank? key)
      (throw (ex-info "FRED_API_KEY is not set (get a free key at fredaccount.stlouisfed.org)" {})))
    (let [cpi (p/build-cpi (p/parse-fred-json (fetch-observations key)))]
      (spit cpi-path (with-out-str (pp/pprint cpi)))
      (println "wrote" cpi-path "from FRED -" (count (:monthly cpi)) "months, current" (:current cpi))
      cpi)))
