;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.cpi.frink
  "DEV-ONLY. Pipeline (2): parse the pinned Frink CPIAUCNS HTML into the committed oracle fixture
  `test/commensura/frink_cpi.edn`. Also bootstraps `resources/commensura/cpi.edn` from the same
  file so the feature is testable offline — but that copy is only as complete as Frink's backup
  (FRED's 1000-row default view = 1913-01 → 1996-04); run `cpi-fetch` (FRED) for current data."
  (:require [commensura.cpi.parse :as p]
            [clojure.pprint :as pp]))

(def ^:private default-html "dev-resources/frink/CPIAUCNS")
(def ^:private fixture-path "test/commensura/frink_cpi.edn")
(def ^:private cpi-path     "resources/commensura/cpi.edn")

(defn- pp-spit [path x] (spit path (with-out-str (pp/pprint x))))

(defn gen-fixture!
  "Write the oracle fixture — the raw monthly `{[y m] rational}` from Frink's HTML."
  [& [html-path]]
  (let [monthly (into (sorted-map) (p/parse-html (slurp (or html-path default-html))))]
    (pp-spit fixture-path monthly)
    (println "wrote" fixture-path "-" (count monthly) "months,"
             (first (keys monthly)) "->" (last (keys monthly)))
    monthly))

(defn bootstrap-cpi!
  "OFFLINE FALLBACK ONLY: bootstrap the shipped `cpi.edn` from Frink's HTML when you can't reach
  FRED. It's only as recent as the backup (1996). Normally `cpi-fetch` (FRED) owns `cpi.edn`."
  [& [html-path]]
  (let [cpi (p/build-cpi (p/parse-html (slurp (or html-path default-html))))]
    (pp-spit cpi-path cpi)
    (println "wrote" cpi-path "(offline bootstrap) -" (count (:monthly cpi)) "months, current" (:current cpi))
    cpi))

(defn frink!
  "Build task entry (clojure -X:build cpi-frink): regenerate the oracle fixture from Frink's HTML.
  (`cpi.edn` is owned by `cpi-fetch`; see `bootstrap-cpi!` for the offline fallback.)"
  [& _]
  (gen-fixture!))
