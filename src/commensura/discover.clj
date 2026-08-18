;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.discover
  "Read-only discovery over the registered units and dimensions — \"what units have this
  dimension?\", \"search unit names\", \"what is this value?\". These are *queryable views*
  over the registry (`commensura.registry`): every fn reflects live registrations and
  returns plain data — sorted vectors of names, or maps — never the internal atoms. Reach
  here instead of filtering the registry atoms by hand.

    (require '[commensura.discover :as d] '[commensura.units :as u])
    (d/search-units \"volt\")            ;=> [\"abvolt\" \"electronvolt\" \"intvolt\" \"statvolt\" \"volt\"]
    (d/units-of-dimension u/foot)       ;=> [\"actus\" \"angstrom\" … \"foot\" … \"meter\" … \"mile\" …]  (all lengths)
    (d/describe (c/per u/mile u/hour))  ;=> {:value \"1 mile/hour [velocity]\" :dimensions {:length 1 :time -1} :dimension \"velocity\"}"
  (:require [clojure.string :as str]
            [commensura.quantity :as q]
            [commensura.registry :as registry]
            [commensura.suggest :as suggest]))

(defn- canonical
  "Drop zero exponents so a dims-map matches a unit's stored, canonical dimensions."
  [d]
  (into {} (remove (comp zero? val)) d))

(defn- name->dims
  "The dims-map registered under human dimension name `s`, or nil."
  [s]
  (some (fn [[dm nm]] (when (= nm s) dm)) (registry/all-dimensions)))

(defn- ->dims
  "Coerce a dimension spec to a canonical dims-map: a dims-map (`{:length 1}`), a commensura
  unit/quantity (its dimensions), or a human dimension name (`\"velocity\"` — reverse-looked
  up, with a `did you mean?` on a miss)."
  [d]
  (cond
    ;; a unit/quantity first — records also satisfy `map?`, so this must precede the dims-map branch
    (satisfies? q/Displayable d) (q/dims d)
    (and (map? d) (not (instance? clojure.lang.IRecord d))) (canonical d)
    (string? d) (or (name->dims d)
                    (let [near (suggest/nearest d (vals (registry/all-dimensions)))]
                      (throw (ex-info (cond-> (str "unknown dimension name " (pr-str d))
                                        (seq near) (str " — did you mean " (str/join ", " near) "?"))
                                      (cond-> {:dimension d} (seq near) (assoc :suggestions near))))))
    :else (throw (ex-info "expected a dims-map, dimension name, or commensura unit/quantity"
                          {:got d}))))

(defn units-of-dimension
  "Sorted names of registered units whose canonical dimensions equal `d`. `d` may be a
  dims-map (`{:length 1}`), a commensura unit/quantity (its dimensions are used), or a human
  dimension name (`\"velocity\"`). Reflects live registrations."
  [d]
  (let [dims (->dims d)]
    (->> (registry/all-units)
         (keep (fn [[nm u]] (when (= (q/dims u) dims) nm)))
         (sort-by (fn [s] [(str/lower-case s) s]))   ; case-insensitive, exact name as tiebreak
         vec)))

(defn search-units
  "Sorted registered unit names matching `query` — a case-insensitive substring, or a regex
  `Pattern` (matched with `re-find`). Reflects live registrations."
  [query]
  (let [match? (if (instance? java.util.regex.Pattern query)
                 #(boolean (re-find query %))
                 (let [needle (str/lower-case (str query))]
                   #(str/includes? (str/lower-case %) needle)))]
    (->> (keys (registry/all-units))
         (filter match?)
         (sort-by (fn [s] [(str/lower-case s) s]))   ; case-insensitive, exact name as tiebreak
         vec)))

(defn describe
  "A plain-data description of a commensura unit or quantity `x`: its display string, its
  canonical dimensions, and the human dimension name if one is registered — `nil` for the
  base dimensions (e.g. bare `{:length 1}`), which the table doesn't name."
  [x]
  (when-not (satisfies? q/Displayable x)
    (throw (ex-info "describe expects a commensura unit or quantity" {:got x})))
  (let [dims (q/dims x)]
    {:value      (str x)
     :dimensions dims
     :dimension  (registry/lookup-dimension dims)}))

(defn dimensions
  "All registered dimension → human-name entries as `[dims-map name]` pairs, sorted by name.
  Base dimensions may be unnamed and simply don't appear here."
  []
  (->> (registry/all-dimensions)
       (sort-by val)
       (mapv (fn [[dm nm]] [dm nm]))))
