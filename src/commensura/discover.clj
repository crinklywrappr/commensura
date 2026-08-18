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
    (d/search-units \"volt\")            ;=> [\"volt\" \"abvolt\" \"intvolt\" \"statvolt\" \"thermalvolt\" \"electronvolt\"]
    (d/units-of-dimension u/foot)       ;=> [\"actus\" \"angstrom\" … \"foot\" … \"meter\" … \"mile\" …]  (all lengths)
    (d/describe (c/per u/mile u/hour))  ;=> {:value \"1 mile/hour [velocity]\" :dimensions {:length 1 :time -1} :dimension-name \"velocity\"}"
  (:require [clojure.string :as str]
            [commensura.quantity :as q]
            [commensura.interval :as iv]
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

(defn- interval-dims
  "The shared dimensions of an interval's parts (lo, hi, and the main value if present), throwing
  if they disagree — which a constructor-built interval never does."
  [x]
  (let [dimset (into #{} (map q/dims) (remove nil? [(iv/lo x) (iv/hi x) (iv/main-value x)]))]
    (if (= 1 (count dimset))
      (first dimset)
      (throw (ex-info "interval parts have differing dimensions" {:dimensions dimset})))))

(defn- ->dims
  "Coerce a dimension spec to a canonical dims-map: a dims-map (`{:length 1}`), a commensura
  unit/quantity/interval (its dimensions), or a human dimension name (`\"velocity\"` — reverse-
  looked up, with a `did you mean?` on a miss)."
  [d]
  (cond
    ;; interval first (its parts share one dimension); then unit/quantity — records also satisfy
    ;; `map?`, so both must precede the dims-map branch
    (iv/interval? d) (interval-dims d)
    (q/displayable? d) (q/dims d)
    (and (map? d) (not (instance? clojure.lang.IRecord d))) (canonical d)
    (string? d) (or (name->dims d)
                    (let [near (suggest/nearest d (vals (registry/all-dimensions)))]
                      (throw (ex-info (cond-> (str "unknown dimension name " (pr-str d))
                                        (seq near) (str " — did you mean " (str/join ", " near) "?"))
                                      (cond-> {:dimension d} (seq near) (assoc :suggestions near))))))
    :else (throw (ex-info "expected a dims-map, dimension name, or commensura unit/quantity/interval"
                          {:got d}))))

(defn units-of-dimension
  "Sorted names of registered units whose canonical dimensions equal `d`. `d` may be a dims-map
  (`{:length 1}`), a commensura unit/quantity/interval (its dimensions are used — an interval's
  parts share one dimension), or a human dimension name (`\"velocity\"`). Reflects live registrations."
  [d]
  (let [dims (->dims d)]
    (->> (registry/all-units)
         (keep (fn [[nm u]] (when (= (q/dims u) dims) nm)))
         (sort-by (fn [s] [(str/lower-case s) s]))   ; case-insensitive, exact name as tiebreak
         vec)))

(defn search-units
  "Registered unit names matching `query` — a case-insensitive substring, or a regex `Pattern`
  (matched with `re-find`). A substring search is ranked by closeness to the query (edit distance,
  so an exact name leads and the tightest wrappers follow — `\"volt\"` → `volt`, then `abvolt`, …);
  a regex, having no query string to rank against, is returned alphabetically. Reflects live
  registrations."
  [query]
  (if (instance? java.util.regex.Pattern query)
    (->> (keys (registry/all-units))
         (filter #(re-find query %))
         (sort-by (fn [s] [(str/lower-case s) s]))   ; no query string to rank by → case-insensitive alpha
         vec)
    (let [q      (str query)
          needle (str/lower-case q)]
      (->> (keys (registry/all-units))
           (filter #(str/includes? (str/lower-case %) needle))
           ;; rank by edit distance to the query (exact = 0 leads), then shorter, then alpha
           (sort-by (fn [s] [(suggest/distance q s) (count s) (str/lower-case s) s]))
           vec))))

(defn describe
  "A plain-data description of a commensura unit or quantity — or a registered unit *name*
  (string), so `(map describe (search-units \"volt\"))` just works. For a string, the exact
  unit is resolved; an unknown name throws (with a `did you mean?`).

  Returns a map with `:value` (display string), `:dimensions` (canonical dims), `:dimension-name`
  (the human name — matching `units-of-dimension`'s input), `:namespace` (where a `defunit` unit was
  defined), and `:doc` (its docstring). Keys whose value would be nil are omitted, so the map stays
  tight — an unnamed base dimension drops `:dimension-name`; a unit not made with `defunit` (a currency
  fn, a resolver-minted unit, or a bare quantity) drops `:namespace`; a doc-less unit drops `:doc`."
  [x]
  (let [u (cond
            (string? x)
            (or (registry/resolve-unit x)
                (let [near (suggest/nearest x (keys (registry/all-units)))]
                  (throw (ex-info (cond-> (str "no registered unit named " (pr-str x))
                                    (seq near) (str " — did you mean " (str/join ", " near) "?"))
                                  (cond-> {:unit x} (seq near) (assoc :suggestions near))))))
            (q/displayable? x) x
            :else (throw (ex-info "describe expects a commensura unit/quantity or a unit name"
                                  {:got x})))
        dims (q/dims u)]
    (into {} (remove (comp nil? val))                 ; drop nil keys (unnamed dim / non-defunit / doc-less)
          {:value          (str u)
           :dimensions     dims
           :dimension-name (registry/lookup-dimension dims)
           :namespace      (:ns  (meta u))            ; where a `defunit` unit was defined
           :doc            (:doc (meta u))})))
