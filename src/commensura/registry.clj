;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.registry
  "Global tables, in the spirit of Frink's single global namespace:

    * a name -> unit table — `defunit` registers every unit here (builtins
      auto-register when `commensura.units` loads); the `#commensura/quantity`
      reader resolves unit names here, so *user*-defined units reify too.
    * a dimension-map -> human-name table — seeded from the generated
      `commensura.dimensions/names` (the `|||` labels) and extended at runtime
      via `register-dimension!`, so users can name their own dimensions.

  Trade-off (documented): this is global mutable state. Unit *vars* remain
  lexically namespaced and unaffected — only the string-keyed lookup / literal
  reification path is global, and redefining a name warns (last writer wins).
  For cold deserialization, the defining `defunit` must have run before a literal
  that names its unit is read."
  (:require [commensura.dimensions :as dimensions]))   ; seed only

(defonce ^:private units (atom {}))

(defn register!
  "Register unit Quantity `qty` under string `nm`; returns `qty`. Warns (does not
  throw) when redefining an existing name to a *different* value — last writer
  wins, matching Frink."
  [nm qty]
  (when-let [prev (@units nm)]
    (when (not= prev qty)
      (binding [*out* *err*]
        (println (str "WARNING: commensura unit \"" nm "\" redefined")))))
  (swap! units assoc nm qty)
  qty)

(defn lookup
  "The unit Quantity registered under string `nm`, or nil. Also a handy
  string-keyed unit API: `(lookup \"gallon\")`."
  [nm]
  (@units nm))

(defn all
  "The whole name -> unit map (for introspection)."
  []
  @units)

(defn clear!
  "Empty the unit registry (for test isolation)."
  []
  (reset! units {}))

;; ---- dimension-name table (seeded from the generated ||| labels) ----
(defonce ^:private dim-names (atom dimensions/names))

(defn register-dimension!
  "Register (or override) the human name for a dimension map — e.g.
  `(register-dimension! {:length 4} \"quaternary space\")`. Zero exponents are
  dropped so the key matches a quantity's canonical dimensions. Returns `nm`."
  [dims nm]
  (swap! dim-names assoc (into {} (remove (comp zero? val)) dims) nm)
  nm)

(defn dimension-name
  "Human name registered for dimension map `d` (builtin or user), or nil."
  [d]
  (@dim-names d))
