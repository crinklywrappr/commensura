;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.registry
  "A global name -> unit table, in the spirit of Frink's single global unit
  namespace. `defunit` registers every unit here (builtins auto-register when
  `commensura.units` loads); the `#commensura/quantity` reader resolves unit names here,
  so *user*-defined units reify too — not just builtins.

  Trade-off (documented): this is global mutable state. Unit *vars* remain
  lexically namespaced and unaffected — only the string-keyed lookup / literal
  reification path is global, and redefining a name warns (last writer wins).
  For cold deserialization, the defining `defunit` must have run before a literal
  that names its unit is read.")

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
  "Empty the registry (for test isolation)."
  []
  (reset! units {}))
