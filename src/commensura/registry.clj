;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

;; TODO: [PLAN] bring the dimension registry feature-set to parity with the unit registry
;;       this requires warning messages for overwriting and similar function names
;;       e.g. register-unit!, lookup-unit, all-units, clear-units! &
;;            regist-dimension!, lookup-dimension, all-dimensions, clear-dimensions!
;;       OR just expose the two registry atoms publicly, with register-xxx! functions
;;       for appropriate logging.

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
  (:require [commensura.dimensions :as dimensions]   ; seed only
            [taoensso.trove :as trove]))

(defonce ^:private units (atom {}))

(defn register!
  "Register the unit `qty` under string `nm`; returns `qty`. The swap is atomic
  (last writer wins, matching Frink); when it replaces an existing name with a
  *different* value it warns afterward, including the old and new values."
  [nm qty]
  (let [[snapshot _] (swap-vals! units assoc nm qty)
        old          (get snapshot nm)]
    (when (and (some? old) (not= old qty))
      (trove/log! {:level :warn
                   :id ::unit-redefined
                   :msg (str "commensura unit \"" nm "\" redefined")
                   :data {:unit nm :old (into {} old) :new (into {} qty)}}))
    qty))

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
