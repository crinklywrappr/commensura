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
    * unit *resolvers* — `register-unit-resolver!` installs a `pred`/`dispatch` pair
      that builds a whole *family* of names on demand (e.g. `dollar_1960`); `resolve-unit`
      tries the table first, then resolvers, so families reify from the reader without
      being registered one-by-one.
    * a dimension-map -> human-name table — seeded from the generated
      `commensura.dimensions/names` (the `|||` labels) and extended at runtime
      via `register-dimension!`, so users can name their own dimensions.

  The two tables share a symmetric API — `register-{unit,dimension}!` (last-writer-
  wins, warning when a name is redefined to a *different* value), `lookup-{unit,
  dimension}`, `all-{units,dimensions}`, `clear-{units,dimensions}!`.

  The backing atoms (`units`, `dim-names`, `unit-resolvers`) are **public** — reach for
  them directly when you want bulk / `swap!` / `add-watch` / reordering access. The
  functions above are the ergonomic path that also applies the invariants (redefine
  warnings, dimension-key normalization, reseed-on-clear); the atoms trust you to know
  what you're doing.

  Trade-off (documented): this is global mutable state. Unit *vars* remain lexically
  namespaced and unaffected — only the string-keyed lookup / literal reification path is
  global, and redefining a name via `register-*!` warns (last writer wins). For cold
  deserialization, the defining `defunit` (or a resolver) must be loaded before a literal
  that names its unit is read."
  (:require [commensura.dimensions :as dimensions]   ; seed only
            [taoensso.trove :as trove]))

;; ---- shared register-with-warn ----
(defn- register*
  "Atomically assoc `k`→`v` into registry atom `a` (last writer wins); warn if it
  replaced an existing, *different* value. `kind` (\"unit\"/\"dimension\") labels the
  log id and message. Returns `v`."
  [a kind k v]
  (let [[snapshot _] (swap-vals! a assoc k v)
        old          (get snapshot k)]
    (when (and (some? old) (not= old v))
      (trove/log! {:level :warn
                   :id   (keyword "commensura.registry" (str kind "-redefined"))
                   :msg  (str "commensura " kind " " (pr-str k) " redefined")
                   :data {:kind kind :key k :old old :new v}}))
    v))

;; ---- unit table (name -> unit) ----
(defonce units (atom {}))

(defn register-unit!
  "Register unit `qty` under string `nm` (last writer wins; warns on a differing
  redefine). Returns `qty`. What `defunit` calls."
  [nm qty]
  (register* units "unit" nm qty))

(defn lookup-unit
  "The unit registered under string `nm`, or nil. A handy string-keyed unit API:
  `(lookup-unit \"gallon\")`."
  [nm]
  (@units nm))

(defn all-units
  "The whole name -> unit map (for introspection)."
  []
  @units)

(defn clear-units!
  "Empty the unit registry (for test isolation)."
  []
  (reset! units {}))

;; ---- unit resolvers (on-demand families: a pred + a dispatch) ----
(defonce unit-resolvers (atom []))

(defn register-unit-resolver!
  "Install a resolver for a *family* of units whose members are derivable from their name rather
  than registered one-by-one (e.g. the historical `dollar_1960`). `pred` is `name -> boolean`;
  `dispatch` is `name -> unit`, invoked when a name isn't in the unit table and `pred` matches.
  Resolvers are tried in registration order; the first whose `pred` matches wins, and its `dispatch`
  result — or exception — is the answer (so a matched-but-unbuildable name surfaces its own error,
  not a generic \"unknown unit\"). Complements `register-unit!`/`defunit` (a single fixed unit)."
  [pred dispatch]
  (swap! unit-resolvers conj {:pred pred :dispatch dispatch})
  nil)

(defn resolve-unit
  "Resolve a unit by name: the registered unit, else the first matching resolver's `dispatch`, else
  nil. The `#commensura/…` reader resolves through this, so builtins, user `defunit`s, and resolver
  families all reify."
  [nm]
  (or (lookup-unit nm)
      (when-let [{:keys [dispatch]} (first (filter #((:pred %) nm) @unit-resolvers))]
        (dispatch nm))))

(defn all-unit-resolvers
  "The installed resolvers, in registration order (for introspection)."
  []
  @unit-resolvers)

(defn clear-unit-resolvers!
  "Remove all installed unit resolvers (for test isolation)."
  []
  (reset! unit-resolvers []))

;; ---- dimension-name table (dims-map -> human name; seeded from the ||| labels) ----
(defonce dim-names (atom dimensions/names))

(defn- normalize-dims [d]
  (into {} (remove (comp zero? val)) d))   ; drop zero exponents to match canonical dims

(defn register-dimension!
  "Register (or override) the human name for a dimension map — e.g.
  `(register-dimension! {:length 4} \"quaternary space\")`. Zero exponents are dropped
  so the key matches a quantity's canonical dimensions (last writer wins; warns on a
  differing redefine). Returns `nm`."
  [dims nm]
  (register* dim-names "dimension" (normalize-dims dims) nm))

(defn lookup-dimension
  "Human name registered for dimension map `d` (builtin or user), or nil."
  [d]
  (@dim-names d))

(defn all-dimensions
  "The whole dims-map -> name table (for introspection)."
  []
  @dim-names)

(defn clear-dimensions!
  "Reset the dimension-name table to the builtin `|||` seed, dropping user
  registrations (for test isolation)."
  []
  (reset! dim-names dimensions/names))
