;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.provenance
  "Opt-in **build history** — \"how did I get here?\" — for commensura values.

  When recording is on, each public verb tags its result with a provenance *node* stored in the
  value's Clojure metadata: the operation and the operand values that produced it. Because the operands
  are themselves values carrying their own nodes, the history is a **DAG** you walk through the object
  graph — and a `let`-bound value reused in two places is a *shared* node (object identity is
  preserved), not two copies.

    (require '[commensura.provenance :as prov :refer [with-provenance explain]]
             '[commensura.units :as u]
             '[commensura.core :refer [by to]])

    (with-provenance
      (explain (to (by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons)))

  **Off by default, zero-cost when off.** Recording is gated by the dynamic var
  `*record-provenance-on-step*` (default `false`); `with-provenance` just binds it true for its body.
  When it's false the verbs do no `vary-meta` at all — they don't even build the operand vector.

  **Only real values carry history.** Metadata rides on `IObj` values (quantities, units, intervals,
  uncertains); raw scalars, exponents and keyword targets can't hold metadata, so they appear as inline
  *operands* of a node, never as nodes of their own. History is **process-local** — it lives in memory
  and does not survive `pr`/read (the printed tagged literal ignores metadata), so a recorded value is
  still `=` to, and prints identically to, its unrecorded self.

  Wrap your own multi-step function as a single named node with `step` / `defstep` (commensura's own
  `math` fns do this, so `sqrt` shows as `sqrt`, not its internal `pow`); the collapsed internals are
  kept under the node and revealed with `explain … :expand :all`. Inspect with `history` (the nodes as a
  seq), `explain` (a readable outline), and `replay` (a `clojure.zip` cursor you can step through)."
  (:require [commensura.quantity :as q]
            [commensura.interval :as iv]
            [clojure.zip :as zip]))

;; ---- the switch --------------------------------------------------------------------------------
(def ^:dynamic *record-provenance-on-step*
  "When true, the public verbs record a provenance node on each result. Default false — bind it with
  `with-provenance` (or directly) to record. Kept off by default so ordinary use pays nothing."
  false)

(defmacro with-provenance
  "Evaluate `body` with provenance recording on, returning its value (now carrying history)."
  [& body]
  `(binding [*record-provenance-on-step* true] ~@body))

;; ---- the node ----------------------------------------------------------------------------------
;; A node lives in a value's metadata under `::node`. The value itself is the node's result; the node
;; records how it was made:
;;   :op       — the verb (a symbol/label)
;;   :inputs   — the operand values, in order (some carry their own ::node → DAG children; the rest —
;;               numbers, exponents, keyword targets — are inline leaves)
;;   :internal — for a `step`, the collapsed sub-computation's node (revealed by `explain … :expand :all`)

(defn record-node
  "Attach a provenance node to `result` (an IObj value): op + inputs, stashing any node the result
  already carried as `:internal` (so a `step` collapses its internals under one name). A non-IObj
  result — a bare number, a boolean — is returned unchanged. Callers gate on
  `*record-provenance-on-step*` before calling this (see `step`)."
  [op inputs result]
  (if (instance? clojure.lang.IObj result)
    (vary-meta result assoc ::node {:op op
                                    :inputs (vec inputs)
                                    :internal (::node (meta result))})
    result))

(defmacro step
  "Record `body`'s result as a single provenance node labelled `op`, with `inputs` (the operand values
  that are the node's DAG children/inline operands). A no-op — and zero allocation — unless
  `*record-provenance-on-step*` is on. Any provenance the body built internally is collapsed under this
  node (kept as its `:internal`, shown by `explain … :expand :all`), so `(step 'sqrt [x] …)` shows as
  `sqrt` rather than its inner `pow`."
  [op inputs & body]
  `(let [r# (do ~@body)]
     (if *record-provenance-on-step*
       (record-node ~op ~inputs r#)
       r#)))

(defmacro defstep
  "Define a function whose call records as one provenance node. Sugar for a `defn` whose single-arity
  body is wrapped in `step` with the fixed parameters as inputs:

    (defstep scale [q n] (quantity/scale q n))   ; records `scale` with inputs [q n]

  For a verb with several arities or its own dispatch, use `step` inline instead."
  [name params & body]
  `(defn ~name ~params
     (step '~name ~(vec params) ~@body)))

;; ---- reading a node ----------------------------------------------------------------------------
(defn node
  "The provenance node recorded on value `x`, or nil if it carries none (a leaf)."
  [x]
  (when (instance? clojure.lang.IObj x) (::node (meta x))))

(defn recorded?
  "Does `x` carry a provenance node?"
  [x]
  (some? (node x)))

(defn op
  "The verb that produced `x` (nil for a leaf)."
  [x]
  (:op (node x)))

(defn inputs
  "The operand values that produced `x`, in order (nil for a leaf)."
  [x]
  (:inputs (node x)))

(defn child-nodes
  "The inputs of `x` that themselves carry provenance — the DAG children (leaves are dropped)."
  [x]
  (filterv recorded? (inputs x)))

(defn history
  "Every recorded node reachable from `x`, `x` first, in depth-first order (a shared node may repeat —
  `explain` de-duplicates for display; use `distinct`-by-identity if you need each once)."
  [x]
  (tree-seq recorded? child-nodes x))

;; ---- zipper (replay) ---------------------------------------------------------------------------
(defn replay
  "A `clojure.zip` cursor over `x`'s history, positioned at `x`. Walk it with the ordinary zipper
  moves — `clojure.zip/down` into an operand, `/up` back out, `/right`/`/left` between operands,
  `/next`/`/prev` for a depth-first stroll — and read the value under the cursor with `clojure.zip/node`.
  A branch's children are its recorded inputs (the DAG children); leaves have none. (The DAG is walked
  as a tree, so a shared value is visited under each parent; `explain` marks the repeats.)"
  [x]
  (zip/zipper recorded? child-nodes (fn [n _children] n) x))

;; ---- explain (readable outline) ----------------------------------------------------------------
(defn- show
  "A one-line label for a value in an `explain` outline."
  [x]
  (cond
    (iv/interval? x)    (str "[" (str (iv/lo x)) " … " (str (iv/hi x)) "]")
    (q/displayable? x)  (str x)                        ; quantity / unit / bare number
    :else               (str x)))                      ; uncertain (nice toString) & anything else

(defn- explain* [x depth expand? seen sb]
  (let [pad (apply str (repeat depth "    "))]
    (if-let [id (get @seen x)]
      (.append ^StringBuilder sb (str pad "↑ [" id "] " (show x) "\n"))   ; shared node — back-reference
      (let [id (inc (count @seen))]
        (swap! seen assoc x id)
        (.append ^StringBuilder sb (str pad "[" id "] " (show x)
                                        (when-let [o (op x)] (str "  ←  " o)) "\n"))
        (doseq [in (inputs x)]
          (if (recorded? in)
            (explain* in (inc depth) expand? seen sb)
            (.append ^StringBuilder sb (str pad "    " (show in) "\n"))))   ; inline operand (leaf)
        (when (and expand? (:internal (node x)))
          (.append ^StringBuilder sb (str pad "    · internals:\n")))))    ; (expanded rendering: v2)
    sb))

(defn explain-str
  "Render `x`'s build history as a readable, indented outline and return it as a string. Each recorded
  value gets a bracketed number and its producing verb (`←  by`); inline operands (scalars, target
  units) sit unnumbered beneath their verb; a value reused elsewhere is shown once and later referenced
  as `↑ [n]`. Pass `:expand :all` to also note collapsed `step` internals."
  [x & {:keys [expand]}]
  (str (explain* x 0 (= expand :all) (atom {}) (StringBuilder.))))

(defn explain
  "Print `x`'s build history as a readable outline (see `explain-str`); returns nil."
  [x & opts]
  (print (apply explain-str x opts))
  (flush))
