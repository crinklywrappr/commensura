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

  When recording is on, each public verb tags its result with a self-contained provenance *node* in
  the value's Clojure metadata: the operation, the result value, and the operand *sub-nodes* nested
  inline. So `(meta result)` holds the entire history as one `::node` map — you can read the whole tree
  by eye, no walker required.

    (require '[commensura.provenance :as prov :refer [with-provenance explain]]
             '[commensura.units :as u]
             '[commensura.core :refer [by to]])

    (with-provenance
      (explain (to (by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons)))

  **Off by default, zero-cost when off.** Recording is gated by the dynamic var
  `*record-provenance-on-step*` (default `false`); `with-provenance` binds it true for its body. When
  it's off the verbs do no `vary-meta` at all — they don't even build the operand vector.

  **Each step forgets its internals.** A `step` evaluates its body with recording *suppressed*, then
  records exactly one node for itself. So a verb records a single node over its operands, and a
  `defstep`'d function (like `commensura.math/sqrt`) shows only its own name — never the `pow` it calls
  underneath. The operands' *own* histories, built before the step ran, are nested in unchanged.

  **It's a DAG, not a tree.** The nested sub-nodes are shared immutable maps, so a `let`-bound value
  reused in two operands is the *same* node object in both places (`identical?` holds); `explain` prints
  the repeat as a back-reference and `replay` navigates it as one node.

  **Only real values carry history**, and it's **process-local.** Metadata rides on `IObj` values;
  raw scalars, exponents and keyword targets appear as inline operand leaves, never as nodes. Because
  `pr`/read ignore metadata, a recorded value is still `=` to, and prints identically to, its
  unrecorded self — history lives only in memory.

  Inspect with `node` (the nested map), `history` (its nodes as a seq), `explain` (a readable outline),
  and `replay` (a `clojure.zip` cursor you can step through)."
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
;; A node is a plain map — `{:op <verb> :value <result> :inputs [<child> ...]}` — stored in the result
;; value's metadata under `::node`. Each child is either another node map (a recorded operand, nested
;; inline) or a raw leaf (a number, an unrecorded quantity/unit). A node map is told from a value by
;; its `:op` key (commensura value records never have one).

(defn- node-map? [x] (and (map? x) (contains? x :op)))

(defn node
  "The provenance node — a nested `{:op :value :inputs}` map — for `x`, or nil if it carries none.
  Accepts a value (reads its metadata) or a node map (returns it), so the inspection fns take either."
  [x]
  (cond
    (node-map? x)                     x
    (instance? clojure.lang.IObj x)   (::node (meta x))
    :else                             nil))

(defn- as-child
  "How an operand appears inside its parent node: its own (nested) node if it has one, else itself."
  [x]
  (or (node x) x))

(defn record-node
  "Attach a provenance node to `result` (an IObj value): its op, the result value, and each input
  folded in as a nested sub-node or a leaf. A non-IObj result (a bare number, a boolean) is returned
  unchanged. Callers gate on `*record-provenance-on-step*` first (see `step`)."
  [op inputs result]
  (if (instance? clojure.lang.IObj result)
    (vary-meta result assoc ::node {:op op :value result :inputs (mapv as-child inputs)})
    result))

(defmacro step
  "Record `body`'s result as one provenance node labelled `op` over `inputs` (its operand values).
  A no-op — and zero allocation — unless `*record-provenance-on-step*` is on. The body runs with
  recording *suppressed*, so a step keeps no trace of the ops it calls internally: `(step #'sqrt [x] …)`
  records a lone `sqrt` node, and the operands' own histories (built before the step) nest in. `op` is
  conventionally the fn's `#'var` (a fully-qualified reference), so a node names exactly what made it."
  [op inputs & body]
  `(if *record-provenance-on-step*
     (record-node ~op ~inputs (binding [*record-provenance-on-step* false] ~@body))
     (do ~@body)))

(defmacro defstep
  "Define a function whose call records as one provenance node (its internals forgotten). Like `defn`
  — a docstring and multiple arities are supported — each arity's body is wrapped in `step` with that
  arity's parameters as inputs (a variadic arity folds its rest args in), and the node's op is the new
  `#'fully-qualified` var:

    (defstep sqrt [x] (c/pow x 1/2))       ; records `#'…/sqrt` over [x]; the inner `pow` leaves no trace
    (defstep root
      ([x]   (c/pow x 1/2))
      ([x n] (c/pow x (/ 1 n))))"
  [name & fdecl]
  (let [[doc fdecl] (if (string? (first fdecl)) [(first fdecl) (rest fdecl)] [nil fdecl])
        arities     (if (vector? (first fdecl)) (list fdecl) fdecl)   ; single-arity vs. several
        wrap        (fn [[params & body]]
                      (let [[fixed [_amp restsym]] (split-with #(not= '& %) params)
                            inputs (if restsym `(into ~(vec fixed) ~restsym) (vec fixed))]
                        `(~params (step (var ~name) ~inputs ~@body))))]
    `(defn ~name ~@(when doc [doc]) ~@(map wrap arities))))

;; ---- reading a node ----------------------------------------------------------------------------
(defn recorded?
  "Does `x` carry a provenance node?"
  [x]
  (some? (node x)))

(defn op
  "The verb that produced `x` (nil for a leaf)."
  [x]
  (:op    (node x)))

(defn value  "The result value stored on `x`'s node (nil for a leaf)."
  [x]
  (:value (node x)))

(defn inputs
  "The operand sub-nodes/leaves of `x`, in order (nil: a leaf)."
  [x]
  (:inputs (node x)))

(defn child-nodes
  "The inputs of `x` that are themselves nodes — the DAG children (inline leaves dropped)."
  [x]
  (filterv node-map? (inputs x)))

(defn history
  "Every node reachable from `x`, `x`'s node first, depth-first. A shared node repeats (it's the same
  object each time — `distinct` collapses them; `explain` shows it once with a back-reference)."
  [x]
  (when-let [n (node x)]
    (tree-seq node-map? #(filterv node-map? (:inputs %)) n)))

;; ---- zipper (replay) ---------------------------------------------------------------------------
(defn replay
  "A `clojure.zip` cursor over `x`'s history, positioned at its root node. Walk it with the ordinary
  zipper moves — `clojure.zip/down` into an operand, `/up` back out, `/right`/`/left` between operands,
  `/next`/`/prev` for a depth-first stroll — and read the node under the cursor with `clojure.zip/node`
  (then `op`/`value`/`inputs` on it). A branch's children are its recorded operands; leaves have none."
  [x]
  (zip/zipper node-map? #(filterv node-map? (:inputs %)) (fn [n _children] n) (node x)))

;; ---- explain (readable outline) ----------------------------------------------------------------
(defn- show
  "A one-line label for a value in an outline."
  [x]
  (if (iv/interval? x)
    (str "[" (str (iv/lo x)) " … " (str (iv/hi x)) "]")
    (str x)))                                            ; quantity/unit/number/uncertain toString

(defn- explain* [nd depth ^java.util.IdentityHashMap seen ^StringBuilder sb]
  (let [pad (apply str (repeat depth "    "))]
    (if-let [prior (.get seen nd)]
      (.append sb (str pad "↑ [" prior "] " (show (:value nd)) "\n"))        ; shared node — back-ref
      (let [id (inc (.size seen))]
        (.put seen nd id)
        (.append sb (str pad "[" id "] " (show (:value nd)) "  ←  " (:op nd) "\n"))
        (doseq [in (:inputs nd)]
          (if (node-map? in)
            (explain* in (inc depth) seen sb)
            (.append sb (str pad "    " (show in) "\n"))))))                  ; inline operand (leaf)
    sb))

(defn explain-str
  "Render `x`'s build history as a readable, indented outline and return it as a string. Each node
  gets a bracketed number, its result value, and its producing verb (`←  by`); inline operands sit
  unnumbered beneath their verb; a value reused elsewhere is shown once and later cited as `↑ [n]`."
  [x]
  (if-let [nd (node x)]
    (str (explain* nd 0 (java.util.IdentityHashMap.) (StringBuilder.)))
    (str (show x) "  (no recorded history)\n")))

(defn explain
  "Print `x`'s build history as a readable outline (see `explain-str`); returns nil."
  [x]
  (print (explain-str x))
  (flush))
