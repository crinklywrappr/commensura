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

    (require '[commensura.core :refer [with-provenance by to]]
             '[commensura.provenance :refer [explain]]
             '[commensura.units :as u])

    (with-provenance
      (explain (to (by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons)))

  The two entry points for *making* history — `with-provenance` (turn recording on) and `defstep`
  (define a fn that records as one node) — live in `commensura.core`, alongside the verbs they drive.
  This namespace holds the mechanism (`step`/`record-node`, the `*record-provenance-on-step*` var) and
  everything for *reading* history back.

  **Off by default, zero-cost when off.** Recording is gated by the dynamic var
  `*record-provenance-on-step*` (default `false`); `commensura.core/with-provenance` binds it true for
  its body. When it's off the verbs do no `vary-meta` at all — they don't even build the operand vector.

  **Each step forgets its internals.** A `step` evaluates its body with recording *suppressed*, then
  records exactly one node for itself. So a verb records a single node over its operands, and a
  `defstep`'d function (like `commensura.math/sqrt`) shows only its own name — never the `pow` it calls
  underneath. The operands' *own* histories, built before the step ran, are nested in unchanged.

  **It's a DAG, not a tree.** The nested sub-nodes are shared immutable maps, so a `let`-bound value
  reused in two operands is the *same* node object in both places (`identical?` holds); `explain` shows
  the repeat once, citing it thereafter as a back-reference (`↑ [n]`).

  **Only real values carry history**, and it's **process-local.** Metadata rides on `IObj` values;
  raw scalars, exponents and keyword targets appear as inline operand leaves, never as nodes. Because
  `pr`/read ignore metadata, a recorded value is still `=` to, and prints identically to, its
  unrecorded self — history lives only in memory.

  Inspect with `node` (the nested map), `history` (its nodes as a flat seq), `history-zip` (a
  `clojure.zip` cursor over the whole tree — nodes and leaves), and `explain`/`explain-str`/
  `explain-lines` (a readable outline — printed, as one string, or as a seq of line-strings — itself
  written on `history-zip`)."
  (:require [commensura.quantity :as q]
            [commensura.interval :as iv]
            [clojure.string :as str]
            [clojure.zip :as zip]))

;; ---- the switch --------------------------------------------------------------------------------

(def ^:dynamic *record-provenance-on-step*
  "When true, the public verbs record a provenance node on each result. Default false — bind it with
  `commensura.core/with-provenance` (or directly) to record. Kept off by default so ordinary use pays
  nothing."
  false)

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

;; ---- reading a node ----------------------------------------------------------------------------
(defn recorded?
  "Does `x` carry a provenance node?"
  [x]
  (some? (node x)))

(defn op
  "The verb that produced `x` (nil for a leaf)."
  [x]
  (:op (node x)))

(defn value
  "The result value stored on `x`'s node (nil for a leaf)."
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
    (tree-seq node-map? child-nodes n)))

;; ---- history-zip: a cursor over the whole tree (nodes and leaves) ------------------------------
(defn history-zip
  "A `clojure.zip` cursor over `x`'s history — pass a recorded value *or* a node map. Unlike `history`
  (nodes only), this walks the **whole** tree: a branch's children are all its `:inputs`, so `zip/down`
  reaches the inline-operand leaves too. Read the node/leaf under the cursor with `clojure.zip/node`,
  move with `clojure.zip/next`/`down`/`up`/`right`/`left`. `explain` is written on top of it."
  [x]
  (zip/zipper node-map? :inputs (fn [n _children] n) (node x)))

;; ---- explain (readable outline) ----------------------------------------------------------------
(defn- show
  "A one-line label for a value in an outline."
  [x]
  (if (iv/interval? x)
    (str "[" (str (iv/lo x)) " … " (str (iv/hi x)) "]")
    (str x)))                                            ; quantity/unit/number/uncertain toString

(defn- skip-subtree
  "The next loc after `loc`'s whole subtree — its right sibling, else the nearest ancestor's right
  sibling, else nil at the end. (clojure.zip has no built-in subtree skip; used to prune an
  already-shown shared node without re-descending it.)"
  [loc]
  (or (zip/right loc)
      (loop [l (zip/up loc)]
        (when l (or (zip/right l) (recur (zip/up l)))))))

(defn explain-lines
  "`x`'s build history as a seq of outline line-strings — the data `explain-str` joins and `explain`
  prints. A node line reads `[n] <value>  ←  <verb>`; inline operands sit unnumbered beneath their
  verb; a value reused elsewhere appears once, then is cited as `↑ [n]`. Returned as data so you can
  count/filter/re-indent it or feed it to a viewer. Walks `history-zip` (indentation from `zip/path`;
  an `IdentityHashMap` numbers nodes so a repeat renders as a back-reference, its subtree then pruned)."
  [x]
  (if (node x)
    (loop [loc (history-zip x), seen (java.util.IdentityHashMap.), lines []]
      (if (or (nil? loc) (zip/end? loc))
        lines
        (let [nd  (zip/node loc)
              pad (apply str (repeat (count (zip/path loc)) "    "))]
          (cond
            (not (node-map? nd))                                                ; inline operand (leaf)
            (recur (zip/next loc) seen (conj lines (str pad (show nd))))

            (.get seen nd)                                                      ; shared node — back-ref
            (recur (skip-subtree loc) seen
                   (conj lines (str pad "↑ [" (.get seen nd) "] " (show (:value nd)))))

            :else
            (let [id (inc (.size seen))]
              (.put seen nd id)
              (recur (zip/next loc) seen
                     (conj lines (str pad "[" id "] " (show (:value nd)) "  ←  " (:op nd)))))))))
    [(str (show x) "  (no recorded history)")]))

(defn explain-str [x] (str/join "\n" (explain-lines x)))

(defn explain [x] (println (explain-str x)))
