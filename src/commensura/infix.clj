;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.infix
  "A **frinj-flavoured** notation for commensura — an optional, familiar entry point for people coming
  from the `frinj` library (Martin Trojer's Clojure port of Alan Eliasen's Frink). It is a thin sugar
  layer: every value it builds is an ordinary `commensura.quantity`, so the exact tower, dimensions, and
  printing all carry through — a conversion keeps its unit and dimension rather than flattening to a bare
  number the way frinj does.

    (require '[commensura.infix :refer [fj $= to]])

    (fj 10 :feet 12 :feet 8 :feet :to :gallons)   ;=> 552960/77 gallon ≈ 7181.30 [volume]
    ($= (fj 2 :tons) / (fj 10 :feet 12 :feet :water))
        ; a value you can keep computing with — no `str` needed

  Two forms:

  * **`fj`** — *keyword soup*. A left-to-right product of alternating numbers and `:unit` keywords
    (`(fj 5 12 :floz)` ⇒ 60 floz); `:per` divides the next factor; `:to` converts everything after it to
    a target built the same way. Unit keywords resolve through `commensura.registry` — with a plural
    fallback (`:gallons` → `gallon`), since commensura registers singular names.
  * **`$=`** — *infix math* over `** * / + -` (→ `pow`/`by`/`per`/`plus`/`minus`) and the comparisons
    `== != < > <= >=` (→ `eq?`/`ne?`/`lt?`/`gt?`/`le?`/`ge?`), with precedence `**` > `* /` > `+ -` >
    comparisons. Add your own with **`defop`**. Operands are `fj` forms, numbers, or nested `$=`.

  `to` (a plain fn) converts an already-built quantity: `(to q :dollars :per :day)`. A *unit-led* target
  re-expresses in that unit (keeps the dimension); a *number-led* target — `(to keg 12 :floz)` — returns
  the dimensionless **count**; a *mirrored* target whose dimension is the reciprocal of the source's is
  flipped for you (frinj's \"reverse mirrored units\"), so `(to fuel-per-distance :feet :per :gallon)`
  yields the economy. (This reversal is the frinj-flavoured layer's; `commensura.core/to` stays strict.)

  Define your own units the ordinary commensura way — `(defunit beer (fj 12 :floz 3.2 :percent :water
  :per :alcohol))` — and later soups resolve them by name (`(fj :magnum 13.5 :percent :to :beer)`); this
  layer adds no separate registration path (frinj's `add-unit!` is just `commensura.core/defunit`).

  Credit: the notation and the worked examples are Alan Eliasen's Frink and Martin Trojer's frinj; this
  namespace just re-points them at commensura's exact engine."
  (:require [commensura.core :as c]
            [commensura.quantity :as q]
            [commensura.registry :as registry]))

;; ---- unit resolution: a frinj `:keyword` -> a commensura unit (or a bare scalar) -------------------
(def ^:private word-scalars
  "frinj words that are bare dimensionless scalars rather than registered units."
  {"half" 1/2})

(defn- resolve-unit-name
  "Resolve a soup unit name to a commensura unit: the name, else a trailing-`s` plural stripped to the
  singular commensura registers (`gallons`→`gallon`), else a known bare-scalar word (`half`), else throw.
  (commensura ships a few explicit plurals like `feet`/`inches`; the strip covers the rest.)"
  [nm]
  (or (registry/resolve-unit nm)
      (when (.endsWith nm "s") (registry/resolve-unit (subs nm 0 (dec (count nm)))))    ; gallons→gallon, miles→mile
      (word-scalars nm)
      (throw (ex-info (str "infix: unknown unit " (pr-str nm)
                           " — no commensura unit for that name (or its singular)")
                      {:unit nm}))))

(defn- factor
  "The multiplicative factor a soup token contributes: a keyword resolves to a unit, anything else
  (number/Ratio/quantity) is used as-is."
  [t]
  (if (keyword? t) (resolve-unit-name (name t)) t))

(defn- soup->quantity
  "Fold a keyword-soup token seq into one quantity: numbers/units multiply left-to-right; `:per` divides
  the single factor that follows it."
  [tokens]
  (first
   (reduce (fn [[acc per?] t]
             (if (= t :per)
               [acc true]
               [(if per? (c/per acc (factor t)) (c/by acc (factor t))) false]))
           [(q/scalar 1) false]
           tokens)))

(defn- convert
  "Convert `source` to a soup target. A unit-led target re-expresses `source` in it (dimension kept); a
  number-led target (a specific quantity, e.g. `12 floz`) yields the dimensionless count. Like frinj, a
  *mirrored* target — one whose dimension is the reciprocal of the source's — flips the source first, so
  `(to (…gallons/foot…) :feet :per :gallon)` reads the fuel economy instead of throwing. This lives in
  the frinj-flavoured layer only; `commensura.core/to` itself stays strict and rejects the mismatch."
  [source target-tokens]
  (let [target (soup->quantity target-tokens)
        source (cond
                 (q/conforms? source target)           source
                 (q/conforms? (c/per 1 source) target) (c/per 1 source)  ; frinj: reverse mirrored units
                 :else                                 source)]          ; non-conforming: let core throw
    (if (number? (first target-tokens))
      (c/ratio source target)
      (c/to source target))))

;; ---- fj: keyword soup ------------------------------------------------------------------------------
(defn fj
  "Build a quantity from *keyword soup*: `(fj 10 :feet 12 :feet 8 :feet)` ⇒ 960 feet³. `:per` divides the
  next factor; a trailing `:to <target soup>` converts the result (see the ns docstring)."
  [& tokens]
  (let [[value-tokens to-tokens] (split-with #(not= :to %) tokens)
        value (soup->quantity value-tokens)]
    (if (seq to-tokens)
      (convert value (rest to-tokens))                     ; drop the :to
      value)))

(defn to
  "Convert an already-built quantity to a soup target: `(to (fj :keg) :case)`, `(to q :dollars :per
  :day)`. Number-led targets give the dimensionless count (`(to (fj :keg) 12 :floz)` ⇒ 496/3)."
  [source & target-tokens]
  (convert source target-tokens))

;; ---- $=: infix math --------------------------------------------------------------------------------
;; Operators live in a table so `defop` can extend it, exactly like frinj. Each maps to a precedence
;; (higher binds tighter) and a resolvable fn symbol that `$=` emits in prefix position.
(defonce ^:private ops (atom {}))

(defn register-op!
  "Install an infix operator for `$=`: `op` symbol → precedence + a binary fn (a resolvable symbol).
  Prefer the `defop` macro."
  [op prec fn-sym]
  (swap! ops assoc op {:prec prec :fn fn-sym})
  op)

(defmacro defop
  "Define an infix operator usable inside `$=`: `(defop <sym> <precedence> <fn>)`, higher precedence
  binding tighter, `<fn>` a binary function named by a resolvable symbol. Mirrors frinj's `defop` — the
  builtins below register themselves this way, and user operators join them at load time."
  [op prec f]
  `(register-op! '~op ~prec '~f))

;; builtins — precedence order matches frinj: ** > * / > + - > comparisons
(defop **  5 commensura.core/pow)
(defop *   4 commensura.core/by)
(defop /   4 commensura.core/per)
(defop +   3 commensura.core/plus)
(defop -   3 commensura.core/minus)
(defop <   2 commensura.core/lt?)
(defop >   2 commensura.core/gt?)
(defop <=  2 commensura.core/le?)
(defop >=  2 commensura.core/ge?)
(defop ==  1 commensura.core/eq?)
(defop !=  1 commensura.core/ne?)

(defn- emit
  "Pop one operator, combining the top two output operands into a prefix call."
  [op out]
  (cons (list (:fn (@ops op)) (second out) (first out)) (drop 2 out)))

(defn- infix->prefix
  "Shunting-yard: turn an infix token seq (operands + registered operators) into one nested prefix
  form, left-associative, honouring each operator's precedence."
  [tokens]
  (loop [tokens tokens, out nil, opstack nil]
    (if (seq tokens)
      (let [t  (first tokens)
            op (@ops t)]
        (if op
          (let [[out opstack] (loop [out out, opstack opstack]  ; pop ops of >= precedence (left-assoc)
                                (if (and (seq opstack) (>= (:prec (@ops (first opstack))) (:prec op)))
                                  (recur (emit (first opstack) out) (rest opstack))
                                  [out opstack]))]
            (recur (rest tokens) out (cons t opstack)))
          (recur (rest tokens) (cons t out) opstack)))          ; operand
      (loop [out out, opstack opstack]                          ; drain remaining operators
        (if (seq opstack) (recur (emit (first opstack) out) (rest opstack))
            (first out))))))

(defmacro $=
  "Infix over quantities: `($= (fj 4/3 :pi) * (fj 250 :km) ** 3)`. Operators come from the `defop` table
  — arithmetic `** * / + -` (→ pow/by/per/plus/minus) and comparisons `== != < > <= >=` (→
  eq?/ne?/lt?/gt?/le?/ge?) — with precedence `**` > `* /` > `+ -` > comparisons, all left-associative.
  Operands are `fj` forms, numbers, or nested `$=` (which also groups a sub-expression)."
  [& tokens]
  (infix->prefix tokens))
