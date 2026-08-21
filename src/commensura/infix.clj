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
  * **`$=`** — *infix math* over `+ - * / **` with the usual precedence (`**` > `* /` > `+ -`), mapping
    to `by`/`per`/`plus`/`minus`/`pow`. Operands are `fj` forms, numbers, or nested `$=`.

  `to` (a plain fn) converts an already-built quantity: `(to q :dollars :per :day)`. When the target is
  *unit-led* it re-expresses in that unit (keeps the dimension); when it is *number-led* — `(to keg 12
  :floz)` — it returns the dimensionless **count**, matching frinj.

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
  number-led target (a specific quantity, e.g. `12 floz`) yields the dimensionless count."
  [source target-tokens]
  (let [target (soup->quantity target-tokens)]
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
(def ^:private op->fn {'* `c/by, '/ `c/per, '+ `c/plus, '- `c/minus, '** `c/pow})
(def ^:private op->prec {'** 3, '* 2, '/ 2, '+ 1, '- 1})

(defn- emit
  "Pop one operator, combining the top two output operands into a prefix call."
  [op out]
  (cons (list (op->fn op) (second out) (first out)) (drop 2 out)))

(defn- infix->prefix
  "Shunting-yard: turn an infix token seq (operands + `+ - * / **`) into one nested prefix form,
  left-associative, honouring `op->prec`."
  [tokens]
  (loop [tokens tokens, out nil, ops nil]
    (if-let [t (first tokens)]
      (if-let [p (op->prec t)]
        (let [[out ops] (loop [out out, ops ops]                ; pop all ops of >= precedence (left-assoc)
                          (if (and (seq ops) (>= (op->prec (first ops)) p))
                            (recur (emit (first ops) out) (rest ops))
                            [out ops]))]
          (recur (rest tokens) out (cons t ops)))
        (recur (rest tokens) (cons t out) ops))               ; operand
      (loop [out out, ops ops]                                 ; drain remaining operators
        (if (seq ops) (recur (emit (first ops) out) (rest ops))
            (first out))))))

(defmacro $=
  "Infix arithmetic over quantities: `($= (fj 4/3 :pi) * (fj 250 :km) ** 3)`. Operators `+ - * / **`
  map to `plus`/`minus`/`by`/`per`/`pow` with `**` > `* /` > `+ -` precedence; operands are `fj` forms,
  numbers, or nested `$=`."
  [& tokens]
  (infix->prefix tokens))
