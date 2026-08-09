;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.quantity
  "The value types.

  A `Unit` is a *named* registered unit — a name, an exact base-SI magnitude, and
  a stored dimension map. `foot`, `meter`, `newton`, `beer` are all Units; a bare
  Unit is one of itself and prints `foot [length]`. `defunit` mints Units.

  A `Quantity` is an *anonymous* computed value — an exact magnitude plus an
  ordered display `formula` (a vector of `UnitTerm`s). Its dimensions are *derived*
  from the formula, never stored, so they can't disagree with it. Every exact
  arithmetic result is a Quantity.

  An `ApproxQuantity` is the same as a `Quantity` but its magnitude is an
  arbitrary-precision `BigDecimal` — for *irrational* values (planck units,
  `semitone`, Richter) that have no exact rational form. Arithmetic *promotes*:
  the moment any operand is approximate, the result is an `ApproxQuantity` and the
  magnitude math runs under the caller's `*math-context*` (`with-precision` /
  `binding`), defaulting to DECIMAL128. Values print under two tags —
  `#commensura/unit` and `#commensura/quantity` — with a leading `≈` in the payload
  marking approximate values, so inexactness is never mistaken for exact.

  So: named ⇒ Unit (stores dims), anonymous ⇒ Quantity/ApproxQuantity (store a
  formula); dims live in exactly one place. All are callable and implement
  `Dimensionable`/`Measured`/`Formulaic`."
  (:require [commensura.registry :as registry]
            [clojure.string :as str]
            [clojure.pprint :as pp])
  (:import [ch.obermuhlner.math.big BigDecimalMath]
           [java.math MathContext]))

;; ---- protocols ----
(defprotocol Dimensionable
  (dims [x] "Base-dimension -> integer-exponent map (zero exponents removed)."))

(defprotocol Measured
  (magnitude [x]
    "Magnitude in base SI units. Exact for Units/Quantities/plain numbers
    (`rationalize`d, so 3.2 -> 16/5); a BigDecimal for ApproxQuantities."))

(defprotocol Formulaic
  (formula [x] "A vector of UnitTerms that describe the components of a measurement."))

(defprotocol Displayable
  (display-value [x]
    "The value shown to the user: base magnitude ÷ the display formula's factor (a bare
    unit is 1; a plain number is itself). Used by the printer, intervals, and the reader.")
  (display-string [x]
    "The full human-readable payload — value, unit, trailing `[dimension]` — that is a
    unit/quantity's `toString` and the body of its `#commensura/…` tagged literal."))

;; ---- dimension-map helpers ----
(def ^:private clean-xf (remove (comp zero? second)))

(defn- scale-dims [d n]
  (let [mult (fn [[k v]] [k (* v n)])
        xf (comp (map mult) clean-xf)]
    (into {} xf d)))

(defn- formula-dims
  "Derive a quantity's dimensions from its display formula: sum each term's
  contribution, dropping zero exponents."
  [formula]
  (transduce
   (map dims)
   (completing
    (partial merge-with +)
    (partial into {} clean-xf))
   {} formula))

;; commensura's fallback context for approximate (BigDecimal) arithmetic when the
;; caller hasn't bound `*math-context*` (via `with-precision`/`binding`):
;; 34-significant-digit DECIMAL128 (HALF_EVEN). Defined here because the approx
;; records' `magnitude` coerces through it — a Ratio→BigDecimal conversion needs a
;; context to terminate (`(bigdec 1/3)` throws otherwise).
(def ^:private default-mc MathContext/DECIMAL128)
(defn- ctx [] (or *math-context* default-mc))

(declare scale)

;; one term of a display formula, e.g. foot^3 — a Unit raised to a power
(defrecord UnitTerm [unit-name exp factor base-dims]
  Dimensionable
  (dims [_] (scale-dims base-dims exp)))          ; this term's dimensional contribution

;; a named registered unit: name + base magnitude + stored dimensions
(defrecord PreciseUnit [name mag dims]
  Dimensionable
  (dims [_] dims)

  Measured
  (magnitude [_] (magnitude mag))

  Formulaic
  (formula [_] [(->UnitTerm name 1 mag dims)])

  clojure.lang.IFn
  (invoke [this n] (scale this n))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))

  Object
  (toString [this] (display-string this)))

;; an anonymous computed value: exact magnitude + ordered display formula
(defrecord PreciseQuantity [mag formula]
  Dimensionable
  (dims [_] (formula-dims formula))

  Measured
  (magnitude [_] (magnitude mag))

  Formulaic
  (formula [_] formula)

  clojure.lang.IFn
  (invoke [this n] (scale this n))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))

  Object
  (toString [this] (display-string this)))

;; like Unit, but the magnitude is an approximate BigDecimal (irrational value)
(defrecord ApproxUnit [name mag dims]
  Dimensionable
  (dims [_] dims)

  Measured
  (magnitude [_] (binding [*math-context* (ctx)] (bigdec mag)))

  Formulaic
  (formula [_] [(->UnitTerm name 1 mag dims)])

  clojure.lang.IFn
  (invoke [this n] (scale this n))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))

  Object
  (toString [this] (display-string this)))

;; like Quantity, but the magnitude is an approximate BigDecimal (irrational value)
(defrecord ApproxQuantity [mag formula]
  Dimensionable
  (dims [_] (formula-dims formula))

  Measured
  (magnitude [_] (binding [*math-context* (ctx)] (bigdec mag)))

  Formulaic
  (formula [_] formula)

  clojure.lang.IFn
  (invoke [this n] (scale this n))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))

  Object
  (toString [this] (display-string this)))

(extend-protocol Dimensionable
  Number (dims [_] {})
  nil    (dims [_] {}))

(extend-protocol Measured
  Number
  (magnitude [n]
    (let [mag' (rationalize n)]
      (if (integer? mag')
        (bigint mag')
        mag'))))

(extend-protocol Formulaic
  Number (formula [_] []))

;; PREDICATES

(defn unit? [x]
  (or (instance? PreciseUnit x)
      (instance? ApproxUnit x)))

(defn quantity? [x]
  (or (instance? PreciseQuantity x)
      (instance? ApproxQuantity x)))

(defn precise? [x]
  (or (instance? PreciseUnit x)
      (instance? PreciseQuantity x)))

(defn approx? [x]
  (or (instance? ApproxUnit x)
      (instance? ApproxQuantity x)))

;; ---- exact integer power (keeps the exact tower) ----
(defn- expt [base n]
  (cond
    (zero? n) 1
    (pos? n)  (reduce * (repeat n base))
    :else     (/ 1 (reduce * (repeat (- n) base)))))

;; ---- approximate (BigDecimal) numeric layer ------------------------------
;; A magnitude that is a BigDecimal is *approximate*; any operation touching one
;; yields a BigDecimal, so approximateness propagates. Precision and rounding
;; follow the caller's `*math-context*` (set via `with-precision` or `binding`),
;; defaulting to 34-digit DECIMAL128 (unbiased HALF_EVEN) when unbound. The exact
;; Ratio/Long path never touches BigDecimal, so `*math-context*` only ever affects
;; approximate arithmetic — never exact. (`default-mc`/`ctx` defined above the
;; records, since their `magnitude` coerces through `ctx`.)

;; Native +,-,*,/ under a bound *math-context* already dispatch correctly: exact
;; Long/Ratio operands stay exact (the context is ignored), while any BigDecimal
;; operand yields a BigDecimal rounded to the context (coercing ratios). So no
;; type branch is needed — approximateness propagates through the numeric tower.
(defn- nmul [a b] (binding [*math-context* (ctx)] (* a b)))
(defn- nadd [a b] (binding [*math-context* (ctx)] (+ a b)))
(defn- nsub [a b] (binding [*math-context* (ctx)] (- a b)))
(defn- ndiv [a b] (binding [*math-context* (ctx)] (/ a b)))
(defn- npow [a n]
  (if (decimal? a)
    (let [c (ctx)]
      (if (neg? n)
        (.divide 1M (.pow a (- n) c) c)
        (.pow a n c)))
    (expt a n)))

;; ---- transcendental BigDecimal helpers (exp / ln) ------------------------
;; For genuinely irrational log/exp-scale units (e.g. Richter). The JDK's BigDecimal
;; has no exp/ln, so we defer to big-math (ch.obermuhlner), which evaluates them with
;; adaptive-precision Newton at a given MathContext. We coerce the argument under the
;; caller's `*math-context*` (default DECIMAL128) and compute at the same context.
(defn bexp
  "e^x as a BigDecimal at the caller's `*math-context*` (default DECIMAL128); x is
  any number, coerced to BigDecimal under that context."
  [x]
  (let [mc (ctx)]
    (BigDecimalMath/exp (binding [*math-context* mc] (bigdec x)) mc)))

(defn bln
  "Natural log of a positive number as a BigDecimal at the caller's `*math-context*`."
  [x]
  (let [mc (ctx)]
    (BigDecimalMath/log (binding [*math-context* mc] (bigdec x)) mc)))

;; ---- constructors ----
(defn unit
  "Mint a named registered `Unit` from a name, base magnitude, and dimensions —
  what `defunit` and the generated builtins call."
  [name mag dims]
  (if (or (ratio? mag) (integer? mag))
    (->PreciseUnit name mag (into {} clean-xf dims))
    (->ApproxUnit name mag (into {} clean-xf dims))))

(defn quantity
  "Mint a PreciseQuantity if the magnitude is a ratio or integer, or can
   be rationalized to an integer.  Otherwise, returns an ApproxQuantity."
  [mag formula]
  (if (or (ratio? mag) (integer? mag))
    (->PreciseQuantity mag formula)
    (let [mag' (rationalize mag)]
      (if (integer? mag')
        (->PreciseQuantity mag' formula)
        ;; use the provided magnitude for ApproxQuantity
        (->ApproxQuantity mag formula)))))

(defn scalar
  "Wrap a plain number as a dimensionless Quantity (idempotent on any quantity)."
  [n]
  (if (or (unit? n) (quantity? n) (approx? n))
    n
    (->PreciseQuantity (rationalize n) [])))

;; ---- accessors ----
(defn dimensionless? [x] (empty? (dims x)))
(defn conforms? [a b] (= (dims a) (dims b)))

(defn scale
  "Scale a quantity's magnitude by a plain number — what `(u/feet 10)` does.
  Yields an anonymous Quantity (or ApproxQuantity if the input is approximate)."
  [q n]
  (cond
    (precise? q) (->PreciseQuantity (* (magnitude q) (rationalize n)) (formula q))
    (approx? q) (quantity (nmul (magnitude q) (rationalize n)) (formula q))
    :else (throw (ex-info "q must be a unit or quantity" {:q q :n n}))))

;; ---- display-formula algebra ----
(defn- combine-terms
  "Merge UnitTerms sharing a unit-name (adding exponents), drop terms that cancel
  to exponent 0, and canonicalize order to positive-exponent terms (first-seen
  order) followed by negative-exponent terms — so the printed formula and the
  reader are exact inverses."
  [terms]
  (let [order   (distinct (map :unit-name terms))
        by-name (group-by :unit-name terms)
        merged  (keep (fn [nm]
                        (let [ts (by-name nm)
                              e  (transduce (map :exp) + ts)]
                          (when-not (zero? e)
                            (assoc (first ts) :exp e))))
                      order)]
    (into (filterv (comp pos? :exp) merged)
          (filterv (comp neg? :exp) merged))))

(defn- formula-neg [formula]
  (mapv #(update % :exp -) formula))

(defn- formula-pow [formula n]
  (if (zero? n)
    []
    (mapv #(update % :exp * n) formula)))

;; ---- arithmetic (yields an anonymous Quantity, promoting to ApproxQuantity) ----
;; In the precise branch every operand's magnitude is already exact: precise records
;; carry rational :mags by construction, and `magnitude` rationalizes plain numbers.
;; So the exact `+ - * /`/`expt` here need no `rationalize` wrapper.
(defn qmul [x y]
  (let [formula' (combine-terms (concat (formula x) (formula y)))]
    (if (or (approx? x) (approx? y))
      (quantity (nmul (magnitude x) (magnitude y)) formula')
      (->PreciseQuantity (* (magnitude x) (magnitude y)) formula'))))

(defn qdiv [x y]
  (let [formula' (combine-terms (concat (formula x) (formula-neg (formula y))))]
    (if (or (approx? x) (approx? y))
      (quantity (ndiv (magnitude x) (magnitude y)) formula')
      (->PreciseQuantity (/ (magnitude x) (magnitude y)) formula'))))

(defn qpow [x n]
  (let [formula' (formula-pow (formula x) n)]
    (if (approx? x)
      (quantity (npow (magnitude x) n) formula')
      (->PreciseQuantity (npow (magnitude x) n) formula'))))

(defn- assert-conforms [op x y]
  (when-not (conforms? x y)
    (throw (ex-info (str op ": non-conforming dimensions")
                    {:a (dims x) :b (dims y)}))))

(defn qadd [x y]
  (assert-conforms "plus" x y)
  ;; keeps left's formula
  (if (or (approx? x) (approx? y))
    (quantity (nadd (magnitude x) (magnitude y)) (formula x))
    (->PreciseQuantity (+ (magnitude x) (magnitude y)) (formula x))))

(defn qsub [x y]
  (assert-conforms "minus" x y)
  (if (or (approx? x) (approx? y))
    (quantity (nsub (magnitude x) (magnitude y)) (formula x))
    (->PreciseQuantity (- (magnitude x) (magnitude y)) (formula x))))

(defn to
  "Re-express q over the target unit basis (dimension-preserving). The physical
  magnitude is unchanged — only the display formula becomes the target's, so the
  printed value equals magnitude(q)/factor(target)."
  [q target]
  (assert-conforms "to" q target)
  (if (or (approx? q) (approx? target))
    (quantity (magnitude q) (formula target))
    (->PreciseQuantity (magnitude q) (formula target))))

(defn ratio
  "Bare dimensionless count: how many of target fit in q."
  [q target]
  (assert-conforms "ratio" q target)
  (if (or (approx? q) (approx? target))
    (quantity (ndiv (magnitude q) (magnitude target)) [])
    (->PreciseQuantity (/ (magnitude q) (magnitude target)) [])))

;; ---- display ----
(defn- formula-factor
  "Base magnitude of one of the compound display unit: the product of each term's
  factor raised to its exponent."
  [formula]
  (reduce (fn [acc t] (* acc (expt (:factor t) (:exp t)))) 1 formula))

(defn- base-dim-name
  "A *single* base dimension rendered unambiguously — {:length 4} -> \"length^4\",
  {:length -1} -> \"1/length\", {:length 1} -> \"length\" — or nil for compounds."
  [d]
  (when (== 1 (count d))
    (let [[k e] (first d)
          nm    (str/replace (clojure.core/name k) "_" " ")]
      (cond
        (== e 1)  nm
        (pos? e)  (str nm "^" e)
        (== e -1) (str "1/" nm)
        :else     (str "1/" nm "^" (- e))))))

(defn- dimension-name
  "Human name for a dimension map: a registered name (builtin `|||` label or a
  user `register-dimension!`), else a single-base-dimension expression, else nil
  (a compound with no registered name)."
  [d]
  (or (registry/dimension-name d)
      (base-dim-name d)))

(defn- term-str [nm e] (if (== e 1) nm (str nm "^" e)))

(defn- format-formula
  "Render a formula as `foot^3`, `mile/hour`, `meter/minute/celsius`, `kg m/s^3`,
  or \"\" (empty). The numerator is a space-joined product; each divisor gets its
  own `/`, so it reads as the repeated division `(per a b c)` that produced it."
  [formula]
  (let [pos   (filter (comp pos? :exp) formula)
        neg   (filter (comp neg? :exp) formula)
        numer (str/join " " (map #(term-str (:unit-name %) (:exp %)) pos))
        dens  (map #(str "/" (term-str (:unit-name %) (- (:exp %)))) neg)]
    (cond
      (empty? formula) ""
      (empty? neg)     numer
      :else            (apply str (if (empty? pos) "1" numer) dens))))

(defn- dim-bracket [d]
  (if-let [nm (dimension-name d)]
    (str "[" nm "]")
    (if (seq d) "[unknown dimension]"                    ; unnamed compound — register-dimension!
        "[dimensionless]")))

;; Per-record display, dispatched by type (no unit?/approx?/quantity? branching). Placed
;; here, after the helpers above, so the impls resolve without forward-declares.
;;  - unit:             `foot [length]`   (approx: `≈ planckmass [mass]`)
;;  - precise quantity: `552960/77 gallon ≈ 7181.30 [volume]`   (exact, then a ≈<double> eyeball)
;;  - approx quantity:  `≈ 2.176434…E-8 kilogram [mass]`        (full BigDecimal — round-trips)
(extend-protocol Displayable
  PreciseUnit
  (display-value  [_] 1)                                  ; a bare unit is one of itself
  (display-string [u] (str (:name u) " " (dim-bracket (dims u))))

  ApproxUnit
  (display-value  [_] 1)
  (display-string [u] (str "≈ " (:name u) " " (dim-bracket (dims u))))  ; leading ≈ = approximate

  PreciseQuantity
  (display-value  [q] (let [m (magnitude q), f (formula-factor (:formula q))]
                        (if (zero? f) m (/ m f))))
  (display-string [q] (let [dv (display-value q), us (format-formula (:formula q))] ; `str`, not
                        (str dv (when (seq us) (str " " us))            ; `pr-str`: no BigInt `N`
                             " ≈ " (double dv) " " (dim-bracket (dims q)))))

  ApproxQuantity
  (display-value  [q] (let [m (magnitude q), f (formula-factor (:formula q))]
                        (if (zero? f) m (ndiv m f))))
  (display-string [q] (let [dv (display-value q), us (format-formula (:formula q))] ; leading ≈,
                        (str "≈ " dv (when (seq us) (str " " us))       ; full BigDecimal (no `M`)
                             " " (dim-bracket (dims q)))))

  Number
  (display-value  [n] n))                                 ; a plain number is itself

;; `pr`/`prn`/the REPL emit a tagged literal whose payload is `display-string`. Two tags
;; only: `#commensura/unit` (named — reifies via the registry) and
;; `#commensura/quantity` (anonymous). Precise vs approx is NOT in the tag — it shows
;; in the payload (a leading `≈` marks approximate) and the reader dispatches on that.
(defmethod print-method PreciseUnit     [q ^java.io.Writer w]
  (.write w (str "#commensura/unit " (pr-str (display-string q)))))
(defmethod print-method ApproxUnit      [q ^java.io.Writer w]
  (.write w (str "#commensura/unit " (pr-str (display-string q)))))
(defmethod print-method PreciseQuantity [q ^java.io.Writer w]
  (.write w (str "#commensura/quantity " (pr-str (display-string q)))))
(defmethod print-method ApproxQuantity  [q ^java.io.Writer w]
  (.write w (str "#commensura/quantity " (pr-str (display-string q)))))

;; clojure.pprint (CIDER/Calva) ignores print-method on records — delegate back.
(defmethod pp/simple-dispatch PreciseUnit     [q] (print-method q *out*))
(defmethod pp/simple-dispatch ApproxUnit      [q] (print-method q *out*))
(defmethod pp/simple-dispatch PreciseQuantity [q] (print-method q *out*))
(defmethod pp/simple-dispatch ApproxQuantity  [q] (print-method q *out*))
