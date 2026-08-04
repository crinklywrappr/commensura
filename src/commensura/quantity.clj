;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.quantity
  "The two value types.

  A `Unit` is a *named* registered unit — a name, an exact base-SI magnitude, and
  a stored dimension map. `foot`, `meter`, `newton`, `beer` are all Units; a bare
  Unit is one of itself and prints `1 foot ≈ 1.0 [length]`. `defunit` mints Units.

  A `Quantity` is an *anonymous* computed value — an exact magnitude plus an
  ordered display `formula` (a vector of `UnitTerm`s). Its dimensions are *derived*
  from the formula, never stored, so they can't disagree with it. Every arithmetic
  result is a Quantity: `(by (u/feet 10) (u/feet 12) (u/feet 8))` carries `[foot^3]`
  and prints as cubic feet.

  So: named ⇒ Unit (stores dims), anonymous ⇒ Quantity (stores a formula); dims
  live in exactly one place. Both are callable (`(u/foot 10)` scales one foot to
  ten, yielding a Quantity) and both implement `Dimensionable`/`Measured`."
  (:require [commensura.registry :as registry]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

;; ---- protocols ----
(defprotocol Dimensionable
  (dims [x] "Base-dimension -> integer-exponent map (zero exponents removed)."))

(defprotocol Measured
  (magnitude [x]
    "Exact magnitude in base SI units. Plain numbers are `rationalize`d (3.2 ->
    16/5) so decimal inputs stay exact, matching Frink."))

;; ---- dimension-map helpers ----
(defn- clean [d] (into {} (remove (comp zero? val)) d))
(defn- scale-dims [d n] (clean (into {} (map (fn [[k v]] [k (* v n)])) d)))
(defn- merge-dims [a b] (clean (merge-with + a b)))

(declare scale as-formula format-quantity)

;; one term of a display formula, e.g. foot^3 — a Unit raised to a power
(defrecord UnitTerm [unit-name exp factor base-dims]
  Dimensionable
  (dims [_] (scale-dims base-dims exp)))          ; this term's dimensional contribution

;; a named registered unit: name + base magnitude + stored dimensions
(defrecord Unit [name mag dims]
  Dimensionable (dims [_] dims)
  Measured      (magnitude [_] mag)
  clojure.lang.IFn
  (invoke [this n] (scale this n))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))
  Object (toString [this] (format-quantity this)))

;; an anonymous computed value: magnitude + ordered display formula (dims derived)
(defrecord Quantity [mag formula]
  Dimensionable (dims [_] (reduce merge-dims {} (map dims formula)))
  Measured      (magnitude [_] mag)
  clojure.lang.IFn
  (invoke [this n] (scale this n))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))
  Object (toString [this] (format-quantity this)))

(extend-protocol Dimensionable
  Number (dims [_] {})
  nil    (dims [_] {}))
(extend-protocol Measured
  Number (magnitude [x] (rationalize x)))

(defn unit?     [x] (instance? Unit x))
(defn quantity? [x] (or (instance? Unit x) (instance? Quantity x)))

;; ---- constructors ----
(defn unit
  "Mint a named registered `Unit` from a name, base magnitude, and dimensions —
  what `defunit` and the generated builtins call."
  [name mag dims]
  (->Unit name mag (clean dims)))

(defn scalar
  "Wrap a plain number as a dimensionless Quantity (idempotent on Units/Quantities)."
  [n]
  (if (quantity? n) n (->Quantity (rationalize n) [])))

;; ---- accessors ----
(defn dimensionless? [x] (empty? (dims x)))
(defn conforms? [a b] (= (dims a) (dims b)))

(defn scale
  "Scale a Unit/Quantity's magnitude by a plain number — what `(u/feet 10)` does.
  Always yields an anonymous Quantity (the result is no longer *the* unit itself)."
  [q n]
  (->Quantity (* (magnitude q) (rationalize n)) (as-formula q)))

;; ---- display-formula algebra ----
(defn- as-formula
  "The display formula for any operand: a Quantity's own formula, a Unit promoted
  to a single term, or [] for plain numbers."
  [x]
  (cond
    (instance? Quantity x) (:formula x)
    (instance? Unit x)     [(->UnitTerm (:name x) 1 (:mag x) (:dims x))]
    :else                  []))

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
                              e  (reduce + (map :exp ts))]
                          (when-not (zero? e)
                            (assoc (first ts) :exp e))))
                      order)]
    (into (filterv (comp pos? :exp) merged)
          (filterv (comp neg? :exp) merged))))

(defn- formula-neg [formula] (mapv #(update % :exp -) formula))
(defn- formula-pow [formula n]
  (if (zero? n) [] (mapv #(update % :exp * n) formula)))

;; ---- exact integer power (keeps the exact tower) ----
(defn- expt [base n]
  (cond
    (zero? n) 1
    (pos? n)  (reduce * (repeat n base))
    :else     (/ 1 (reduce * (repeat (- n) base)))))

;; ---- arithmetic (always yields an anonymous Quantity) ----
(defn qmul [x y]
  (->Quantity (* (magnitude x) (magnitude y))
              (combine-terms (concat (as-formula x) (as-formula y)))))

(defn qdiv [x y]
  (->Quantity (/ (magnitude x) (magnitude y))
              (combine-terms (concat (as-formula x) (formula-neg (as-formula y))))))

(defn qpow [x n]
  (->Quantity (expt (magnitude x) n) (formula-pow (as-formula x) n)))

(defn- assert-conforms [op x y]
  (when-not (conforms? x y)
    (throw (ex-info (str op ": non-conforming dimensions")
                    {:a (dims x) :b (dims y)}))))

(defn qadd [x y]
  (assert-conforms "plus" x y)
  (->Quantity (+ (magnitude x) (magnitude y)) (as-formula x)))    ; keeps left's formula

(defn qsub [x y]
  (assert-conforms "minus" x y)
  (->Quantity (- (magnitude x) (magnitude y)) (as-formula x)))

(defn to
  "Re-express q over the target unit basis (dimension-preserving). The physical
  magnitude is unchanged — only the display formula becomes the target's, so the
  printed value equals magnitude(q)/factor(target)."
  [q target]
  (assert-conforms "to" q target)
  (->Quantity (magnitude q) (as-formula target)))

(defn ratio
  "Bare dimensionless count: how many of target fit in q."
  [q target]
  (assert-conforms "ratio" q target)
  (->Quantity (/ (magnitude q) (magnitude target)) []))

;; ---- display ----
(defn- formula-factor
  "Base magnitude of one of the compound display unit: the product of each term's
  factor raised to its exponent."
  [formula]
  (reduce (fn [acc t] (* acc (expt (:factor t) (:exp t)))) 1 formula))

(defn display-value
  "The exact value shown to the user: for a Quantity, base magnitude divided by
  the display formula's combined factor; a Unit is always one of itself (1); a
  plain number is itself."
  [q]
  (cond
    (instance? Quantity q) (let [f (formula-factor (:formula q))]
                             (if (zero? f) (:mag q) (/ (:mag q) f)))
    (instance? Unit q)     1
    :else                  q))

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

(defn- unit-string [q]
  (cond
    (instance? Quantity q) (format-formula (:formula q))
    (instance? Unit q)     (:name q)
    :else                  ""))

(defn format-quantity
  "Human-readable form: `<exact> <unit> ≈ <approx> [dimension]`. Used by
  `toString`/`str`/`println`, and as the payload of the `#commensura/…` literals."
  [q]
  (let [dv    (display-value q)
        d     (dims q)
        dname (dimension-name d)
        us    (unit-string q)]
    (str (pr-str dv)
         (when (seq us) (str " " us))
         " ≈ " (double dv) " "
         (cond dname   (str "[" dname "]")
               (seq d)  "[unknown dimension]"          ; unnamed compound — name it with register-dimension!
               :else    "[dimensionless]"))))

;; `pr`/`prn`/the REPL emit a tagged literal whose payload is the human-readable
;; string; the two records share the format but carry distinct tags.
(defmethod print-method Unit     [q ^java.io.Writer w]
  (.write w (str "#commensura/unit " (pr-str (format-quantity q)))))
(defmethod print-method Quantity [q ^java.io.Writer w]
  (.write w (str "#commensura/quantity " (pr-str (format-quantity q)))))

;; clojure.pprint (CIDER/Calva) ignores print-method on records — delegate back.
(defmethod pp/simple-dispatch Unit     [q] (print-method q *out*))
(defmethod pp/simple-dispatch Quantity [q] (print-method q *out*))
