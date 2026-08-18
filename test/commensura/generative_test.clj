(ns commensura.generative-test
  "Property-based tests (clojure.test.check). Generators draw magnitudes from the
  EXACT tower only, so the exact-equality laws are meaningful."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [commensura.units :as u]
            [commensura.core :as c]
            [commensura.interval :as iv]
            [commensura.math :as m]
            [commensura.quantity :as q]
            [commensura.reader]))   ; loads the #commensura/… data readers for the round-trip law

;; ---- helpers ----
(defn q= "Same physical quantity (magnitude + dimensions), ignoring display unit."
  [a b]
  (and (= (q/magnitude a) (q/magnitude b))
       (= (q/dims a) (q/dims b))))

;; ---- generators (exact magnitudes only) ----
(def gen-exact
  (gen/one-of [gen/small-integer
               (gen/fmap (fn [[a b]] (/ a b)) (gen/tuple gen/small-integer gen/s-pos-int))]))
(def gen-nonzero (gen/such-that (complement zero?) gen-exact 100))

(def unit-groups
  {:length [u/meter u/foot u/mile u/inch u/km]
   :time   [u/second u/hour u/minute u/day]
   :mass   [u/kilogram u/gram u/pound u/ton]})
(def all-units (into [] (concat (:length unit-groups) (:time unit-groups) (:mass unit-groups)
                                [u/watt u/joule u/newton u/gallon])))

(def gen-unit (gen/elements all-units))
(def gen-quantity (gen/let [n gen-exact,   uu gen-unit] (uu n)))
(def gen-nonzero-quantity (gen/let [n gen-nonzero, uu gen-unit] (uu n)))

(def gen-conforming                     ; [q unitA unitB] all same dimension
  (gen/let [group (gen/elements (vals unit-groups))
            n gen-exact, a (gen/elements group), b (gen/elements group)]
    [(a n) a b]))
(def gen-conforming-two                 ; [x y] same dimension
  (gen/let [group (gen/elements (vals unit-groups))
            n1 gen-exact, n2 gen-exact, a (gen/elements group), b (gen/elements group)]
    [(a n1) (b n2)]))

(def gen-iv-point                       ; an interval and a point provably inside it
  (gen/let [uu gen-unit, n1 gen-exact, n2 gen-exact, p gen/nat, w gen/s-pos-int]
    (let [lo (uu (min n1 n2)), hi (uu (max n1 n2))
          frac (/ p (+ p w 1))]         ; in [0,1)
      {:iv (iv/interval lo hi)
       :pt (c/plus lo (c/by frac (c/minus hi lo)))})))

;; ---- conversion ----
(defspec conversion-round-trips 200
  (prop/for-all [[qty a b] gen-conforming]
    (q= (c/to (c/to qty b) a) qty)))

(defspec to-preserves-dimension 200
  (prop/for-all [[qty a b] gen-conforming]
    (= (q/dims (c/to qty b)) (q/dims qty))))

(defspec conforms-reflexive 100
  (prop/for-all [qty gen-quantity] (q/conforms? qty qty)))

;; ---- multiplicative group ----
(defspec by-commutative 200
  (prop/for-all [x gen-quantity y gen-quantity] (q= (c/by x y) (c/by y x))))

(defspec by-associative 200
  (prop/for-all [x gen-quantity y gen-quantity z gen-quantity]
    (q= (c/by (c/by x y) z) (c/by x (c/by y z)))))

(defspec per-self-is-one 200
  (prop/for-all [qty gen-nonzero-quantity]
    (let [r (c/per qty qty)]
      (and (= 1 (q/magnitude r)) (empty? (q/dims r))))))

(defspec pow-two-is-self-product 200
  (prop/for-all [qty gen-quantity] (q= (c/pow qty 2) (c/by qty qty))))

;; ---- additive group ----
(defspec plus-commutative 200
  (prop/for-all [[x y] gen-conforming-two] (q= (c/plus x y) (c/plus y x))))

(defspec minus-self-is-zero 200
  (prop/for-all [qty gen-quantity] (zero? (q/magnitude (c/minus qty qty)))))

;; ---- ratio / to relation ----
(defspec ratio-reconstructs 200
  (prop/for-all [[qty a b] gen-conforming]
    ;; (ratio q b)·b has the same magnitude+dims as q
    (q= (c/by (c/ratio qty b) b) qty)))

;; ---- exactness invariant (commensura's core promise) ----
(defspec arithmetic-stays-exact 300
  (prop/for-all [x gen-quantity y gen-quantity]
    (not (instance? Double (q/magnitude (c/by x y))))))

(defspec plus-stays-exact 200
  (prop/for-all [[x y] gen-conforming-two]
    (not (instance? Double (q/magnitude (c/plus x y))))))

(defspec decimal-inputs-rationalized 200
  (prop/for-all [d (gen/double* {:infinite? false :NaN? false})]
    (not (instance? Double (q/magnitude (c/by d u/meter))))))

;; ---- interval inclusion theorem ----
(defn- within?                          ; membership by magnitude (iv comparison ops deferred to M3.2)
  [iv x]
  (<= (q/magnitude (iv/lo iv)) (q/magnitude x) (q/magnitude (iv/hi iv))))

(defspec interval-inclusion-by 200
  (prop/for-all [x gen-iv-point y gen-iv-point]
    (within? (c/by (:iv x) (:iv y)) (c/by (:pt x) (:pt y)))))

;; ---- comparison laws (M3.2): certainly/possibly + qcompare consistency ----
(def gen-conforming-ivs                 ; two intervals sharing a unit (⇒ same dimension)
  (gen/let [uu gen-unit, a gen-exact, b gen-exact, c gen-exact, d gen-exact]
    [(iv/interval (uu a) (uu b)) (iv/interval (uu c) (uu d))]))

(defspec possibly-is-negation-of-opposite-certainly 300
  (prop/for-all [[x y] gen-conforming-ivs]
    (and (= (c/possibly-lt? x y) (not (c/certainly-ge? x y)))
         (= (c/possibly-le? x y) (not (c/certainly-gt? x y)))
         (= (c/possibly-gt? x y) (not (c/certainly-le? x y)))
         (= (c/possibly-ge? x y) (not (c/certainly-lt? x y)))
         (= (c/possibly-eq? x y) (not (c/certainly-ne? x y)))
         (= (c/possibly-ne? x y) (not (c/certainly-eq? x y))))))

(defspec certainly-implies-possibly 300
  (prop/for-all [[x y] gen-conforming-ivs]
    (and (or (not (c/certainly-lt? x y)) (c/possibly-lt? x y))
         (or (not (c/certainly-le? x y)) (c/possibly-le? x y))
         (or (not (c/certainly-gt? x y)) (c/possibly-gt? x y))
         (or (not (c/certainly-ge? x y)) (c/possibly-ge? x y))
         (or (not (c/certainly-eq? x y)) (c/possibly-eq? x y))
         (or (not (c/certainly-ne? x y)) (c/possibly-ne? x y)))))

(defspec scalar-trichotomy-matches-qcompare 300
  (prop/for-all [[x y] gen-conforming-two]
    (let [s (q/qcompare x y)]              ; scalars: certainly = plain, and never throw
      (cond
        (neg? s)  (and (c/certainly-lt? x y) (c/lt? x y) (not (c/eq? x y)))
        (zero? s) (and (c/certainly-eq? x y) (c/eq? x y) (not (c/lt? x y)) (not (c/gt? x y)))
        :else     (and (c/certainly-gt? x y) (c/gt? x y) (not (c/eq? x y)))))))

(defspec plain-op-certainly-pairs-are-mutually-exclusive 300
  ;; Each plain relational op decides via two certainly-* predicates — a claim and its strict
  ;; opposite. They must never both hold, or the op's `:else` (the ambiguous-overlap throw) would
  ;; be masked. This pins that invariant so the plain ops stay unambiguous.
  (prop/for-all [[x y] gen-conforming-ivs]
    (and (not (and (c/certainly-lt? x y) (c/certainly-ge? x y)))    ; lt? / ge?
         (not (and (c/certainly-le? x y) (c/certainly-gt? x y)))    ; le? / gt?
         (not (and (c/certainly-eq? x y) (c/certainly-ne? x y)))))) ; eq? / ne?

;; ---- math functions (M3.3) ----
(defspec sqrt-of-square-is-abs 200
  (prop/for-all [x gen-quantity]
    (q= (m/sqrt (c/pow x 2)) (m/abs x))))         ; exact on the perfect square √(x²) = |x|

(defspec abs-nonneg-and-idempotent 200
  (prop/for-all [x gen-quantity]
    (and (not (neg? (q/magnitude (m/abs x))))
         (q= (m/abs (m/abs x)) (m/abs x)))))

(defspec min-max-bound-both-operands 200
  (prop/for-all [[x y] gen-conforming-two]
    (and (<= (q/qcompare (m/min x y) x) 0) (<= (q/qcompare (m/min x y) y) 0)
         (>= (q/qcompare (m/max x y) x) 0) (>= (q/qcompare (m/max x y) y) 0))))

(defspec abs-interval-includes-abs-point 200
  (prop/for-all [x gen-iv-point]
    (within? (m/abs (:iv x)) (m/abs (:pt x)))))

(defspec sqrt-then-square-restores-dims 200        ; M3.4: √ halves dims (maybe fractional), ² restores
  (prop/for-all [x gen-quantity]
    (let [ax (m/abs x)]                             ; non-negative, so the real √ never throws
      (= (q/dims (c/pow (m/sqrt ax) 2)) (q/dims ax)))))

;; ---- print / read round-trip: the reader is the exact inverse of the printer (M7) ----
(defspec quantity-pr-read-round-trips 300
  (prop/for-all [qty gen-quantity]
    (= qty (read-string (pr-str qty)))))           ; full record equality, not just q=

(defspec unit-pr-read-round-trips 100
  (prop/for-all [uu gen-unit]                       ; units now carry :ns/:doc metadata;
    (= uu (read-string (pr-str uu)))))              ; `=` ignores it, so the round-trip still holds

;; ---- linearity: scaling distributes over addition ----
(defspec scaling-distributes-over-plus 200
  (prop/for-all [[x y] gen-conforming-two, k gen-exact]
    (q= (c/by k (c/plus x y))
        (c/plus (c/by k x) (c/by k y)))))

;; ---- dimension homomorphism: `by` adds exponents ----
(defn- add-dims [a b] (into {} (remove (comp zero? val)) (merge-with + a b)))
(defspec by-adds-dimensions 200
  (prop/for-all [x gen-quantity y gen-quantity]
    (= (q/dims (c/by x y)) (add-dims (q/dims x) (q/dims y)))))

;; ---- sign: a dimensionless trit (-1/0/1) that tracks the magnitude and negates ----
(defspec sign-tracks-magnitude 200
  (prop/for-all [x gen-quantity]
    (let [s (m/sign x), mag (q/magnitude x)]
      (and (contains? #{-1 0 1} s)
           (= (= s -1) (neg?  mag))
           (= (= s  1) (pos?  mag))
           (= (= s  0) (zero? mag))))))

(defspec sign-negates 200
  (prop/for-all [x gen-quantity]
    (= (m/sign (c/minus x)) (- (m/sign x)))))

;; ---- min / max: commutative + idempotent ----
(defspec min-max-commutative 200
  (prop/for-all [[x y] gen-conforming-two]
    (and (q= (m/min x y) (m/min y x))
         (q= (m/max x y) (m/max y x)))))

(defspec min-max-idempotent 200
  (prop/for-all [x gen-quantity]
    (and (q= (m/min x x) x) (q= (m/max x x) x))))

;; ---- callable records: applying to n args is the product of the scaled values (M7 item 5) ----
(defspec callable-is-product-of-scaled 200
  (prop/for-all [uu gen-unit, args (gen/vector gen-exact 2 6)]
    (q= (apply uu args) (reduce c/by (map uu args)))))

;; ---- monotone math brackets an interval (the spine for interval parity — M7 item 8) ----
;; floor/ceil/round/sign are non-decreasing, so mapping them over [lo,hi] must bracket the op at any
;; interior point. (abs is special-cased over zero-spanning intervals and is checked separately above.)
(defspec monotone-math-brackets-interval 200
  (prop/for-all [x gen-iv-point]
    (every? (fn [f] (within? (f (:iv x)) (f (:pt x))))
            [m/floor m/ceil m/round m/sign])))
