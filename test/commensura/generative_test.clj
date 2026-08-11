(ns commensura.generative-test
  "Property-based tests (clojure.test.check). Generators draw magnitudes from the
  EXACT tower only, so the exact-equality laws are meaningful."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [commensura.units :as u]
            [commensura.core :as c]
            [commensura.interval :as iv]
            [commensura.quantity :as q]))

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

;; ---- exactness invariant (the core promise over frinj) ----
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
