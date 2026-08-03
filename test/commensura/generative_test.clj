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
       (= (q/dimensions a) (q/dimensions b))))

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
    (= (q/dimensions (c/to qty b)) (q/dimensions qty))))

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
      (and (= 1 (q/magnitude r)) (empty? (q/dimensions r))))))

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
(defspec interval-inclusion-by 200
  (prop/for-all [x gen-iv-point y gen-iv-point]
    (iv/within? (c/by (:iv x) (:iv y)) (c/by (:pt x) (:pt y)))))
