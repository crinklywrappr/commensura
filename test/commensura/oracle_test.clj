(ns commensura.oracle-test
  "Oracle tests against the REAL Frink engine (opt-in `:frink` alias; see deps.edn). Every test is
  tagged ^:oracle — `clojure -X:build test` excludes the slice (`-e oracle`) and `test-oracle` runs
  only it (`-i oracle`, with frink.jar); each also self-skips when Frink is absent, so a direct run
  without the tag filter is still a no-op. The strong claim:
  for the exact (rational) subset, commensura's base-SI magnitude is *bit-identical* to Frink's.

  commensura's `q/magnitude` is the value reduced to base SI; Frink reduces a bare expression to base
  SI too, so `(q/magnitude expr) == (frink-rational expr)` compares by/per/pow/plus/minus in one shot.
  The generators draw from `aligned-pool` — the units whose base factor Frink already reproduces
  exactly (`commensura.oracle/aligned-unit-names`) — an intersection that self-maintains as units.txt
  evolves."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as str]
            [commensura.oracle :as o]
            [commensura.core :as c]
            [commensura.math :as m]
            [commensura.units :as u]
            [commensura.quantity :as q]
            [commensura.registry :as registry]))

(defn- lookup [nm] (registry/lookup-unit nm))

;; The vetted pool + its grouping by dimension are computed once, and ONLY when Frink is present
;; (a dummy otherwise so generators still build and the properties short-circuit to `true`).
(def ^:private aligned-pool
  (delay (if (o/available?) (vec (o/aligned-unit-names)) ["meter"])))

(def ^:private pool-by-dim
  (delay (if (o/available?)
           (vec (group-by #(q/dims (lookup %)) @aligned-pool))
           [[{:length 1} ["meter"]]])))

;; ---- generators (garbage-but-harmless when Frink is absent; the properties ignore them then) ----
(def ^:private gen-scalar                                  ; small positive exact rationals (no doubles)
  (gen/fmap (fn [[p q]] (/ p q)) (gen/tuple (gen/choose 1 20) (gen/choose 1 12))))
(def ^:private gen-exp (gen/elements [1 1 2 3 -1 -2]))
(def ^:private gen-term (gen/tuple (gen/elements @aligned-pool) gen-exp))

(defn- rel-diff [a b] (/ (Math/abs (- a b)) (max 1.0 (Math/abs b))))

;; ---- fixed oracle cases (archived worked examples; commensura computed, compared to Frink) ----
(deftest ^:oracle fixed-oracle-cases
  (when (o/available?)
    (doseq [[label c-val frink]
            [["bedroom gallons"  (q/display-value (c/to (c/by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons))
              "10 feet 12 feet 8 feet -> gallons"]
             ["mph -> m/s"       (q/display-value (c/to (c/per u/mile u/hour) (c/per u/meter u/second)))
              "mile/hour -> meter/second"]
             ["10ft + 3in -> m"  (q/magnitude (c/plus (u/feet 10) (u/inch 3)))
              "10 feet + 3 inches -> meter"]
             ["2000 Cal/day -> W" (q/display-value (c/to (c/per (u/Calories 2000) u/day) u/watt))
              "2000 Calories/day -> watt"]
             ["corvette $/lb"    (q/display-value (c/to (c/per (u/dollars 48055) (u/lb 3115)) (c/per u/dollar u/lb)))
              "48055 dollars / (3115 lb) -> dollar/lb"]
             ["keg in cases"     (q/display-value (c/to u/keg u/case))
              "keg -> case"]]]
      (testing label
        (is (= c-val (:rational (o/frink-eval frink)))
            (str "commensura " c-val " vs frink " (:raw (o/frink-eval frink))))))))

;; ---- generative: products / quotients / integer powers (by, per, pow) ----
(defspec ^:oracle product-magnitude-matches-frink 200
  (prop/for-all [scalar gen-scalar
                 terms  (gen/vector gen-term 1 4)]
    (or (not (o/available?))
        (let [c-mag (q/magnitude (reduce (fn [acc [nm e]] (c/by acc (c/pow (lookup nm) e)))
                                         scalar terms))
              fstr  (str "(" scalar ") " (str/join " " (map (fn [[nm e]] (str nm "^(" e ")")) terms)))]
          (= c-mag (:rational (o/frink-eval fstr)))))))

;; ---- generative: sums / differences of conforming quantities (plus, minus) ----
(defspec ^:oracle sum-magnitude-matches-frink 150
  (prop/for-all [group (gen/elements @pool-by-dim)
                 a gen-scalar b gen-scalar
                 op (gen/elements [:+ :-])
                 idx (gen/tuple gen/nat gen/nat)]
    (or (not (o/available?))
        (let [names (second group)
              u1 (nth names (mod (first idx) (count names)))
              u2 (nth names (mod (second idx) (count names)))
              q  (if (= op :+) (c/plus  (c/by a (lookup u1)) (c/by b (lookup u2)))
                               (c/minus (c/by a (lookup u1)) (c/by b (lookup u2))))
              fstr (str "(" a ") " u1 " " (if (= op :+) "+" "-") " (" b ") " u2)]
          (= (q/magnitude q) (:rational (o/frink-eval fstr)))))))

;; ---- generative conversions: `to` display-value and `ratio` count ----
;; The magnitude specs above are invariant under `to`/`ratio` (magnitude is base-SI), so they cannot
;; touch the conversion math — the whole point of a unit library. A Frink `->` returns the value IN THE
;; TARGET unit, which is exactly commensura's display-value; `ratio` is that same number as a bare count.
;; One eval oracles both. (u1 and u2 are drawn from a single dimension group, so they conform.)
(defspec ^:oracle conversion-and-ratio-match-frink 200
  (prop/for-all [group (gen/elements @pool-by-dim)
                 s     gen-scalar
                 idx   (gen/tuple gen/nat gen/nat)]
    (or (not (o/available?))
        (let [names (second group)
              u1 (nth names (mod (first idx) (count names)))
              u2 (nth names (mod (second idx) (count names)))
              q  (c/by s (lookup u1))
              fr (:rational (o/frink-eval (str "(" s ") " u1 " -> " u2)))]
          (and (= fr (q/display-value (c/to q (lookup u2))))        ; (s u1) expressed in u2
               (= fr (q/magnitude    (c/ratio q (lookup u2)))))))))  ; …how many u2 fit — the same number

;; ---- generative dimension-carrying math: abs / min / max / mod (base magnitude) ----
;; These preserve dimension and agree with Frink on the base-SI magnitude. (floor/ceil/round are
;; dimensionless-only in Frink — oracled on scalars below; sqrt/root have the exact-root spec.)
(defspec ^:oracle dimensioned-math-matches-frink 200
  (prop/for-all [group (gen/elements @pool-by-dim)
                 a  gen-scalar
                 b  gen-scalar
                 idx (gen/tuple gen/nat gen/nat)
                 op (gen/elements [:abs :min :max :mod])]
    (or (not (o/available?))
        (let [names (second group)
              u1 (nth names (mod (first idx) (count names)))
              u2 (nth names (mod (second idx) (count names)))
              q1 (c/by a (lookup u1))
              q2 (c/by b (lookup u2))
              [c-mag fstr]
              (case op
                :abs [(q/magnitude (m/abs (c/by (- a) (lookup u1)))) (str "abs[(" (- a) ") " u1 "]")]
                :min [(q/magnitude (m/min q1 q2)) (str "min[(" a ") " u1 ", (" b ") " u2 "]")]
                :max [(q/magnitude (m/max q1 q2)) (str "max[(" a ") " u1 ", (" b ") " u2 "]")]
                :mod [(q/magnitude (m/mod q1 q2)) (str "(" a " " u1 ") mod (" b " " u2 ")")])]
          (= c-mag (:rational (o/frink-eval fstr)))))))

;; ---- generative exact roots: sqrt / root of a perfect power (dims halve/divide, value exact) ----
;; k^n · unit^n has an exact n-th root, k·unit, which Frink reproduces — exercising the rational-exponent
;; (fractional-dimension) path the integer-power product spec never reaches.
(defspec ^:oracle exact-roots-match-frink 150
  (prop/for-all [nm (gen/elements @aligned-pool)
                 k  (gen/choose 1 12)
                 n  (gen/elements [2 3 4])]
    (or (not (o/available?))
        (let [u    (lookup nm)
              kn   (reduce * 1 (repeat n k))              ; k^n, exact
              q    (c/by kn (c/pow u n))
              rt   (if (= n 2) (m/sqrt q) (m/root q n))
              fstr (str "((" kn ") " nm "^" n ")^(1/" n ")")]
          (= (q/magnitude rt) (:rational (o/frink-eval fstr)))))))

;; ---- generative scalar rounding: floor / ceil / round (dimensionless — Frink floor is scalar-only) ----
(defspec ^:oracle scalar-rounding-matches-frink 200
  (prop/for-all [s  gen-scalar
                 op (gen/elements [:floor :ceil :round])]
    (or (not (o/available?))
        (let [x (q/scalar s)
              [c-val fstr] (case op
                             :floor [(m/floor x) (str "floor[" s "]")]
                             :ceil  [(m/ceil  x) (str "ceil["  s "]")]
                             :round [(m/round x) (str "round[" s "]")])]
          (= (q/display-value c-val) (:rational (o/frink-eval fstr)))))))

;; ---- irrational spot-checks: no exact rational, so compare within the reader's 1e-9 tolerance ----
(deftest ^:oracle irrational-spot-checks
  (when (o/available?)
    (doseq [[label c-val frink]
            [["pi metre"        (double (q/magnitude (c/by u/pi u/meter)))                 "pi meter"]
             ["sqrt 2 metre"    (double (q/magnitude (m/sqrt (u/meter 2))))                "sqrt[2] meter"]
             ["circle area"     (double (q/magnitude (c/by u/pi (c/pow (u/meter 3) 2))))   "pi (3 meter)^2"]]]
      (testing label
        (let [fr (:approx (o/frink-eval frink))]
          (is (< (rel-diff c-val fr) 1e-9) (str "commensura " c-val " vs frink " fr)))))))
