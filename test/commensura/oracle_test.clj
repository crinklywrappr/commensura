(ns commensura.oracle-test
  "Oracle tests against the REAL Frink engine (opt-in `:frink` alias; see deps.edn). Every test
  self-skips when Frink is absent, so the default `clojure -X:test` is unaffected. The strong claim:
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
(deftest fixed-oracle-cases
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
(defspec product-magnitude-matches-frink 200
  (prop/for-all [scalar gen-scalar
                 terms  (gen/vector gen-term 1 4)]
    (or (not (o/available?))
        (let [c-mag (q/magnitude (reduce (fn [acc [nm e]] (c/by acc (c/pow (lookup nm) e)))
                                         scalar terms))
              fstr  (str "(" scalar ") " (str/join " " (map (fn [[nm e]] (str nm "^(" e ")")) terms)))]
          (= c-mag (:rational (o/frink-eval fstr)))))))

;; ---- generative: sums / differences of conforming quantities (plus, minus) ----
(defspec sum-magnitude-matches-frink 150
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

;; ---- irrational spot-checks: no exact rational, so compare within the reader's 1e-9 tolerance ----
(deftest irrational-spot-checks
  (when (o/available?)
    (doseq [[label c-val frink]
            [["pi metre"        (double (q/magnitude (c/by u/pi u/meter)))                 "pi meter"]
             ["sqrt 2 metre"    (double (q/magnitude (m/sqrt (u/meter 2))))                "sqrt[2] meter"]
             ["circle area"     (double (q/magnitude (c/by u/pi (c/pow (u/meter 3) 2))))   "pi (3 meter)^2"]]]
      (testing label
        (let [fr (:approx (o/frink-eval frink))]
          (is (< (rel-diff c-val fr) 1e-9) (str "commensura " c-val " vs frink " fr)))))))
