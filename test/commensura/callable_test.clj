(ns commensura.callable-test
  "The callable-record surface (M7 item 5): a unit/quantity applied to n args."
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.core :as c]
            [commensura.units :as u]
            [commensura.quantity :as q]
            [commensura.temperature :as t]))

(deftest unit-invoke-arities
  (testing "n=0 returns the unit itself (direct and via apply)"
    (is (identical? u/meter (u/meter)))
    (is (identical? u/meter (apply u/meter []))))
  (testing "n=1 scales — the existing behavior"
    (is (c/eq? (u/meter 3) (c/by 3 u/meter)))
    (is (= {:length 1} (q/dims (u/meter 3)))))
  (testing "n≥2 is the product of the scaled values ⇒ unit^n (direct and via apply)"
    (is (= {:length 2} (q/dims (u/meter 3 5))))
    (is (= {:length 3} (q/dims (u/meter 3 5 9))))
    (is (= {:length 3} (q/dims (apply u/meter [3 5 9]))))
    (is (c/eq? (u/meter 3 5)           (c/by (u/meter 3) (u/meter 5))))
    (is (c/eq? (apply u/meter [3 5 9]) (c/by (u/meter 3) (u/meter 5) (u/meter 9)))))
  (testing "no dimension gating — a compound unit powers up too (N² is defined, if unusual)"
    (is (= {:mass 2 :length 2 :time -4} (q/dims (u/newton 3 5))))))

(deftest quantity-is-callable-too
  (let [q3 (u/meter 3)]
    (is (identical? q3 (q3)))                       ; n=0 → itself
    (is (c/eq? (q3 4) (u/meter 12)))))              ; n=1 → scale

(deftest high-arity-and-apply
  (testing "direct calls work across the fixed positional range (0..20)"
    (is (= {:length 9} (q/dims (u/meter 1 2 3 4 5 6 7 8 9)))))    ; well past the old 8-arg cap
  (testing "a literal call past 20 hits IFn's varargs invoke (the 22nd method)"
    (is (= {:length 21}                                            ; apply would use applyTo — this is a
           (q/dims (u/meter 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)))))  ; genuine 21-arg call
  (testing "apply covers any arity"
    (is (= {:length 30} (q/dims (apply u/meter (repeat 30 1)))))))

(deftest affine-units-excluded-by-construction
  (testing "celsius/fahrenheit are plain fns (absolute temperature), so the record change can't misfire"
    (is (c/eq? (t/celsius 0) (u/kelvin 5463/20)))))          ; 0°C = 273.15 K, unchanged
