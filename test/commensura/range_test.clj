(ns commensura.range-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [commensura.core :as c]
            [commensura.quantity :as q]
            [commensura.interval :as iv]
            [commensura.uncertain :as un :refer [plus-minus]]
            [commensura.math :as m]
            [commensura.units :as u]
            [commensura.reader]))

(deftest span-gives-a-dimensioned-extent
  (testing "interval: hi − lo, expressed in the unit"
    (let [s (c/span (iv/interval (u/meter 6) (u/meter 11)) u/foot)]
      (is (= 6250/381 (q/display-value s)))            ; (11−6) m = 5 m, in feet
      (is (= {:length 1} (q/dims s)))))
  (testing "uncertain: the full 2σ width"
    (is (= 4 (q/display-value (c/span (plus-minus (u/meter 5) (u/cm 2)) u/cm)))))
  (testing "a plain quantity is a point → zero span"
    (is (= 0 (q/display-value (c/span (u/meter 5) u/foot))))))

(deftest ticks-enumerates-the-range-unit-by-unit
  (let [i  (iv/interval (u/meter 6) (u/meter 11))
        xs (into [] (c/ticks i u/foot))]
    (testing "each mark is a quantity in the unit, within [lo, hi]"
      (is (every? #(= {:length 1} (q/dims %)) xs))
      (is (every? #(and (c/certainly-ge? % (u/meter 6)) (c/certainly-le? % (u/meter 11))) xs)))
    (testing "starts at the low bound and advances by exactly one unit"
      (is (= 2500/127 (q/display-value (first xs))))   ; 6 m in feet
      (is (c/certainly-eq? (c/minus (second xs) (first xs)) (u/foot 1))))
    (testing "half-open: every mark is strictly below hi"
      (is (every? #(c/certainly-lt? % (u/meter 11)) xs))
      (is (= 17 (count xs))))))

(deftest ticks-is-half-open-at-an-aligned-boundary
  (testing "a mark landing exactly on hi is excluded (like range)"
    ;; [1 m, 4 m] by the metre lands exactly on 4 m, which is dropped: {1, 2, 3}, not {1,2,3,4}.
    (is (= [1 2 3]
           (mapv q/display-value (into [] (c/ticks (iv/interval (u/meter 1) (u/meter 4)) u/meter)))))))

(deftest ticks-is-an-eduction-that-composes
  (let [i (iv/interval (u/meter 6) (u/meter 11))]
    (is (instance? clojure.core.Eduction (c/ticks i u/foot)))
    (testing "composes with into + transducers, reducing without an intermediate seq"
      (is (= 17 (count (into [] (c/ticks i u/foot)))))
      (is (= [20 21 22 23]                              ; whole-foot marks after rounding, first four
             (mapv q/display-value
                   (into [] (comp (map m/round) (take 4)) (c/ticks i u/foot))))))))

(deftest ticks-over-an-uncertain
  (testing "range is [value−σ, value+σ], half-open so the top bound (502 cm) is excluded"
    (is (= [498 499 500 501]                            ; [4.98, 5.02) m, marked in cm
           (mapv q/display-value (into [] (c/ticks (plus-minus (u/meter 5) (u/cm 2)) u/cm)))))))

;; The two verbs agree: half-open ticks count = ⌈span/unit⌉ (= floor(span/unit)+1 for the non-aligned
;; spans generated here — a whole number of metres is never a whole number of feet for w in 1..30).
(defn- floor-ratio [r] (if (ratio? r) (quot (numerator r) (denominator r)) (long r)))

(defspec ticks-count-matches-span 200
  (prop/for-all [lo (gen/choose 1 20), w (gen/choose 1 30)]
    (let [i (iv/interval (u/meter lo) (u/meter (+ lo w)))]
      (= (count (into [] (c/ticks i u/foot)))
         (inc (floor-ratio (q/display-value (c/span i u/foot))))))))
