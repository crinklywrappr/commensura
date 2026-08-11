(ns commensura.fractional-dimension-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.math :as m]
            [commensura.core :as c]
            [commensura.interval :as iv]
            [commensura.quantity :as q]
            commensura.reader                        ; bind the #commensura/quantity data-reader
            [commensura.units :as u]))

(deftest fractional-dims-arise-and-render
  (testing "an odd-dimensioned root produces a rational dimension exponent"
    (is (= {:length 1/2} (q/dims (m/sqrt (u/meter 1)))))
    (is (= {:time -1/2}  (q/dims (m/sqrt (u/hertz 1)))))     ; Hz = 1/time
    (is (= {:length 2}   (q/dims (c/pow (u/liter 1) 2/3))))) ; liter = length^3
  (testing "rendered as unit^(p/q), and the [dim] bracket too"
    (is (= "1 meter^(1/2) [length^(1/2)]" (str (m/sqrt (u/meter 1)))))
    (is (= "1 liter^(2/3) [area]"         (str (c/pow (u/liter 1) 2/3))))
    (is (= "1 1/hertz^(1/2) [time^(1/2)]" (str (m/sqrt (c/per 1 (u/hertz 1))))))))

(deftest fractional-dims-compose-and-conform
  (testing "a root composes back to an integer dimension"
    (is (= {:time -1} (q/dims (c/by (m/sqrt (u/hertz 1)) (m/sqrt (u/hertz 1))))))
    (is (= {:length 1} (q/dims (c/pow (m/sqrt (u/meter 1)) 2)))))
  (testing "two fractional-dim quantities conform; a mismatch does not"
    (is (q/conforms? (m/sqrt (u/hertz 1)) (m/sqrt (u/hertz 1))))
    (is (not (q/conforms? (m/sqrt (u/hertz 1)) (m/sqrt (u/meter 1)))))))

(deftest fractional-dims-round-trip
  (testing "the #commensura/quantity literal round-trips (precise, approx, and the divisor form)"
    (doseq [x [(m/sqrt (u/meter 1))                 ; precise, exact display value
               (m/sqrt (u/foot 1))                  ; approx (irrational factor)
               (c/by 3/2 (m/sqrt (u/meter 1)))      ; non-integer precise display value
               (m/sqrt (c/per 1 (u/hertz 1)))       ; 1/hertz^(1/2) — exercises the paren-aware split
               (c/pow (u/liter 1) 2/3)]]
      (is (= x (read-string (pr-str x)))))))

(deftest fractional-dims-in-intervals
  (testing "an interval of fractional-dim endpoints works"
    (let [i (iv/interval (m/sqrt (u/meter 1)) (m/sqrt (u/meter 4)))]
      (is (= {:length 1/2} (q/dims (iv/lo i))))
      (is (= {:length 1/2} (q/dims (iv/hi i)))))))
