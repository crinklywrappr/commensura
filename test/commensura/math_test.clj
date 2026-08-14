(ns commensura.math-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.math :as m]
            [commensura.core :as c]
            [commensura.interval :as iv]
            [commensura.quantity :as q]
            [commensura.units :as u]))

(defn- dv   [x] (q/display-value x))
(defn- ends [i] [(dv (iv/lo i)) (dv (iv/hi i))])

;; ---- roots & rational powers ----
(deftest roots-and-powers
  (testing "exact when a perfect root (dimensions halve)"
    (let [r (m/sqrt (c/by (u/meter 3) (u/meter 3)))]           ; sqrt(9 m^2)
      (is (= 3 (dv r)))
      (is (= {:length 1} (q/dims r)))
      (is (q/precise? r)))
    (is (= 2 (dv (m/root 8 3))))                               ; cube root of 8
    (is (= 4 (dv (q/qpow 8 2/3)))))                            ; 8^(2/3)
  (testing "irrational root ⇒ ApproxQuantity"
    (let [r (m/sqrt 2)]
      (is (q/approx? r))
      (is (< (Math/abs (- 1.4142135 (double (dv r)))) 1e-6))))
  (testing "an odd-dimensioned root yields a fractional dimension (M3.4)"
    (let [r (m/sqrt (u/meter 2))]                             ; sqrt of 2 metres
      (is (= {:length 1/2} (q/dims r)))
      (is (q/approx? r)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sqrt -4)))))   ; even root of negative — complex, still throws

;; ---- value functions (scalar) ----
(deftest value-functions
  (testing "abs — dimension-preserving"
    (is (= [5 {:length 1}] [(dv (m/abs (u/meter -5))) (q/dims (m/abs (u/meter -5)))]))
    (is (= 5 (dv (m/abs 5)))))
  (testing "sign — dimensioned → plain -1/0/1"
    (is (= -1 (m/sign (u/meter -2))))
    (is (=  1 (m/sign (u/meter 2))))
    (is (=  0 (m/sign (u/meter 0)))))
  (testing "floor/ceil/round keep the unit, operate on the display value"
    (is (= [3 {:length 1}] [(dv (m/floor (u/meter 3.7))) (q/dims (m/floor (u/meter 3.7)))]))
    (is (= 3 (dv (m/floor (u/km 3.7)))))                       ; 3.7 km → 3 km, not base metres
    (is (= 4 (dv (m/ceil (u/meter 3.2)))))
    (is (= 4 (dv (m/round 3.7))))
    (is (= -2 (dv (m/floor -3/2)))))
  (testing "mod/rem — conforming, dimension-preserving"
    (is (= [1 {:time 1}] [(dv (m/mod (u/hour 25) (u/hour 24))) (q/dims (m/mod (u/hour 25) (u/hour 24)))]))
    (is (thrown? clojure.lang.ExceptionInfo (m/mod (u/meter 1) (u/second 1))))
    (is (= [1 {:time 1}] [(dv (m/rem (u/hour 25) (u/hour 24))) (q/dims (m/rem (u/hour 25) (u/hour 24)))]))
    (is (thrown? clojure.lang.ExceptionInfo (m/rem (u/meter 1) (u/second 1)))))
  (testing "min/max — physical order, keep the winner's unit"
    (is (= "6 inch [length]" (str (m/min (u/foot 1) (u/inch 6)))))
    (is (= "1 foot [length]" (str (m/max (u/foot 1) (u/inch 6)))))
    (is (= "2 meter [length]" (str (m/min (u/meter 5) (u/meter 2) (u/meter 9)))))
    (is (= "9 meter [length]" (str (m/max (u/meter 5) (u/meter 2) (u/meter 9)))))   ; max & more
    (is (= (u/meter 5) (m/min (u/meter 5))) )                                        ; single-arg identity
    (is (= (u/meter 5) (m/max (u/meter 5))))))

;; ---- interval versions (the monotone ones) ----
(deftest interval-math
  (testing "sqrt over an interval"
    (is (= [2 3] (ends (m/sqrt (iv/interval 4 9))))))
  (testing "abs over a zero-spanning interval reaches 0; else it is monotone"
    (is (= [0 3] (ends (m/abs (iv/interval (u/meter -2) (u/meter 3))))))
    (is (= [2 5] (ends (m/abs (iv/interval (u/meter -5) (u/meter -2)))))))
  (testing "abs propagates the main value"
    (let [i (m/abs (iv/interval (u/meter -2) (u/meter 1) (u/meter 3)))]
      (is (= [0 3] (ends i)))
      (is (= 1 (dv (iv/main-value i))))))
  (testing "floor/sign over an interval"
    (is (= [1 3]   (ends (m/floor (iv/interval 1.2 3.8)))))
    (is (= [-1 1]  (ends (m/sign (iv/interval -2 3))))))
  (testing "min/max are componentwise on the bounds"
    (is (= [1 3] (ends (m/min (iv/interval 1 4) (iv/interval 2 3)))))
    (is (= [2 4] (ends (m/max (iv/interval 1 4) (iv/interval 2 3))))))
  (testing "an interval fractional power needs a non-negative base"
    (is (thrown? clojure.lang.ExceptionInfo (m/sqrt (iv/interval -1 4))))))
