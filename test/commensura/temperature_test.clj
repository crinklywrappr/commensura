(ns commensura.temperature-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.temperature :as t]
            [commensura.units :as u]
            [commensura.quantity :as q]))

(defn- dv [x] (q/display-value x))

(deftest forward-to-absolute-temperature
  (testing "a dimensionless reading becomes an absolute temperature (in kelvin), exactly"
    (is (= 5463/20 (dv (t/celsius 0))))            ; 0 °C   = 273.15 K
    (is (= 7463/20 (dv (t/celsius 100))))          ; 100 °C = 373.15 K
    (is (= 5463/20 (dv (t/fahrenheit 32))))        ; 32 °F  = 273.15 K
    (is (= 7463/20 (dv (t/fahrenheit 212))))       ; 212 °F = 373.15 K
    (is (= 5463/20 (dv (t/reaumur 0))))            ; 0 °Ré  = 273.15 K
    (is (= 7463/20 (dv (t/reaumur 80))))           ; 80 °Ré = 373.15 K
    (is (= {:temperature 1} (q/dims (t/celsius 100))))))

(deftest inverse-and-cross-scale
  (testing "a temperature becomes the scale reading (a bare number)"
    (is (= 100 (t/celsius    (t/celsius 100))))    ; round-trip
    (is (= 212 (t/fahrenheit (t/fahrenheit 212))))
    (is (= 80  (t/reaumur    (t/reaumur 80)))))
  (testing "cross-scale: 100 °C read on the other scales"
    (is (= 212 (t/fahrenheit (t/celsius 100))))    ; 100 °C = 212 °F
    (is (= 80  (t/reaumur    (t/celsius 100))))))   ; 100 °C = 80 °Ré

(deftest absolute-is-not-a-difference
  (testing "the affine fn is ABSOLUTE; the degree unit is a DIFFERENCE"
    (is (= 7463/20 (dv (t/celsius 100))))          ; absolute: 373.15 K
    (is (= 100     (dv (u/degcelsius 100))))))      ; difference: 100 K

(deftest guards
  (testing "a non-temperature, non-dimensionless argument is rejected"
    (is (thrown? clojure.lang.ExceptionInfo (t/celsius (u/meter 1))))))
