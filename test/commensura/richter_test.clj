(ns commensura.richter-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.richter :as r]
            [commensura.quantity :as q]
            [commensura.units :as u]
            [commensura.core :as c :refer [by]]))

(defn- relerr [got ref] (Math/abs (/ (- got ref) ref)))

(deftest magnitude-to-energy
  (testing "returns an approximate energy quantity"
    (let [E (r/magnitude->energy 7.0)]
      (is (q/approx? E))
      (is (= {:mass 1 :length 2 :time -2} (q/dims E)))))
  (testing "matches the Choy-Boatwright formula 22387*e^(3.45388*m) J"
    (doseq [m [4.0 6.0 7.5 9.5]]
      (is (> 1e-12 (relerr (double (q/magnitude (r/magnitude->energy m)))
                           (* 22387 (Math/exp (* 3.45388 m)))))))))

(deftest energy-to-magnitude
  (testing "returns an approximate dimensionless magnitude"
    (let [m (r/energy->magnitude (r/magnitude->energy 7.0))]
      (is (q/approx? m))
      (is (q/dimensionless? m))))
  (testing "matches -2.9 + 0.28953*ln(E/J), independent of the energy's unit basis"
    (doseq [j [1e12 7.0795e14 3.98e18]]
      (let [ref (+ -2.9 (* 0.28953 (Math/log j)))]
        (is (> 1e-12 (relerr (double (q/magnitude (r/energy->magnitude (by j u/joule)))) ref)))
        ;; same physical energy expressed in ergs (1 J = 1e7 erg) → same magnitude
        (is (> 1e-12 (relerr (double (q/magnitude (r/energy->magnitude (u/erg (* 1e7 j))))) ref)))))))

(deftest round-trips-within-the-constants
  (testing "forward then inverse recovers the magnitude to the precision Frink's rounded constants allow"
    (doseq [m [5.0 7.0 9.5]]                          ; 0.28953 ≠ 1/3.45388, so ~2e-5 drift is expected
      (is (> 5e-5 (Math/abs (- m (double (q/magnitude (r/energy->magnitude (r/magnitude->energy m)))))))))))

(deftest guards
  (is (thrown? clojure.lang.ExceptionInfo (r/magnitude->energy (u/meter 3))))
  (is (thrown? clojure.lang.ExceptionInfo (r/energy->magnitude 7.0))))
