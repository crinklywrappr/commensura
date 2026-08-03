(ns commensura.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.core :as c :refer [by per plus minus pow to ratio]]
            [commensura.units :as u]
            [commensura.quantity :as q]))

(defn dv [x] (q/display-value x))

(deftest bedroom-volume-oracle
  (testing "Frink's flagship example is exact: 10ft × 12ft × 8ft in gallons"
    (is (= 552960/77
           (dv (to (by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons))))))

(deftest callable-units
  (testing "a unit var is a Quantity of one; calling it scales"
    (is (= 1 (dv u/foot)))
    (is (= 10 (dv (u/foot 10))))
    (is (= {:length 1} (q/dimensions (u/foot 10))))))

(deftest by-and-per
  (testing "by multiplies dims, per divides; numbers are dimensionless scalars"
    (is (= {:length 2} (q/dimensions (by u/foot u/foot))))
    (is (= {} (q/dimensions (per u/foot u/foot))))
    (is (= {:length 1 :time -1} (q/dimensions (per u/mile u/hour))))
    (is (= 60 (dv (by 5 (u/foot 12)))))))          ; 5 × 12 ft = 60 ft

(deftest plus-minus-conform
  (testing "same-dim add/subtract; non-conforming throws"
    (is (= 3 (dv (plus (u/foot 1) (u/foot 2)))))
    (is (= 0 (dv (minus (u/foot 2) (u/foot 2)))))
    (is (thrown? clojure.lang.ExceptionInfo (plus u/foot u/second)))))

(deftest to-is-dimension-preserving
  (testing "to keeps the dimension (unlike Frink's dimensionless ->)"
    (let [r (to (u/mile 1) u/foot)]
      (is (= 5280 (dv r)))
      (is (= {:length 1} (q/dimensions r))))          ; still a length
    (is (thrown? clojure.lang.ExceptionInfo (to u/foot u/second)))))

(deftest ratio-is-a-count
  (testing "ratio gives the bare dimensionless count"
    (let [r (ratio (u/mile 1) u/foot)]
      (is (= 5280 (dv r)))
      (is (= {} (q/dimensions r))))))

(deftest exactness-preserved
  (testing "arithmetic on exact inputs never produces a double"
    (let [m (q/magnitude (to (by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons))]
      (is (not (instance? Double m)))
      (is (ratio? (dv (to (by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons)))))))
