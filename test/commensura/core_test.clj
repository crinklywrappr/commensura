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
    (is (= {:length 1} (q/dims (u/foot 10))))))

(deftest by-and-per
  (testing "by multiplies dims, per divides; numbers are dimensionless scalars"
    (is (= {:length 2} (q/dims (by u/foot u/foot))))
    (is (= {} (q/dims (per u/foot u/foot))))
    (is (= {:length 1 :time -1} (q/dims (per u/mile u/hour))))
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
      (is (= {:length 1} (q/dims r))))          ; still a length
    (is (thrown? clojure.lang.ExceptionInfo (to u/foot u/second)))))

(deftest ratio-is-a-count
  (testing "ratio gives the bare dimensionless count"
    (let [r (ratio (u/mile 1) u/foot)]
      (is (= 5280 (dv r)))
      (is (= {} (q/dims r))))))

(deftest exactness-preserved
  (testing "arithmetic on exact inputs never produces a double"
    (let [m (q/magnitude (to (by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons))]
      (is (not (instance? Double m)))
      (is (ratio? (dv (to (by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons)))))))

(deftest verb-arities
  (testing "single-arg verbs are identity (the arithmetic-neutral edge)"
    (is (= (u/foot 3) (by (u/foot 3))))
    (is (= (u/foot 3) (per (u/foot 3))))
    (is (= (u/foot 3) (plus (u/foot 3)))))
  (testing "3+-arg by/per/plus/minus fold left"
    (is (= 24 (dv (by (u/foot 2) 3 4))))                    ; 2·3·4 ft
    (is (= 1  (dv (per (u/foot 24) 4 6))))                  ; 24/4/6 ft
    (is (= 6  (dv (plus (u/foot 1) (u/foot 2) (u/foot 3)))))
    (is (= 5  (dv (minus (u/foot 10) (u/foot 2) (u/foot 3)))))))  ; 10-2-3

(deftest register-dimension-verb
  (testing "the core verb names a dimension map so quantities print it"
    (is (= "quaternary space" (c/register-dimension! {:length 4} "quaternary space")))
    (is (= "quaternary space" (last (re-find #"\[(.*)\]" (str (pow u/meter 4))))))))
