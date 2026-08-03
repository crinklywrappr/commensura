(ns commensura.interval-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.core :as c]
            [commensura.interval :as iv]
            [commensura.units :as u]
            [commensura.quantity :as q]))

(defn dv [x] (q/display-value x))
(defn ends [iv] [(dv (iv/lo iv)) (dv (iv/hi iv))])

(deftest construction-and-accessors
  (let [r (iv/interval-pm 10 3)]                     ; [7, 13]
    (is (= [7 13] (ends r)))
    (is (= 10 (dv (iv/midpoint r))))
    (is (= 3  (dv (iv/radius r))))
    (is (= 6  (dv (iv/width r))))
    (is (iv/within? r 10))
    (is (not (iv/within? r 6))))
  (testing "endpoints auto-order"
    (is (= [7 13] (ends (iv/interval 13 7))))))

(deftest addition-subtraction
  (is (= [4 6]   (ends (c/plus  (iv/interval 1 2) (iv/interval 3 4)))))
  (is (= [-3 -1] (ends (c/minus (iv/interval 1 2) (iv/interval 3 4)))))
  (is (= [-13 -7] (ends (c/minus (iv/interval-pm 10 3))))))   ; unary negate

(deftest multiplication-sign-cases
  (is (= [3 8]  (ends (c/by (iv/interval 1 2)   (iv/interval 3 4)))))   ; both positive
  (is (= [-4 8] (ends (c/by (iv/interval -1 2)  (iv/interval 3 4)))))   ; spans zero
  (is (= [3 8]  (ends (c/by (iv/interval -2 -1) (iv/interval -4 -3)))))) ; both negative

(deftest division
  (is (= [1/4 1] (ends (c/per (iv/interval 1 2) (iv/interval 2 4)))))
  (testing "dividing by an interval spanning zero throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (c/per (iv/interval 1 2) (iv/interval -1 1))))))

(deftest scalar-promotion
  (is (= [11 12] (ends (c/plus (iv/interval 1 2) 10))))     ; scalar → [10 10]
  (testing "degenerate interval behaves like a scalar"
    (is (= [10 10] (ends (c/by (iv/interval 5 5) 2))))))

(deftest inclusion-property
  (testing "a∈X, b∈Y ⇒ a·b ∈ X·Y (fundamental theorem of interval arithmetic)"
    (let [X (iv/interval 1 2) Y (iv/interval 3 4)]
      (is (iv/within? (c/by X Y) (* 3/2 7/2)))          ; 1.5·3.5 = 5.25 ∈ [3,8]
      (is (iv/within? (c/plus X Y) (+ 3/2 7/2)))
      (is (iv/within? (c/per X Y) (/ 3/2 7/2))))))

(deftest dimensioned-intervals
  (let [r (iv/interval (u/meter 1) (u/meter 3))]
    (is (= {:length 1} (q/dimensions (iv/lo r))))
    (is (= 2 (dv (iv/midpoint r))))                    ; 2 m
    (is (= {:length 1} (q/dimensions (iv/midpoint r))))
    (testing "non-conforming endpoints rejected"
      (is (thrown? clojure.lang.ExceptionInfo
                   (iv/interval (u/meter 1) (u/second 1)))))))
