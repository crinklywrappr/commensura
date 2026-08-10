(ns commensura.interval-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.core :as c]
            [commensura.interval :as iv]
            [commensura.units :as u]
            [commensura.quantity :as q]
            [commensura.reader]))            ; load so #commensura/quantity round-trips

(defn- dv [x] (q/display-value x))
(defn- ends [iv] [(dv (iv/lo iv)) (dv (iv/hi iv))])
(defn- within? [iv x]                        ; membership (comparison ops deferred to M3.2)
  (<= (q/magnitude (iv/lo iv)) (q/magnitude x) (q/magnitude (iv/hi iv))))

(deftest construction-and-accessors
  (testing "2-arg: bounds auto-ordered, no main value; a scalar is not an interval"
    (let [r (iv/interval 7 13)]
      (is (= [7 13] (ends r)))
      (is (nil? (iv/main-value r)))
      (is (iv/interval? r))
      (is (not (iv/interval? 10)))
      (is (= [7 13] (ends (iv/interval 13 7))))))          ; auto-order
  (testing "3-arg: carries a main value (best-known estimate), which must lie within [lo,hi]"
    (let [r (iv/interval 2 2.5 3)]
      (is (= [2 3] (ends r)))
      (is (= 5/2 (dv (iv/main-value r)))))
    (is (thrown? clojure.lang.ExceptionInfo (iv/interval 2 4 3)))))

(deftest addition-subtraction
  (is (= [4 6]    (ends (c/plus  (iv/interval 1 2) (iv/interval 3 4)))))
  (is (= [-3 -1]  (ends (c/minus (iv/interval 1 2) (iv/interval 3 4)))))
  (is (= [-13 -7] (ends (c/minus (iv/interval 7 13))))))   ; unary negate

(deftest multiplication-sign-cases
  (is (= [3 8]  (ends (c/by (iv/interval 1 2)   (iv/interval 3 4)))))   ; both positive
  (is (= [-4 8] (ends (c/by (iv/interval -1 2)  (iv/interval 3 4)))))   ; spans zero
  (is (= [3 8]  (ends (c/by (iv/interval -2 -1) (iv/interval -4 -3)))))) ; both negative

(deftest division
  (is (= [1/4 1] (ends (c/per (iv/interval 1 2) (iv/interval 2 4)))))
  (testing "dividing by an interval spanning zero throws"
    (is (thrown? clojure.lang.ExceptionInfo (c/per (iv/interval 1 2) (iv/interval -1 1))))))

(deftest power
  (is (= [0 4] (ends (c/pow (iv/interval -2 2) 2))))        ; even, spans zero → reaches 0
  (is (= [1 8] (ends (c/pow (iv/interval 1 2) 3))))
  (is (= [1 1] (ends (c/pow (iv/interval 3 7) 0)))))

(deftest scalar-interaction
  (testing "scalars act as degenerate intervals via the accessors — no promotion needed"
    (is (= [11 12] (ends (c/plus (iv/interval 1 2) 10))))   ; scalar 10
    (is (= [10 10] (ends (c/by (iv/interval 5 5) 2))))))    ; degenerate × scalar

(deftest inclusion-property
  (testing "a∈X, b∈Y ⇒ a·b ∈ X·Y (fundamental theorem of interval arithmetic)"
    (let [X (iv/interval 1 2) Y (iv/interval 3 4)]
      (is (within? (c/by X Y)   (* 3/2 7/2)))          ; 1.5·3.5 = 5.25 ∈ [3,8]
      (is (within? (c/plus X Y) (+ 3/2 7/2)))
      (is (within? (c/per X Y)  (/ 3/2 7/2))))))

(deftest main-value-propagates
  (testing "Frink's example: [2,2.5,3] * [7,8.2,9.4] = [14, 20.5, 28.2]"
    (let [r (c/by (iv/interval 2 2.5 3) (iv/interval 7 8.2 9.4))]
      (is (= 14    (dv (iv/lo r))))
      (is (= 141/5 (dv (iv/hi r))))                    ; 28.2
      (is (= 41/2  (dv (iv/main-value r))))))          ; 20.5 = 2.5·8.2, not the center 21.1
  (testing "dropped the moment an operand lacks a main value"
    (is (nil? (iv/main-value (c/by   (iv/interval 2 3)     (iv/interval 7 8.2 9.4)))))
    (is (nil? (iv/main-value (c/plus (iv/interval 2 2.5 3) (iv/interval 1 2))))))
  (testing "a scalar carries its own value as its main (mainValue[5]=5)"
    (is (= 5 (dv (iv/main-value (c/by (iv/interval 2 2.5 3) 2)))))))   ; 2.5·2 = 5

(deftest dimensioned-intervals
  (let [r (iv/interval (u/meter 1) (u/meter 3))]
    (is (= {:length 1} (q/dims (iv/lo r))))
    (is (= [1 3] (ends r)))
    (testing "conversion re-expresses both bounds"
      (is (= [100 300] (ends (c/to r u/cm)))))
    (testing "non-conforming endpoints rejected"
      (is (thrown? clojure.lang.ExceptionInfo (iv/interval (u/meter 1) (u/second 1)))))))

(deftest prints-as-a-plain-record-and-round-trips
  (testing "no bespoke tagged literal; the raw record round-trips (endpoints via #commensura/quantity)"
    (doseq [r [(iv/interval (u/feet 10) (u/feet 12))
               (iv/interval (u/feet 10) (u/feet 11.5) (u/feet 12))]]
      (is (= r (read-string (pr-str r))))))
  (testing "a plain interval carries no :main field (no `:main nil` eyesore)"
    (is (not (contains? (iv/interval (u/feet 10) (u/feet 12)) :main)))))
