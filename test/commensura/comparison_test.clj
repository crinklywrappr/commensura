(ns commensura.comparison-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.units :as u]
            [commensura.core :as c]
            [commensura.interval :as iv]
            [commensura.quantity :as q]))

;; ---- the qcompare primitive ----
(deftest qcompare-basics
  (testing "physical, dimension-checked ordering (unit-agnostic)"
    (is (zero? (q/qcompare (u/inch 12) (u/foot 1))))       ; 12 in == 1 ft physically
    (is (neg?  (q/qcompare (u/meter 999) (u/km 1))))
    (is (pos?  (q/qcompare (u/km 1) (u/meter 999))))
    (is (zero? (q/qcompare (u/meter 1) (u/meter 1)))))
  (testing "non-conforming dimensions throw"
    (is (thrown? clojure.lang.ExceptionInfo (q/qcompare (u/meter 1) (u/second 1))))
    (is (thrown? clojure.lang.ExceptionInfo (q/qcompare (u/meter 1) 2))))   ; length vs dimensionless
  (testing "approx promotion: precise vs approx compares by magnitude"
    (is (pos?  (q/qcompare (u/gram 1) u/planckmass)))      ; 1 g > ~2.18e-8 kg
    (is (neg?  (q/qcompare u/planckmass (u/gram 1))))
    (is (zero? (q/qcompare u/planckmass u/planckmass)))))  ; equal BigDecimals compareTo 0

;; ---- interval certainly/possibly (Frink's example and the boundary cases) ----
(deftest interval-certainly-possibly
  (testing "overlapping A=[1,3], B=[2,4] (Frink's example)"
    (let [A (iv/interval 1 3), B (iv/interval 2 4)]
      (is (c/possibly-lt? A B))
      (is (not (c/certainly-lt? A B)))
      (is (c/possibly-eq? A B))            ; they overlap
      (is (not (c/certainly-eq? A B)))
      (is (not (c/certainly-ne? A B)))))   ; not disjoint
  (testing "disjoint A=[1,2], B=[5,6] (A wholly below B)"
    (let [A (iv/interval 1 2), B (iv/interval 5 6)]
      (is (c/certainly-lt? A B))
      (is (c/possibly-lt? A B))
      (is (c/certainly-ne? A B))
      (is (not (c/possibly-eq? A B)))
      (is (not (c/certainly-gt? A B)))))
  (testing "touching A=[1,2], B=[2,4] (share the point 2)"
    (let [A (iv/interval 1 2), B (iv/interval 2 4)]
      (is (not (c/certainly-lt? A B)))     ; hi(A)=2 is not < lo(B)=2
      (is (c/certainly-le? A B))           ; hi(A)=2 <= lo(B)=2
      (is (c/possibly-eq? A B)))))         ; share the point 2

;; ---- plain relational: unambiguous or throw; scalars never throw ----
(deftest plain-relational
  (testing "unambiguous on disjoint intervals"
    (is (true?  (c/lt? (iv/interval 1 2) (iv/interval 5 6))))
    (is (false? (c/lt? (iv/interval 5 6) (iv/interval 1 2)))))
  (testing "throws on overlapping intervals"
    (is (thrown? clojure.lang.ExceptionInfo (c/lt? (iv/interval 1 3) (iv/interval 2 4))))
    (is (thrown? clojure.lang.ExceptionInfo (c/eq? (iv/interval 1 3) (iv/interval 2 4)))))
  (testing "scalars/quantities are an ordinary total order (never throw)"
    (is (true?  (c/eq? 2 2)))
    (is (false? (c/lt? 2 2)))
    (is (true?  (c/lt? 2 3)))
    (is (true?  (c/eq? (u/inch 12) (u/foot 1))))       ; physical equality across units
    (is (true?  (c/ne? (u/inch 13) (u/foot 1))))
    (is (true?  (c/gt? (u/km 1) (u/meter 999))))))

;; ---- dimensioned intervals + conformance ----
(deftest dimensioned-intervals
  (let [A (iv/interval (u/meter 1) (u/meter 3))
        B (iv/interval (u/meter 2) (u/meter 4))]
    (is (c/possibly-lt? A B))
    (is (not (c/certainly-lt? A B)))
    (is (thrown? clojure.lang.ExceptionInfo (c/lt? A B))))          ; overlap
  (testing "non-conforming intervals throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (c/certainly-lt? (iv/interval (u/meter 1) (u/meter 3))
                                  (iv/interval (u/second 1) (u/second 3)))))))
