(ns commensura.quantity-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [commensura.quantity :as q]
            [commensura.core :as c]
            [commensura.math :as m]
            [commensura.units :as u]
            [commensura.reader]))

(defn- dv [x] (q/display-value x))

(deftest dims-of-plain-values
  (testing "a bare number / nil has no dimensions"
    (is (= {} (q/dims nil)))
    (is (= {} (q/dims 5)))
    (is (= {} (q/dims 3.2)))))

(deftest scale-rejects-a-non-quantity
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must be a unit or quantity"
                        (q/scale "not-a-quantity" 2))))

(deftest zeroth-power-is-dimensionless-one
  (let [r (q/qpow (u/meter 5) 0)]
    (is (= {} (q/dims r)))
    (is (= 1 (dv r)))))

(deftest odd-root-of-a-negative
  (testing "an odd root of a negative base is real (the negated root of the magnitude)"
    (is (= -2 (dv (q/qpow -8 1/3))))
    (is (= -3 (dv (q/qpow -27 1/3))))))

(deftest approximate-arithmetic-propagates
  (let [a (m/sqrt (c/by (u/meter 2) (u/meter 1)))]     ; √2 m — an ApproxQuantity
    (is (q/approx? a))
    (testing "any op touching an approx operand yields an approx result"
      (is (q/approx? (c/plus a (u/meter 1))))
      (is (q/approx? (c/minus a (u/meter 1))))
      (is (q/approx? (c/by a (u/meter 2))))
      (is (q/approx? (c/per a (u/meter 2))))
      (is (q/approx? (c/ratio a u/meter))))
    (testing "a negative integer power of an approx value (the BigDecimal reciprocal path)"
      (let [r (q/qpow a -1)]
        (is (q/approx? r))
        (is (= {:length -1} (q/dims r)))))))

(deftest approximate-values-display-with-a-leading-approx
  (is (str/starts-with? (str (u/planckmass)) "≈"))         ; ApproxUnit
  (is (str/starts-with? (str (m/sqrt (u/meter 2))) "≈")))  ; ApproxQuantity

(deftest negative-dimension-exponents-render
  (testing "an unnamed reciprocal dimension prints 1/… (and 1/…^n for exponents past -1)"
    (is (= "[1/length]"   (re-find #"\[[^\]]*\]" (str (q/qpow (u/meter 1) -1)))))   ; exponent -1
    (is (= "[1/length^2]" (re-find #"\[[^\]]*\]" (str (q/qpow (u/meter 1) -2))))))) ; exponent < -1

(deftest to-ignores-a-scaled-targets-coefficient
  (testing "`to` uses only the target's unit basis — a scaled target's coefficient is dropped (and warned)"
    (is (= 26400 (dv (c/to (u/mile 5) (u/foot 3)))))))   ; 5 miles in feet, not 3-foot units

(deftest pprint-delegates-to-the-tagged-literal
  (testing "clojure.pprint renders every commensura record via print-method (the CIDER/Calva path)"
    (doseq [v [u/meter                       ; PreciseUnit
               (u/meter 5)                   ; PreciseQuantity
               (u/planckmass)                ; ApproxUnit
               (m/sqrt (u/meter 2))]]        ; ApproxQuantity
      (is (str/starts-with? (str/trim (with-out-str (pp/pprint v))) "#commensura/")))))
