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

(deftest unit-predicate-covers-both-unit-records
  (is (q/unit? u/meter))              ; PreciseUnit
  (is (q/unit? u/planckmass))         ; ApproxUnit
  (is (not (q/unit? (u/meter 5))))    ; a quantity is not a unit
  (is (not (q/unit? 5))))

(deftest nth-root-of-a-bigdecimal-uses-the-decimal-path
  (testing "a BigDecimal base takes nth-root's `decimal?` branch (big-math root)"
    (let [r (q/ratpow (bigdec 8) 1/3)]                 ; cube root of 8.0M
      (is (decimal? r))
      (is (< (Math/abs (- 2.0 (double r))) 1e-9)))))

;; Every `defscalable` record is a first-class fn. Emit *literal* calls `(base 3 3 …n)` for arities
;; 0..22 — a literal call is what reaches the compiled fixed-arity `invoke`s (0..20) and, past 20, the
;; varargs `invoke`; `apply` exercises `applyTo`. n args scale the value n times, so the dimension goes
;; to the nth power (0 args ⇒ the value itself).
(defmacro ^:private arity-calls [base]
  (mapv (fn [n] [n (cons base (repeat n 3))]) (range 0 23)))

(defn- check-arities [pairs dim]
  (doseq [[n r] pairs]
    (is (q/displayable? r))
    (is (= {dim (if (zero? n) 1 n)} (q/dims r)))))

(deftest defscalable-is-callable-at-every-arity
  (testing "fixed invokes (0..20), the >20 varargs invoke, and applyTo — across all four records"
    (check-arities (arity-calls u/meter)      :length)                             ; PreciseUnit
    (check-arities (arity-calls u/planckmass) :mass)                               ; ApproxUnit
    (check-arities (arity-calls (u/meter 5))  :length)                             ; PreciseQuantity
    (check-arities (arity-calls (m/sqrt (c/by (u/meter 2) (u/meter 1)))) :length)  ; ApproxQuantity
    (testing "apply routes through applyTo, same result, for 0..22 args"
      (doseq [n (range 0 23)]
        (is (= {:length (if (zero? n) 1 n)} (q/dims (apply u/meter (repeat n 3)))))))))

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
