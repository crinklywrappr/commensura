(ns commensura.approx-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [commensura.units :as u]
            [commensura.core :as c :refer [by per plus pow to ratio]]
            [commensura.quantity :as q]
            [commensura.reader])
  (:import [java.math MathContext RoundingMode]))

(defn- approx
  "Test helper: force an approximate dimensionless quantity. Uses the raw
  ApproxQuantity constructor so even whole numbers stay approximate — bypassing the
  integer re-exactification that `q/quantity` applies to organic arithmetic results."
  [x]
  (q/->ApproxQuantity (bigdec x) []))

(deftest construction
  (testing "a forced approx is an ApproxQuantity with a BigDecimal magnitude"
    (let [a (approx 1.5)]
      (is (q/approx? a))
      (is (instance? BigDecimal (q/magnitude a)))
      (is (== 3/2 (q/magnitude a)))))                     ; == ignores scale
  (testing "scalar is idempotent on an existing ApproxQuantity"
    (let [a (approx 1.5)] (is (identical? a (q/scalar a))))))

(deftest promotion
  (testing "any approx operand promotes the result to ApproxQuantity"
    (is (q/approx? (by (approx 2.5) u/meter)))
    (is (q/approx? (by u/meter (approx 2.5))))            ; either side
    (is (q/approx? (per (approx 2.5) u/second)))
    (is (q/approx? (plus (approx 2.5) 5)))
    (is (q/approx? (pow (approx 1.5) 3)))
    (is (q/approx? (to (by (approx 2.5) u/kg) u/gram))))
  (testing "an approx result landing exactly on an integer re-exactifies to precise"
    (is (q/precise? (by (approx 2) u/meter)))             ; 2 × 1 = 2 exactly
    (is (not (q/approx? (pow (approx 2) 3)))))            ; 2³ = 8 exactly
  (testing "pure-exact arithmetic stays exact — a Quantity with a rational magnitude"
    (is (q/quantity? (by u/foot u/foot)))
    (is (not (q/approx? (by u/foot u/foot))))
    (is (not (instance? BigDecimal (q/magnitude (per u/mile u/hour)))))))

(deftest precision-follows-math-context
  (testing "with no *math-context* bound, defaults to 34-digit DECIMAL128"
    (is (= 34 (.precision (q/magnitude (per (approx 1) (approx 3)))))))  ; 0.333…(34)
  (testing "honors the caller's *math-context* (with-precision / binding)"
    (with-precision 5
      (is (= 5 (.precision (q/magnitude (per (approx 1) (approx 3)))))))
    (binding [*math-context* (MathContext. 10 RoundingMode/HALF_EVEN)]
      (is (= 10 (.precision (q/magnitude (per (approx 1) (approx 3)))))))))

(deftest irrational-power
  (testing "semitone^12 ≈ 2 (an octave), within precision"
    (let [semitone (approx (Math/pow 2.0 (/ 1.0 12)))
          octave   (pow semitone 12)]
      (is (q/approx? octave))
      (is (< (Math/abs (- 2.0 (double (q/magnitude octave)))) 1e-12)))))

(deftest prints-with-approx-marker
  (testing "both quantities use the #commensura/quantity tag; a leading ≈ marks approximate"
    (is (str/starts-with? (pr-str (approx 1.5)) "#commensura/quantity \"≈ "))
    (let [exact (pr-str (by u/foot u/foot))]                          ; exact: same tag, no leading ≈
      (is (str/starts-with? exact "#commensura/quantity \""))
      (is (not (str/starts-with? exact "#commensura/quantity \"≈"))))))

(deftest literal-round-trips
  (testing "approx literals reify to an equivalent printed form"
    (doseq [a [(approx 1.0594630943592953)                 ; dimensionless
               (by (approx 2.176434) u/kilogram)            ; dimensioned
               (per (approx 6.674) u/meter u/second)]]       ; compound
      (is (str/starts-with? (pr-str a) "#commensura/quantity \"≈ "))
      (is (= (pr-str a) (pr-str (read-string (pr-str a))))))))

(deftest builtin-irrational-units
  (testing "fractional-exponent units (planckmass = (ℏc/G)^(1/2)) are ApproxUnits with integer dims"
    (is (q/approx? u/planckmass))
    (is (= {:mass 1} (q/dims u/planckmass)))
    (is (instance? BigDecimal (q/magnitude u/planckmass)))
    (is (< (Math/abs (- 2.176434e-8 (double (q/magnitude u/planckmass)))) 1e-13)))  ; CODATA
  (testing "planck aliases equal their long-named units exactly"
    (doseq [[a b] [[u/m_P u/planckmass] [u/l_P u/plancklength]
                   [u/t_P u/plancktime] [u/T_P u/plancktemperature]]]
      (is (== (q/magnitude a) (q/magnitude b)))
      (is (= (q/dims a) (q/dims b)))))
  (testing "semitone is a dimensionless 2^(1/12); twelve of them make an octave, to build precision"
    (is (q/approx? u/semitone))
    (is (q/dimensionless? u/semitone))
    (is (> 1e-30 (.doubleValue (.abs (.subtract (q/magnitude (pow u/semitone 12)) 2M))))))
  (testing "an irrational unit prints and reifies through #commensura/unit"
    (is (str/starts-with? (pr-str u/planckmass) "#commensura/unit "))
    (is (== (q/magnitude u/planckmass)
            (q/magnitude (read-string (pr-str u/planckmass)))))))
