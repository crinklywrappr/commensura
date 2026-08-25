(ns commensura.uncertain-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [commensura.core :as c]
            [commensura.quantity :as q]
            [commensura.interval :as iv]
            [commensura.uncertain :as un :refer [plus-minus]]
            [commensura.math :as m]
            [commensura.units :as u]
            [commensura.reader]))                       ; load so #commensura/quantity round-trips

;; ---- helpers / generators (exact magnitudes; positive values so relative σ is well-defined) ----
(defn- mag [x] (double (q/magnitude x)))

(defn- mag≈
  "Magnitudes equal within a small relative tolerance (quadrature goes irrational via √)."
  [a b]
  (let [a (double a) b (double b)]
    (or (== a b) (< (Math/abs (- a b)) (* 1e-9 (+ 1.0 (Math/abs a) (Math/abs b)))))))

(def ^:private gen-pos     (gen/fmap (fn [[a b]] (/ (inc a) b)) (gen/tuple gen/nat gen/s-pos-int)))
(def ^:private gen-nonneg  (gen/fmap (fn [[a b]] (/ a b))       (gen/tuple gen/nat gen/s-pos-int)))
(def ^:private gen-unc     (gen/let [v gen-pos, s gen-nonneg] (plus-minus (u/meter v) (u/meter s))))

;; ---- construction, display, round-trip ------------------------------------------------------------
(deftest construction-display-and-round-trip
  (let [a (plus-minus (u/meter 5) (u/cm 2))]
    (testing "friendly str shows `value ± sigma [dimension]`, keeping each operand's own unit"
      (is (= "5 meter ± 2 cm [length]" (str a))))
    (testing "no reader tag: pr emits the raw record and it round-trips losslessly"
      (is (= a (read-string (pr-str a)))))
    (testing "accessors"
      (is (= 5 (q/display-value (un/value a))))
      (is (= 2 (q/display-value (un/sigma a))))
      (is (un/uncertain? a))
      (is (not (un/uncertain? (u/meter 5))))
      (is (mag≈ 1/250 (mag (un/relative a)))))))     ; 2 cm / 5 m = 0.004

(deftest constructor-guards
  (testing "spread must conform to the value"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"conform"
                          (plus-minus (u/meter 5) (u/second 2)))))
  (testing "spread must be non-negative"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"non-negative"
                          (plus-minus (u/meter 5) (u/meter -1)))))
  (testing "plus-minus-rel builds σ = |value|·frac"
    (is (= 1/20 (q/magnitude (un/sigma (un/plus-minus-rel (u/meter 5) 1/100)))))))  ; 5·0.01 = 0.05 m

;; ---- propagation laws -----------------------------------------------------------------------------
(defspec plus-and-minus-add-sigma-in-quadrature 200
  (prop/for-all [a gen-unc b gen-unc]
    (let [target (Math/sqrt (+ (* (mag (un/sigma a)) (mag (un/sigma a)))
                               (* (mag (un/sigma b)) (mag (un/sigma b)))))]
      (and (mag≈ (mag (un/sigma (c/plus a b)))  target)
           (mag≈ (mag (un/sigma (c/minus a b))) target)))))

(defspec times-and-div-add-relative-sigma-in-quadrature 200
  (prop/for-all [a gen-unc b gen-unc]
    (let [ra (mag (un/relative a)), rb (mag (un/relative b))
          target (Math/sqrt (+ (* ra ra) (* rb rb)))]
      (and (mag≈ (mag (un/relative (c/by a b)))  target)
           (mag≈ (mag (un/relative (c/per a b))) target)))))

(defspec pow-scales-relative-sigma-by-the-exponent 200
  (prop/for-all [a gen-unc, n (gen/choose 1 4)]
    (mag≈ (mag (un/relative (c/pow a n)))
          (* n (mag (un/relative a))))))

(defspec pow-then-sqrt-round-trips-value-and-sigma-exactly 200
  (prop/for-all [a gen-unc]
    (let [r (c/pow (c/pow a 2) 1/2)]                   ; nonlinear both ways, but exact for a rational base
      (and (= (q/magnitude (un/value r)) (q/magnitude (un/value a)))
           (= (q/magnitude (un/sigma r)) (q/magnitude (un/sigma a)))))))

(defspec to-carries-sigma-and-preserves-physical-magnitude 200
  (prop/for-all [a gen-unc]
    (let [t (c/to a u/foot)]
      (and (= (q/magnitude (un/value t)) (q/magnitude (un/value a)))
           (= (q/magnitude (un/sigma t)) (q/magnitude (un/sigma a)))
           (= (q/dims (un/value t)) (q/dims (un/value a)))))))

;; ---- plain operands auto-promote to σ=0 ----------------------------------------------------------
(defspec plain-times-plain-stays-plain 100
  (prop/for-all [n gen-pos, m gen-pos]
    (not (un/uncertain? (c/by (u/meter n) (u/meter m))))))

(defspec uncertain-plus-plain-leaves-sigma-unchanged 200
  (prop/for-all [a gen-unc, n gen-pos]
    (let [r (c/plus a (u/meter n))]                    ; adding an exact value shifts the centre, not σ
      (and (un/uncertain? r)
           (= (q/magnitude (un/sigma r)) (q/magnitude (un/sigma a)))))))

(defspec scaling-by-an-exact-constant-scales-sigma 200
  (prop/for-all [a gen-unc, k gen-pos]
    (mag≈ (mag (un/sigma (c/by a k))) (* (double k) (mag (un/sigma a))))))

;; ---- comparisons operate on the central value ----------------------------------------------------
(defspec comparisons-use-the-central-value 200
  (prop/for-all [a gen-unc, b gen-unc]
    (and (= (c/certainly-lt? a b) (c/certainly-lt? (un/value a) (un/value b)))
         (= (c/certainly-gt? a b) (c/certainly-gt? (un/value a) (un/value b))))))

;; ---- statistical agreement + the interval-mixing guard -------------------------------------------
(deftest statistical-predicates
  (let [a (plus-minus (u/meter 5.00) (u/cm 5))
        b (plus-minus (u/meter 5.03) (u/cm 4))         ; |Δ|=3 cm, combined σ=√(5²+4²)≈6.4 cm
        far (plus-minus (u/meter 8) (u/cm 5))]
    (testing "consistent? is 2σ agreement; within? takes an explicit threshold"
      (is (un/within? a b 1))                          ; 3 cm < 1·6.4 cm
      (is (un/consistent? a b))
      (is (not (un/consistent? a far))))               ; 3 m apart, ~42σ
    (testing "a plain reference carries σ=0"
      (is (un/within? (plus-minus (u/meter 5) (u/cm 10)) (u/meter 5.05) 1)))))

(deftest uncertain-and-interval-do-not-mix
  (testing "the core verbs reject an Uncertain combined with an Interval"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"mix.*Uncertain.*Interval"
                          (c/by (plus-minus (u/meter 5) (u/cm 2))
                                (iv/interval (u/meter 2) (u/meter 3)))))))

;; ---- commensura.math over uncertains -------------------------------------------------------------
(deftest math-functions-on-uncertains
  (let [a (plus-minus (u/meter 5) (u/cm 2))]
    (testing "sqrt/root/pow ride c/pow, so σ propagates"
      (is (= "5 meter ± 2 cm [length]" (str (m/sqrt (c/pow a 2)))))
      (is (= "5 meter ± 2 cm [length]" (str (m/root (c/pow a 3) 3)))))
    (testing "abs propagates σ around |value|"
      (let [r (m/abs (plus-minus (u/meter -5) (u/cm 2)))]
        (is (= 5 (q/display-value (un/value r))))
        (is (= 2 (q/display-value (un/sigma r))))))
    (testing "floor/ceil/round round the central value, keeping σ"
      (is (= "5 meter ± 2 cm [length]" (str (m/floor (plus-minus (u/meter 59/10) (u/cm 2))))))
      (is (= "6 meter ± 2 cm [length]" (str (m/ceil  (plus-minus (u/meter 51/10) (u/cm 2))))))
      (is (= "5 meter ± 2 cm [length]" (str (m/round (plus-minus (u/meter 54/10) (u/cm 2)))))))
    (testing "min/max compare central values and keep the winner's σ"
      (let [b (plus-minus (u/meter 3) (u/cm 1))]
        (is (un/uncertain? (m/min a b)))
        (is (= 3 (q/display-value (un/value (m/min a b)))))
        (is (= 1 (q/display-value (un/sigma (m/min a b)))))
        (is (= 5 (q/display-value (un/value (m/max a b)))))
        (is (not (un/uncertain? (m/min a (u/meter 3)))))))   ; exact operand wins → plain result
    (testing "sign reduces to the central value's sign (a plain number)"
      (is (= -1 (m/sign (plus-minus (u/meter -5) (u/cm 2)))))
      (is (=  1 (m/sign a))))
    (testing "mod/rem are guarded, like they are for intervals"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not defined on intervals or uncertains"
                            (m/mod a (u/meter 2))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not defined on intervals or uncertains"
                            (m/rem a (u/meter 2)))))
    (testing "min/max reject mixing an Uncertain with an Interval"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"mix.*Uncertain.*Interval"
                            (m/min a (iv/interval (u/meter 2) (u/meter 3))))))))
