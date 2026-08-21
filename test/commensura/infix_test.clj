(ns commensura.infix-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [commensura.infix :as fx :refer [fj $= to]]
            [commensura.core :as c :refer [defunit]]
            [commensura.quantity :as q]
            [commensura.units :as u]))

;; ---- the iconic frinj worked examples (exact values; commensura keeps the fractions frinj prints) ----
(deftest keyword-soup-worked-examples
  (testing "keyword soup builds a quantity; `:to` converts, keeping the target's dimension"
    (is (= 552960/77 (q/display-value (fj 10 :feet 12 :feet 8 :feet :to :gallons))))  ; bedroom of water
    (is (= 62/9      (q/display-value (fj :keg :to :case))))                           ; kegs → cases
    (is (= {:length 3} (q/dims (fj 10 :feet 12 :feet 8 :feet)))))                      ; a volume, dimension kept
  (testing "plural unit names fall back to the registered singular"
    (is (c/eq? (fj 3 :meters) (c/by 3 u/meter)))
    (is (c/eq? (fj 2 :inches) (c/by 2 u/inch))))
  (testing "`:per` divides the next factor"
    (is (c/eq? (fj 60 :miles :per :hour) (c/per (c/by 60 u/mile) u/hour))))
  (testing "a number-led `to` target yields the dimensionless count (not a re-expression)"
    (is (= 496/3 (q/magnitude (to (fj :keg) 12 :floz))))                               ; 12-floz cans in a keg
    (is (= {} (q/dims (to (fj :keg) 12 :floz)))))
  (testing "$= arithmetic over fj values, with the corvette hamburger classic"
    (is (= 1373/89 (q/display-value (to ($= (fj 48055 :dollars) / (fj 3115 :lb)) :dollars :per :lb))))
    (is (= 5669904625/10618817472                                                     ; 2-ton pool depth
           (q/display-value (to ($= (fj 2 :tons) / (fj 10 :feet 12 :feet :water)) :feet))))))

(deftest defunit-soup-is-resolvable-in-later-soups
  (testing "a unit defined from a soup via core/defunit resolves by name in later soups"
    (defunit beer (fj 12 :floz 3.2 :percent :water :per :alcohol))
    (let [beers (fj :magnum 13.5 :percent :to :beer)]                                  ; how many beers in a magnum
      (is (= {:length 3} (q/dims beers)))                                              ; beer is a volume; unit kept
      (is (< 14.0 (double (q/display-value beers)) 14.1)))))                           ; ≈ 14.07 beers

(deftest unknown-unit-is-a-clear-error
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown unit"
                        (fj 3 :zorkmids))))

;; ---- $= precedence + operator mapping matches the explicit core forms ----
(deftest infix-math-matches-core-ops
  (let [a (fj 3 :meter), b (fj 4 :meter), c- (fj 5 :meter)]
    (testing "* / + - map to by / per / plus / minus"
      (is (c/eq? ($= a * b)  (c/by a b)))
      (is (c/eq? ($= a / b)  (c/per a b)))
      (is (c/eq? ($= a + b)  (c/plus a b)))
      (is (c/eq? ($= a - b)  (c/minus a b))))
    (testing "** binds tighter than * and /"
      (is (c/eq? ($= a * b ** 2) (c/by a (c/pow b 2))))
      (is (c/eq? ($= a / b ** 2) (c/per a (c/pow b 2)))))
    (testing "* and / are left-associative and share precedence"
      (is (c/eq? ($= a / b * c-) (c/by (c/per a b) c-))))
    (testing "* / bind tighter than + - (scalar keeps the sum conforming)"
      (is (c/eq? ($= a + b * 2) (c/plus a (c/by b 2)))))))

(deftest comparison-operators-in-$=
  (testing "== != < > <= >= map to the core comparison verbs"
    (is (true?  ($= (fj 1 :foot) == (fj 12 :inch))))
    (is (false? ($= (fj 1 :foot) != (fj 12 :inch))))
    (is (true?  ($= (fj 3 :meter) < (fj 4 :meter))))
    (is (true?  ($= (fj 5 :meter) >= (fj 5 :meter))))
    (is (false? ($= (fj 5 :meter) > (fj 5 :meter)))))
  (testing "comparisons bind looser than + -"
    (is (true? ($= (fj 2 :meter) + (fj 1 :meter) == (fj 3 :meter))))))

;; a user-defined operator, registered at load time — as `defop` must be, since `$=` reads the operator
;; table when it macroexpands (below), before this ns's tests run.
(fx/defop oplus 3 commensura.core/plus)

(deftest defop-adds-an-operator
  (is (c/eq? ($= (fj 2 :meter) oplus (fj 3 :meter))
             (c/plus (fj 2 :meter) (fj 3 :meter)))))

(deftest to-reverses-mirrored-units
  (testing "a target whose dimension is the reciprocal of the source flips the source (frinj-style)"
    (is (= 1/2 (q/display-value (to (fj 2 :meter) :per :meter))))            ; length → per-length
    (is (= 888659513/26508645                                               ; QE2: gallons/foot → feet/gallon
           (q/display-value (-> ($= (fj 18 :tons) / (fj :hour) / (fj 28 :knot) / (fj 0.85 :kg :per :liter))
                                (to :feet :per :gallon)))))))

;; ---- generative: fj keyword soup == the explicit left-to-right product ----
(def ^:private unit-kws {:meter u/meter, :second u/second, :gram u/gram, :foot u/foot, :hour u/hour})

(def ^:private gen-pair (gen/tuple (gen/choose 1 100) (gen/elements (vec (keys unit-kws)))))

(defspec fj-soup-matches-explicit-product 200
  (prop/for-all [pairs (gen/vector gen-pair 1 4)]
    (let [via-fj   (apply fj (mapcat (fn [[n kw]] [n kw]) pairs))
          via-core (reduce (fn [acc [n kw]] (c/by (c/by acc n) (unit-kws kw))) (q/scalar 1) pairs)]
      (and (= (q/magnitude via-fj) (q/magnitude via-core))
           (= (q/dims via-fj) (q/dims via-core))))))

;; every `:per u` inverts exactly the following unit, so fj == the same product with those units divided
(defspec fj-per-inverts-the-next-unit 200
  (prop/for-all [pairs (gen/vector (gen/tuple (gen/choose 1 100)
                                              (gen/elements (vec (keys unit-kws)))
                                              gen/boolean)                 ; per?
                                   1 4)]
    (let [soup     (mapcat (fn [[n kw per?]] (if per? [n :per kw] [n kw])) pairs)
          via-fj   (apply fj soup)
          via-core (reduce (fn [acc [n kw per?]]
                             (let [acc (c/by acc n)]
                               (if per? (c/per acc (unit-kws kw)) (c/by acc (unit-kws kw)))))
                           (q/scalar 1) pairs)]
      (and (= (q/magnitude via-fj) (q/magnitude via-core))
           (= (q/dims via-fj) (q/dims via-core))))))
