(ns commensura.reader-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.units :as u]
            [commensura.core :as c]
            [commensura.reader :as r]))

(defn round-trip [q] (read-string (pr-str q)))

;; a user-defined unit (registers itself via the registry)
(c/defunit widget (u/meter 3))

(deftest literal-round-trips
  (testing "exact value is preserved through pr → read"
    (doseq [q [(c/to (c/by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons) ; named unit
               (c/per u/mile u/hour)                                       ; base form (velocity)
               (u/newton 3)                                                ; integer, named
               (c/per u/water u/alcohol)                                   ; dimensionless
               (c/per u/meter u/second u/kelvin)                           ; multi-divisor
               (c/by (c/per u/meter u/minute) (c/per u/kelvin u/second))]] ; nested per → flat
      (is (= q (round-trip q))))))

(deftest unit-literal-round-trips
  (testing "a bare Unit reifies via #commensura/unit through the registry"
    (is (= u/meter (round-trip u/meter)))            ; builtin
    (is (= widget (round-trip widget)))))            ; user defunit

(deftest multi-divisor-renders-with-a-slash-each
  (testing "each divisor gets its own /, matching (per a b c)"
    (let [s    (str (c/per u/meter u/second u/kelvin))          ; "1 meter/second/kelvin [dim]" (value 1 ⇒ no ≈)
          unit (subs s (inc (.indexOf s " ")) (.indexOf s " ["))]
      (is (= "meter/second/kelvin" unit)))))

(deftest rejects-inconsistent-approximation
  (testing "a tampered approximation throws (junk / drift guard)"
    (is (thrown? clojure.lang.ExceptionInfo
                 (read-string "#commensura/quantity \"552960/77 gallon ≈ 9999.0 [volume]\"")))))

(deftest accepts-drift-within-tolerance
  (testing "a last-digit difference inside the tolerance still reads"
    ;; 552960/77 ≈ 7181.298701298701; nudge the printed approx by ~1e-11 relative
    (is (some? (read-string "#commensura/quantity \"552960/77 gallon ≈ 7181.2987013 [volume]\"")))))

(deftest user-defunit-reifies-via-registry
  (testing "a user-defined unit round-trips now that defunit registers it"
    (is (= (widget 5) (round-trip (widget 5))))))

(deftest truly-unknown-unit-throws
  (testing "an unregistered unit name still can't be resolved"
    (is (thrown? clojure.lang.ExceptionInfo
                 (read-string "#commensura/quantity \"5 zorkmid ≈ 5.0 [length]\"")))))
