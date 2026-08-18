(ns commensura.suggest-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [commensura.suggest :as suggest]))

(deftest ranks-the-obvious-typo-first
  (testing "the closest candidate leads"
    (is (= "meter"    (first (suggest/nearest "metre"    ["liter" "meter" "gram"]))))
    (is (= "kilogram" (first (suggest/nearest "kilogrma" ["gram" "kilogram" "kilograms"]))))))

(deftest transposition-is-a-single-edit
  (testing "adjacent swaps score 1 (Damerau), so metre still finds meter under the tight cutoff"
    (is (= ["meter"] (suggest/nearest "metre" ["meter"])))))

(deftest case-and-underscore-insensitive
  (is (= ["kilogram"]    (suggest/nearest "KILO_GRAM"  ["kilogram"])))
  (is (= ["dollar_1960"] (suggest/nearest "dollar1960" ["dollar_1960"]))))

(deftest silent-on-a-wild-miss
  (testing "nothing close ⇒ empty, so callers can stay quiet"
    (is (= [] (suggest/nearest "zzzzzzzz" ["meter" "gram" "liter"])))
    (is (= [] (suggest/nearest ""         ["meter"])))
    (is (= [] (suggest/nearest "meter"    [])))))

(deftest respects-the-limit
  (is (<= (count (suggest/nearest "meterx" ["meter" "meters" "metre" "meten" "meser"] 2)) 2)))

(deftest distance-is-normalized-osa
  (is (= 0 (suggest/distance "meter" "METER")))         ; case-insensitive
  (is (= 0 (suggest/distance "kilo_gram" "kilogram")))  ; underscore-insensitive
  (is (= 1 (suggest/distance "metre" "meter")))         ; adjacent transposition = one edit
  (is (= 2 (suggest/distance "volt" "abvolt"))))        ; two insertions

(deftest cutoff-is-tunable
  (testing "an explicit cutoff overrides the length-relative default"
    (is (= []        (suggest/nearest "meterrr" ["meter"] 3 1)))    ; 2 edits, cutoff 1 → excluded
    (is (= ["meter"] (suggest/nearest "meterrr" ["meter"] 3 5)))))  ; generous cutoff → included

;; An exact candidate is always its own nearest match: distance 0 sorts ahead of everything.
;; Candidates are lower-cased + distinct so no two collide under normalization (which would tie at 0).
;; Names are bounded to realistic (short) lengths — unit names, not paragraphs.
(def ^:private gen-name (gen/fmap str/join (gen/vector gen/char-alphanumeric 1 12)))

(defspec exact-match-is-first 200
  (prop/for-all [raw (gen/vector gen-name 1 8)]
    (let [names (->> raw (map str/lower-case) (remove str/blank?) distinct vec)]
      (every? (fn [c] (= c (first (suggest/nearest c names)))) names))))
