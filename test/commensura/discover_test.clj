(ns commensura.discover-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.core :as c]
            [commensura.units :as u]
            [commensura.quantity :as q]
            [commensura.registry :as registry]
            [commensura.discover :as d]))

;; a user-defined length unit (registers itself via defunit) — for the live-view tests
(c/defunit zorptron (u/meter 7))

(deftest search-units-substring-and-regex
  (testing "case-insensitive substring, ranked by closeness — the exact match leads"
    (let [hits (d/search-units "VOLT")]
      (is (= "volt" (first hits)))                 ; exact match first, not buried alphabetically
      (is (some #{"electronvolt"} hits))))
  (testing "regex query is returned alphabetically"
    (let [hits (d/search-units #"^milli")]
      (is (every? #(re-find #"^milli" %) hits))
      (is (= hits (sort-by clojure.string/lower-case hits))))))

(deftest units-of-dimension-by-map-value-and-name
  (testing "a dims-map, a unit value, and (u/foot) all agree for length"
    (let [by-map (d/units-of-dimension {:length 1})]
      (is (some #{"meter"} by-map))
      (is (some #{"foot"} by-map))
      (is (= by-map (d/units-of-dimension u/foot)))))            ; value → its dims
  (testing "zero exponents in the query are canonicalized away"
    (is (= (d/units-of-dimension {:length 1})
           (d/units-of-dimension {:length 1 :time 0}))))
  (testing "a human dimension name resolves"
    (is (some #{"mph"} (d/units-of-dimension "velocity")))))

(deftest units-of-dimension-really-conform
  (testing "every returned name is a unit of exactly that dimension"
    (is (every? (fn [nm] (= {:length 1} (q/dims (registry/lookup-unit nm))))
                (d/units-of-dimension {:length 1})))))

(deftest unknown-dimension-name-suggests
  (testing "a misspelled dimension name throws with a did-you-mean"
    (let [ex (try (d/units-of-dimension "veloctiy")
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex))
      (is (contains? (set (:suggestions (ex-data ex))) "velocity")))))

(deftest describe-reports-dimension
  (testing "a named (compound) dimension"
    (is (= "velocity" (:dimension (d/describe (c/per u/mile u/hour))))))
  (testing "a base dimension is honestly unnamed"
    (let [m (d/describe u/meter)]
      (is (= {:length 1} (:dimensions m)))
      (is (nil? (:dimension m)))
      (is (string? (:value m)))))
  (testing "a bare number is a dimensionless scalar"
    (is (= {}              (:dimensions (d/describe 5))))
    (is (= "dimensionless" (:dimension  (d/describe 5)))))
  (testing "a non-value (e.g. a keyword) is rejected"
    (is (thrown? clojure.lang.ExceptionInfo (d/describe :foo)))))

(deftest dimensions-lists-named-pairs
  (let [dims (d/dimensions)]
    (is (some (fn [[_ nm]] (= nm "velocity")) dims))
    (is (every? (fn [[dm nm]] (and (map? dm) (string? nm))) dims))))

(deftest reflects-live-registrations
  (testing "a freshly defunit'd unit shows up in both views (a live query, not a snapshot)"
    (is (some #{"zorptron"} (d/search-units "zorp")))
    (is (some #{"zorptron"} (d/units-of-dimension {:length 1})))))
