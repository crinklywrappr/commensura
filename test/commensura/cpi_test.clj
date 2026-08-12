(ns commensura.cpi-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.cpi :as cpi]
            [commensura.core :as c]
            [commensura.quantity :as q]
            [commensura.units :as u]
            commensura.reader                          ; bind the #commensura/unit data-reader
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(deftest frink-year-to-year-oracle
  (testing "50 cents_1955_11 → dollars_1985_10 reproduces Frink's exact value (current-independent)"
    (is (= 1087/538
           (q/display-value (c/to (c/by 50 (cpi/cent 1955 11)) (cpi/usd 1985 10)))))
    (is (= 2.020446096654275 (double 1087/538)))))       ; == Frink's documented result

(deftest period-dollar-is-an-exact-currency-unit
  (testing "usd/cent mint currency units with exact rational factors"
    (is (= {:currency 1} (q/dims (cpi/usd 1960))))
    (is (= {:currency 1} (q/dims (cpi/cent 1960))))
    (is (ratio? (q/magnitude (cpi/usd 1955 11))))
    (is (= (* 1/100 (q/magnitude (cpi/usd 1960))) (q/magnitude (cpi/cent 1960)))))  ; cent = usd/100
  (testing "monthly ≠ annual for the same year; conversion to the base dollar stays exact"
    (is (not= (q/magnitude (cpi/usd 1960)) (q/magnitude (cpi/usd 1960 1))))
    (is (ratio? (q/display-value (c/to (cpi/usd 1913) u/dollar))))))

(deftest frink-name-parsing
  (testing "Frink-style names reify to the very same (memoized) unit"
    (is (= (cpi/usd 1960) (cpi/unit "dollar_1960") (cpi/unit "USD_1960") (cpi/unit "dollars_1960")))
    (is (= (cpi/cent 1965 10) (cpi/unit "cents_1965_10")))
    (is (thrown? clojure.lang.ExceptionInfo (cpi/unit "meter")))))

(deftest cross-source-integrity
  (testing "shipped cpi.edn agrees with the Frink fixture on every shared month (NSA CPI isn't revised)"
    (let [shipped (:monthly (edn/read-string (slurp (io/resource "commensura/cpi.edn"))))
          fixture (edn/read-string (slurp (io/resource "commensura/frink_cpi.edn")))]
      (is (seq fixture))
      (is (every? (fn [[period v]] (= v (get shipped period))) fixture)))))

(deftest unit-round-trips
  (testing "a minted period unit round-trips through pr-str/read-string"
    (let [x (cpi/usd 1960)]
      (is (= x (read-string (pr-str x)))))))

(deftest reader-reifies-historical-cold
  (testing "a #commensura/unit literal reifies a historical currency the reader has never minted"
    (let [u (read-string "#commensura/unit \"dollar_1934 [currency]\"")]   ; 1934 untouched elsewhere
      (is (= (cpi/usd 1934) u))
      (is (= {:currency 1} (q/dims u)))))
  (testing "a #commensura/quantity literal reifies its historical unit too"
    (let [x (read-string "#commensura/quantity \"5 cent_1942 [currency]\"")]
      (is (= {:currency 1} (q/dims x)))
      (is (= 5 (q/display-value x)))))
  (testing "a genuinely unknown unit still errors; pre-1913 surfaces the CPI-range error"
    (is (thrown? clojure.lang.ExceptionInfo (read-string "#commensura/unit \"blorp [length]\"")))
    (is (thrown? clojure.lang.ExceptionInfo (read-string "#commensura/unit \"dollar_1800 [currency]\"")))))
