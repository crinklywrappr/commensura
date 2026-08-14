(ns commensura.http-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.http :as http]))

;; `decimal->ratio` is the pure parser shared by both live sources (FRED, CurrencyFreaks); the
;; network `get-json` is exercised only by the env-gated live tests. Pin the exact-rational edges.
(deftest decimal->ratio-parsing
  (testing "decimals become exact rationals (no float contamination)"
    (is (= 1/2   (http/decimal->ratio "0.5")))
    (is (= 92/100 (http/decimal->ratio "0.92")))
    (is (= 3/1000 (http/decimal->ratio "0.003")))
    (is (ratio? (http/decimal->ratio "0.1"))))
  (testing "whole numbers (no decimal point) become BigInt"
    (is (= 14N (http/decimal->ratio "14")))
    (is (= 0N  (http/decimal->ratio "0"))))
  (testing "signs are honoured"
    (is (= -1/4 (http/decimal->ratio "-0.25")))
    (is (= 5N   (http/decimal->ratio "+5"))))
  (testing "surrounding whitespace is tolerated"
    (is (= 7/10 (http/decimal->ratio "  0.7 "))))
  (testing "non-numbers (FRED's missing-value \".\", junk, nil, blank) → nil"
    (is (nil? (http/decimal->ratio ".")))
    (is (nil? (http/decimal->ratio "N/A")))
    (is (nil? (http/decimal->ratio "")))
    (is (nil? (http/decimal->ratio nil)))))
