(ns commensura.money-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.core :as c]
            [commensura.currency :as cur]
            [commensura.currency.rates :as rates]
            [commensura.units :as u])
  (:import [java.math RoundingMode]))

;; a fixed rate snapshot so currency units resolve offline (USD is identity, no fetch)
(def ^:private stub {"USD" 1, "EUR" 9/10, "JPY" 150, "ETH" 1/3000})
(defn- with-rates [f] (with-redefs [rates/rates (constantly stub)] (f)))

(deftest converts-currency-quantities
  (with-rates
   (fn []
     (testing "an amount → Money at the currency's decimal places"
       (is (= "USD 19.99" (str (c/->money (cur/USD 1999/100)))))     ; 2 dp
       (is (= "JPY 1235"  (str (c/->money (cur/JPY 6173/5))))))      ; 0 dp: 1234.6 → 1235
     (testing "sub-minor-unit amounts round — HALF_EVEN by default, mode overridable"
       (is (= "USD 20.00" (str (c/->money (cur/USD 3999/200)))))                     ; 19.995 → 20.00
       (is (= "USD 19.99" (str (c/->money (cur/USD 3999/200) RoundingMode/FLOOR))))) ; → 19.99
     (testing "the builtin `dollar` unit aliases to USD"
       (is (= "USD 100.00" (str (c/->money (u/dollar 100)))))))))

(deftest rejects-bad-inputs
  (with-rates
   (fn []
     (testing "a non-currency quantity throws"
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"plain currency quantity"
                             (c/->money (u/meter 5)))))
     (testing "a compound currency (EUR²) is not a plain amount"
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"plain currency quantity"
                             (c/->money (cur/EUR 3 5)))))
     (testing "a code Joda-Money doesn't know (crypto ETH) throws a clear error"
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not an ISO-4217 currency"
                             (c/->money (cur/ETH 1))))))))
