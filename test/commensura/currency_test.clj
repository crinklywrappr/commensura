(ns commensura.currency-test
  (:require [clojure.test :refer [deftest is testing]]
            [commensura.currency :as cur]
            [commensura.currency.rates :as rates]
            [commensura.core :as c]
            [commensura.quantity :as q]
            [commensura.units :as u]
            commensura.reader))                    ; the #commensura/unit reader (resolves currencies)

;; a fixed rate snapshot: units of CODE per 1 USD (exact rationals).
;; 1 USD = 0.9 EUR = 150 JPY = 1/2000 troy-oz gold.
(def ^:private stub {"USD" 1, "EUR" 9/10, "JPY" 150, "XAU" 1/2000})

(defn- with-rates [f] (with-redefs [rates/rates (constantly stub)] (f)))

(deftest currency-units-from-rates
  (with-rates
   (fn []
     (testing "a currency unit is worth (1/rate) dollars; USD is identity"
       (is (= 10/9 (q/magnitude (cur/EUR))))              ; 1 EUR = 1/0.9 = 10/9 USD
       (is (= {:currency 1} (q/dims (cur/EUR))))
       (is (= 1 (q/magnitude (rates/unit "USD")))))
     (testing "(CODE amount) scales, and `to` converts across currencies exactly"
       (is (= 1000/9  (q/display-value (c/to (cur/EUR 100) u/dollar))))   ; 100 EUR -> USD (÷0.9)
       (is (= 150     (q/display-value (c/to (u/dollar 1) (cur/JPY)))))   ; $1 -> 150 JPY
       (is (= 50000/3 (q/display-value (c/to (cur/EUR 100) (cur/JPY)))))) ; 100 EUR -> JPY: (1000/9)·150
     (testing "a precious metal is a currency unit (per troy ounce)"
       (is (= 2000 (q/magnitude (cur/XAU)))))              ; 1 troy oz = $2000
     (testing "an unsupported code errors"
       (is (thrown? clojure.lang.ExceptionInfo (rates/unit "NOTACODE")))))))

(deftest reader-reifies-currency-cold
  (with-rates
   (fn []
     (testing "a currency code reifies through the #commensura/unit reader"
       (let [eur (read-string "#commensura/unit \"EUR [currency]\"")]
         (is (= {:currency 1} (q/dims eur)))
         (is (= 10/9 (q/magnitude eur)))))
     (testing "a non-currency, non-unit name still errors"
       (is (thrown? clojure.lang.ExceptionInfo
                    (read-string "#commensura/unit \"NOTACODE [currency]\"")))))))

(deftest ^:live live-currencyfreaks    ; runs only when CURRENCYFREAKS_API_KEY is set (skipped in CI)
  (when (System/getenv "CURRENCYFREAKS_API_KEY")
    (testing "live fetch: USD identity, EUR and XAU positive"
      (is (= 1 (q/magnitude (rates/unit "USD"))))
      (is (pos? (q/magnitude (cur/EUR))))
      (is (pos? (q/magnitude (cur/XAU)))))))
