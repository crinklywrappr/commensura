(ns commensura.money-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [commensura.core :as c]
            [commensura.currency :as cur]
            [commensura.currency.rates :as rates]
            [commensura.quantity :as q]
            [commensura.units :as u])
  (:import [java.math RoundingMode]
           [org.joda.money CurrencyUnit Money]))

;; a fixed rate snapshot so currency units resolve offline (USD is identity, no fetch)
(def ^:private stub {"USD" 1, "EUR" 9/10, "JPY" 150, "ETH" 1/3000})
(defn- with-rates [f] (with-redefs [rates/rates (constantly stub)] (f)))

(deftest quantity->money-converts
  (with-rates
   (fn []
     (testing "an amount → Money at the currency's decimal places"
       (is (= "USD 19.99" (str (c/quantity->money (cur/USD 1999/100)))))     ; 2 dp
       (is (= "JPY 1235"  (str (c/quantity->money (cur/JPY 6173/5))))))      ; 0 dp: 1234.6 → 1235
     (testing "sub-minor-unit amounts round — HALF_EVEN by default, mode overridable"
       (is (= "USD 20.00" (str (c/quantity->money (cur/USD 3999/200)))))                     ; 19.995 → 20.00
       (is (= "USD 19.99" (str (c/quantity->money (cur/USD 3999/200) RoundingMode/FLOOR))))) ; → 19.99
     (testing "the builtin `dollar` unit aliases to USD"
       (is (= "USD 100.00" (str (c/quantity->money (u/dollar 100)))))))))

(deftest quantity->money-rejects-bad-inputs
  (with-rates
   (fn []
     (testing "a non-currency quantity throws"
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"currency quantity"
                             (c/quantity->money (u/meter 5)))))
     (testing "a compound currency (EUR²) is not a plain amount"
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"currency quantity"
                             (c/quantity->money (cur/EUR 3 5)))))
     (testing "a code Joda-Money doesn't know (crypto ETH) throws a clear error"
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not an ISO-4217 currency"
                             (c/quantity->money (cur/ETH 1))))))))

(deftest money->quantity-reenters-the-tower
  (with-rates
   (fn []
     (testing "a Money re-enters the exact tower at its ISO code and amount"
       (let [q (c/money->quantity (c/quantity->money (cur/USD 1999/100)))]
         (is (= {:currency 1} (q/dims q)))
         (is (= 1999/100 (q/display-value (c/to q u/dollar))))     ; exact, no double
         (is (= "USD" (:unit-name (first (q/formula q)))))))
     (testing "round-trips a currency with 0 decimal places (JPY)"
       (is (= 1235 (q/display-value (c/to (c/money->quantity (c/quantity->money (cur/JPY 1235)))
                                          (cur/JPY)))))))))

;; ---- round-trip property (item 7) -------------------------------------------------------------------
;; The strong claim: money->quantity and quantity->money are inverses across arbitrary currency
;; arithmetic. We take a Money (A) in one currency, re-enter the tower, turn it into a *rate* by
;; dividing through a random time span, convert the rate to a second currency, exit to Money (B), and
;; then unwind the whole thing — multiply the time span back in, convert to the original currency, and
;; exit to Money (A1). A1 must equal A up to a threshold we can compute exactly from the two exchange
;; rates and the divisor: each Money hop rounds to the currency's minor unit, and that ε propagates
;; through the ×(rate·n) unwind.
;;
;; Live only. This (and the two exact round-trips below) needs real exchange rates. Each is tagged
;; ^:live, so `clojure -X:build test` excludes the whole slice (`-e live`) and `test-live` runs it
;; (`-i live`, with CURRENCYFREAKS_API_KEY set) — the tag is what keeps CI from re-loading and re-running
;; these across every action. The property *also* guards on `live?` with `(or (not live?) …)` — the same
;; self-skip the oracle defspecs use (commensura.oracle-test): a fail-safe if the ns is run directly
;; without `-e live` and no key, since tag exclusion is applied only after the ns has fully loaded.
;;
;; That load-is-before-filter fact is also why `currency-pool` must be non-empty regardless of tags:
;; `(gen/elements currency-pool)` is evaluated when the defspec is *defined*, and throws on an empty
;; collection. So offline we seed the pool with the resolvable codes at a placeholder rate of 1 that no
;; running property ever reads (the `(or (not live?) …)` guard short-circuits before it would).

(def ^:private live? (boolean (System/getenv "CURRENCYFREAKS_API_KEY")))

;; Joda's ISO currencies that have a real minor unit (drop pseudo-currencies with getDecimalPlaces < 0).
(def ^:private joda-iso
  (into {} (for [^CurrencyUnit cu (CurrencyUnit/registeredCurrencies)
                 :when (>= (.getDecimalPlaces cu) 0)]
             [(.getCode cu) cu])))

;; Codes usable in the sweep: Joda knows them (with a minor unit), commensura ships a resolver, and a
;; rate exists in the pinned snapshot. Live: real rates. Offline: placeholder 1s just to keep the pool
;; non-empty for `gen/elements` — the properties run 0 times, so the values are never consulted.
(def ^:private rate-snapshot
  (if live?
    (rates/rates)
    (into {} (for [code (keys joda-iso) :when (rates/supported? code)] [code 1]))))

(def ^:private currency-pool
  (vec (for [code (keys joda-iso)
             :when (and (rates/supported? code) (contains? rate-snapshot code))]
         code)))

(defn- with-snapshot [f] (with-redefs [rates/rates (constantly rate-snapshot)] (f)))

(defn- pow10 [k] (reduce * 1N (repeat k 10)))             ; exact 10^k

(def ^:private gen-time-unit (gen/elements [u/second u/minute u/hour u/day u/week]))

(defspec ^:live money-round-trips-across-currencies-and-time 300
  (prop/for-all [c1        (gen/elements currency-pool)
                 c2        (gen/elements currency-pool)
                 minor     (gen/choose 1 1000000)         ; minor units of c1 (cents, yen, …)
                 time-unit gen-time-unit
                 n         (gen/choose 1 1000)]           ; integer time span
    (or (not live?)                                       ; no key → vacuous pass (fail-safe; normally -e live)
        (with-snapshot
         (fn []
           (let [cu1     (joda-iso c1)
                 dp1     (int (.getDecimalPlaces ^CurrencyUnit cu1))
                 dp2     (int (.getDecimalPlaces ^CurrencyUnit (joda-iso c2)))
                 u1      (rates/usd-value c1)             ; USD value of one c1
                 u2      (rates/usd-value c2)
                 divisor (time-unit n)                    ; the same time span used to divide and multiply
                 a       (Money/ofMinor ^CurrencyUnit cu1 (long minor))  ; (A)
                 q1      (c/money->quantity a)            ; back into the exact tower
                 rate    (c/per q1 divisor)               ; a c1/time rate
                 rate2   (c/to rate (c/per (rates/unit c2) divisor))  ; same rate, expressed in c2
                 b       (c/quantity->money rate2)        ; (B) — currency c2, rounded to its minor unit
                 q2      (c/money->quantity b)
                 back    (c/by q2 divisor)                ; multiply the time span back in
                 back1   (c/to back (c/by (rates/unit c1) divisor)) ; expressed in c1 again
                 a1      (c/quantity->money back1)        ; (A1)
                 ;; ε from the c2 hop is ½·10^-dp2; the unwind scales it by (u2·n/u1); a1 adds ½·10^-dp1
                 threshold (+ (* (/ u2 u1) n (/ 1 (* 2 (pow10 dp2))))
                              (/ 1 (* 2 (pow10 dp1))))]
             (and (= cu1 (.getCurrencyUnit ^Money a1))    ; came back to the original currency
                  (<= (abs (- (rationalize (.getAmount ^Money a1))
                              (rationalize (.getAmount ^Money a))))
                      threshold))))))))                    ; A1 == A within the provable rounding budget

;; ---- exact round-trips (live only) ------------------------------------------------------------------
;; No arithmetic that leaves the currency's minor unit, so these are *exact* — the input Money equals the
;; output Money, not merely within a threshold. money->quantity re-enters the tower losslessly and the
;; amount already sits at the currency's scale, so quantity->money reproduces it byte-for-byte, and `to`
;; only re-bases the display (magnitude is preserved). Live only, same ^:live tag + `(or (not live?) …)`
;; guard as the threshold spec above.

;; A → quantity → Money must return the identical Money.
(defspec ^:live money->quantity->money-is-exact 300
  (prop/for-all [code  (gen/elements currency-pool)
                 minor (gen/choose 0 100000000)]
    (or (not live?)
        (with-snapshot
         (fn []
           (let [a (Money/ofMinor ^CurrencyUnit (joda-iso code) (long minor))]
             (= a (c/quantity->money (c/money->quantity a)))))))))

;; A → quantity → convert to another currency → convert back → Money must return the identical Money.
(defspec ^:live currency-hop-round-trips-exactly 300
  (prop/for-all [code  (gen/elements currency-pool)
                 other (gen/elements currency-pool)
                 minor (gen/choose 0 100000000)]
    (or (not live?)
        (with-snapshot
         (fn []
           (let [a    (Money/ofMinor ^CurrencyUnit (joda-iso code) (long minor))
                 b    (c/to (c/money->quantity a) (rates/unit other))  ; (B) same value, in another currency
                 a1   (c/quantity->money (c/to b (rates/unit code)))]  ; back to A's currency, out to Money
             (= a a1)))))))
