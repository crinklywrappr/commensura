(ns commensura.registry-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [commensura.registry :as reg]
            [commensura.dimensions :as dims]
            [commensura.units]                 ; load so builtin units are registered
            [taoensso.trove :as trove]))

;; clear-* mutate global state the rest of the suite relies on — snapshot and restore
;; both tables around each test.
(use-fixtures :each
  (fn [f]
    (let [u (reg/all-units), d (reg/all-dimensions)]
      (try (f)
        (finally
          (reg/clear-units!)      (doseq [[k v] u] (reg/register-unit! k v))
          (reg/clear-dimensions!) (doseq [[k v] d] (reg/register-dimension! k v)))))))

(deftest unit-registry
  (testing "register / lookup / all"
    (is (nil? (reg/lookup-unit "spamunit")))
    (is (= :fake (reg/register-unit! "spamunit" :fake)))   ; returns the value
    (is (= :fake (reg/lookup-unit "spamunit")))
    (is (contains? (reg/all-units) "spamunit")))
  (testing "clear empties the table"
    (reg/clear-units!)
    (is (empty? (reg/all-units)))
    (is (nil? (reg/lookup-unit "gallon")))))

(deftest dimension-registry
  (testing "register / lookup / all (zero exponents normalized out of the key)"
    (is (= "wobble" (reg/register-dimension! {:widget 3 :time 0} "wobble")))  ; returns nm
    (is (= "wobble" (reg/lookup-dimension {:widget 3})))
    (is (contains? (reg/all-dimensions) {:widget 3})))
  (testing "clear resets to the builtin ||| seed (user regs drop, builtins survive)"
    (reg/register-dimension! {:widget 3} "wobble")
    (reg/clear-dimensions!)
    (is (nil? (reg/lookup-dimension {:widget 3})))          ; user registration gone
    (is (= dims/names (reg/all-dimensions)))                ; exactly the seed
    (is (seq dims/names))))                                 ; and the seed is non-empty

(deftest redefine-warns
  (testing "re-registering a name to a *different* value warns (both tables); same value doesn't"
    (let [ids (atom [])]                                    ; trove/log! is a macro → capture via *log-fn*
      (binding [trove/*log-fn* (fn [_ns _coords _level id _payload] (swap! ids conj id))]
        (reg/register-dimension! {:widget 3} "a")           ; new       → no warn
        (reg/register-dimension! {:widget 3} "b")           ; differing → warn
        (reg/register-dimension! {:widget 3} "b")           ; same      → no warn
        (reg/register-unit! "spamunit" :x)                  ; new       → no warn
        (reg/register-unit! "spamunit" :y))                 ; differing → warn
      (is (= "b" (reg/lookup-dimension {:widget 3})))        ; last writer wins
      (is (= [:commensura.registry/dimension-redefined
              :commensura.registry/unit-redefined]
             @ids)))))                                       ; exactly those two warns, in order
