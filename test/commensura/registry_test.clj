(ns commensura.registry-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [commensura.registry :as reg]
            [commensura.quantity :as q]
            [commensura.dimensions :as dims]
            [commensura.units]                 ; load so builtin units are registered
            commensura.reader                  ; the #commensura/unit reader (resolves via resolvers)
            [taoensso.trove :as trove]))

;; clear-* mutate global state the rest of the suite relies on — snapshot and restore the
;; unit/dimension tables and the resolver list around each test.
(use-fixtures :each
  (fn [f]
    (let [u (reg/all-units), d (reg/all-dimensions), r (reg/all-unit-resolvers)]
      (try (f)
        (finally
          (reg/clear-units!)      (doseq [[k v] u] (reg/register-unit! k v))
          (reg/clear-dimensions!) (doseq [[k v] d] (reg/register-dimension! k v))
          (reg/clear-unit-resolvers!) (doseq [{:keys [pred dispatch]} r]
                                        (reg/register-unit-resolver! pred dispatch)))))))

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

(deftest user-unit-resolver
  (testing "a user resolver builds a whole name family on demand; the reader reifies it"
    (reg/register-unit-resolver!
     (fn [nm] (boolean (re-matches #"\d+dozen" nm)))
     (fn [nm] (q/unit nm (* (parse-long (re-find #"\d+" nm)) 12) {})))   ; Ndozen = N·12 (dimensionless)
    (testing "direct resolution builds the member"
      (is (= 36 (q/magnitude (reg/resolve-unit "3dozen"))))
      (is (= {} (q/dims (reg/resolve-unit "3dozen")))))
    (testing "the #commensura/unit reader reifies a member never registered"
      (is (= 60 (q/magnitude (read-string "#commensura/unit \"5dozen [dimensionless]\"")))))
    (testing "a name no resolver claims still misses / errors"
      (is (nil? (reg/resolve-unit "7bogus")))
      (is (thrown? clojure.lang.ExceptionInfo
                   (read-string "#commensura/unit \"7bogus [dimensionless]\""))))))

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
