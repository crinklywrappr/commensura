(ns commensura.provenance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [commensura.core :as c]
            [commensura.quantity :as q]
            [commensura.interval :as iv]
            [commensura.provenance :as prov :refer [with-provenance]]
            [commensura.units :as u]
            [commensura.reader]))

(deftest off-by-default-zero-footprint
  (testing "no recording ⇒ no metadata, and the value is untouched"
    (let [r (c/by (u/meter 3) (u/meter 4))]
      (is (false? prov/*record-provenance-on-step*))
      (is (nil? (meta r)))
      (is (not (prov/recorded? r))))))

(deftest recorded-value-is-invisible-and-equal
  (testing "a recorded value is = and prints identically to its unrecorded twin (process-local)"
    (let [rec   (with-provenance (c/by (u/meter 3) (u/meter 4)))
          plain (c/by (u/meter 3) (u/meter 4))]
      (is (prov/recorded? rec))
      (is (= rec plain))
      (is (= (pr-str rec) (pr-str plain))))))

(deftest records-op-and-inputs
  (with-provenance
    (let [a (u/meter 3), b (u/meter 4)
          r (c/by a b)]
      (is (= #'c/by (prov/op r)))                 ; the op is the fully-qualified var, not a bare symbol
      (is (= [a b] (prov/inputs r)))
      (is (= [] (prov/child-nodes r)))          ; a, b are freshly constructed leaves (no provenance)
      (is (not (prov/recorded? a))))))

(deftest variadic-records-one-node-over-all-operands
  (with-provenance
    (let [r (c/by (u/meter 2) (u/meter 3) (u/meter 4))]
      (is (= #'c/by (prov/op r)))
      (is (= 3 (count (prov/inputs r))))        ; one node over all three, not nested pairwise
      (is (= {:length 3} (q/dims r))))))

(deftest step-collapses-internals-under-one-name
  (testing "span reads as `span`, not its internal to/minus"
    (with-provenance
      (is (= #'c/span (prov/op (c/span (iv/interval (u/meter 6) (u/meter 11)) u/foot)))))))

(deftest inline-operands-are-not-child-nodes
  (with-provenance
    (let [x (c/by (u/meter 2) (u/meter 3))       ; a recorded child
          r (c/pow x 2)]
      (is (= #'c/pow (prov/op r)))
      (is (= [(prov/node x) 2] (prov/inputs r)))  ; x is folded in as its node; the exponent 2 is a leaf
      (is (= [(prov/node x)] (prov/child-nodes r))))))

(deftest dag-shares-a-reused-node
  (testing "a let-bound value used twice is one shared node (identity preserved), shown once + back-ref"
    (with-provenance
      (let [a (c/by (u/meter 3) (u/meter 4))
            r (c/plus a a)
            [i1 i2] (prov/inputs r)]
        (is (identical? i1 i2))                  ; the same object, not a copy
        (is (str/includes? (prov/explain-str r) "↑"))))))

(deftest history-and-explain
  (with-provenance
    (let [r (c/to (c/by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons)
          s (prov/explain-str r)]
      (is (= #'c/to (prov/op r)))
      (is (= 2 (count (prov/history r))))        ; the `to` node and the `by` node (leaves aren't nodes)
      (is (str/includes? s "commensura.core/to"))  ; op shown as the fully-qualified var
      (is (str/includes? s "commensura.core/by")))))

(deftest explain-lines-exposes-the-outline-as-data
  (with-provenance
    (let [lines (prov/explain-lines (c/to (c/by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons))]
      (is (vector? lines))
      (is (every? string? lines))
      (is (= 6 (count lines)))                          ; to, by, 3 feet leaves, gallon target
      (is (= (str/join "\n" lines) (prov/explain-str (c/to (c/by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons)))))))

(deftest replay-is-a-zipper-cursor
  (with-provenance
    (let [r (c/to (c/by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons)
          z (prov/replay r)]
      (is (= #'c/to (prov/op (zip/node z))))
      (is (= #'c/by (prov/op (zip/node (zip/down z)))))            ; down into the recorded child
      (is (= #'c/to (prov/op (zip/node (zip/up (zip/down z))))))))) ; …and back up

(prov/defstep double-it [q] (c/by q 2))

(deftest defstep-defines-a-recording-fn
  (with-provenance
    (let [r (double-it (u/meter 5))]
      (is (= #'double-it (prov/op r)))           ; the inner `by` is collapsed under the step's own var
      (is (= [(u/meter 5)] (prov/inputs r))))))

;; multi-arity + variadic defstep: every arity records under the fn's var, with that arity's operands
(prov/defstep combine
  ([x]        x)
  ([x y]      (c/by x y))
  ([x y & zs] (apply c/by x y zs)))

(deftest defstep-supports-multiple-arities
  (with-provenance
    (is (= #'combine (prov/op (combine (u/meter 2) (u/meter 5)))))
    (is (= {:length 2} (q/dims (combine (u/meter 2) (u/meter 5)))))
    (let [r (combine (u/meter 2) (u/meter 3) (u/meter 4))]   ; variadic arity folds the rest args in
      (is (= #'combine (prov/op r)))
      (is (= 3 (count (prov/inputs r))))
      (is (= {:length 3} (q/dims r))))))
