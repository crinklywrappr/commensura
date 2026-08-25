;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.suggest
  "Dependency-free fuzzy name matching for \"did you mean?\" hints. A restricted
  Damerau-Levenshtein (optimal string alignment) ranker: given a misspelling and a
  pool of candidate names, return the closest few. Used to enrich the reader's
  unknown-unit error, and reusable anywhere a name lookup misses."
  (:require [clojure.string :as str]))

(defn- osa-distance
  "Optimal string alignment distance (restricted Damerau-Levenshtein) between `s` and
  `t`: insertion, deletion, substitution, and *adjacent* transposition each cost 1
  (so `metre`↔`meter` is one edit, not two). 'Restricted' = a substring is edited at
  most once — plenty for ranking typos."
  ^long [^String s ^String t]
  (let [m (int (count s))
        n (int (count t))]
    (cond
      (zero? m) n
      (zero? n) m
      :else
      ;; Flat (m+1)×(n+1) DP matrix in a single ^longs array (index = i*w + j), with primitive
      ;; loops — no boxing, no reflective 2-D aget/aset.
      (let [w        (inc n)
            ^longs d (long-array (* (inc m) w))]
        (dotimes [i (inc m)] (aset d (* i w) (long i)))
        (dotimes [j (inc n)] (aset d j (long j)))
        (loop [i 1]
          (when (<= i m)
            (let [ci (.charAt s (dec i))]
              (loop [j 1]
                (when (<= j n)
                  (let [cj   (.charAt t (dec j))
                        cost (if (= ci cj) 0 1)
                        base (min (inc (aget d (+ (* (dec i) w) j)))            ; delete s[i]
                                  (inc (aget d (+ (* i w) (dec j))))            ; insert t[j]
                                  (+ (aget d (+ (* (dec i) w) (dec j))) cost))  ; substitute
                        v    (if (and (> i 1) (> j 1)
                                      (= ci (.charAt t (- j 2)))
                                      (= (.charAt s (- i 2)) cj))
                               (min base (+ (aget d (+ (* (- i 2) w) (- j 2))) cost)) ; transpose
                               base)]
                    (aset d (+ (* i w) j) (long v))
                    (recur (inc j))))))
            (recur (inc i))))
        (aget d (+ (* m w) n))))))

(defn- osa-distance-within
  "Cutoff-aware OSA distance: like `osa-distance`, but only fills the diagonal band
  `|i−j| ≤ cutoff` (so O(m·cutoff) work instead of O(m·n)) and bails the moment a whole
  row's minimum exceeds `cutoff`. Returns the true distance when it is ≤ `cutoff`, else a
  sentinel `> cutoff` (specifically `cutoff+1`) meaning \"farther than we care about\".
  Two facts make the shortcuts exact: an optimal alignment of cost ≤ cutoff never leaves
  the band (leaving costs one indel per step), and row minima are non-decreasing (each
  cell ≥ its up-left diagonal), so a row entirely past cutoff can only lead to more."
  ^long [^String s ^String t ^long cutoff]
  (let [m   (int (count s))
        n   (int (count t))
        big (inc cutoff)]
    (cond
      (> (Math/abs (- m n)) cutoff) big
      (zero? m) (if (<= n cutoff) n big)
      (zero? n) (if (<= m cutoff) m big)
      :else
      ;; Same flat (m+1)×(n+1) DP array as `osa-distance`, but cells outside the band are
      ;; left at `big` (an INF that any real path beats) and never visited.
      (let [w        (inc n)
            ^longs d (long-array (* (inc m) w) big)]
        (dotimes [i (inc (min m cutoff))] (aset d (* i w) (long i)))   ; first column, band only
        (dotimes [j (inc (min n cutoff))] (aset d j (long j)))         ; first row, band only
        (loop [i 1]
          (if (> i m)
            (aget d (+ (* m w) n))
            (let [ci     (.charAt s (dec i))
                  jlo    (max 1 (- i cutoff))
                  jhi    (min n (+ i cutoff))
                  rowmin (loop [j jlo, rowmin big]
                           (if (> j jhi)
                             rowmin
                             (let [cj   (.charAt t (dec j))
                                   cost (if (= ci cj) 0 1)
                                   base (min (inc (aget d (+ (* (dec i) w) j)))            ; delete s[i]
                                             (inc (aget d (+ (* i w) (dec j))))            ; insert t[j]
                                             (+ (aget d (+ (* (dec i) w) (dec j))) cost))  ; substitute
                                   v    (if (and (> i 1) (> j 1)
                                                 (= ci (.charAt t (- j 2)))
                                                 (= (.charAt s (- i 2)) cj))
                                          (min base (+ (aget d (+ (* (- i 2) w) (- j 2))) cost)) ; transpose
                                          base)]
                               (aset d (+ (* i w) j) (long v))
                               (recur (inc j) (min rowmin v)))))]
              (if (> rowmin cutoff)
                big                                    ; whole row past cutoff ⇒ so is the result
                (recur (inc i))))))))))

(defn- normalize
  "Fold case and drop underscores, so `Kilo_gram` and `kilogram` compare as equal."
  [s]
  (-> (str s) str/lower-case (str/replace "_" "")))

(defn distance
  "Normalized restricted Damerau-Levenshtein (optimal string alignment) edit distance between
  `a` and `b`, case- and underscore-insensitive (adjacent transpositions cost 1). The public
  counterpart to `nearest` — handy for *ranking* an already-chosen set of matches by closeness
  (e.g. `commensura.discover/search-units`)."
  ^long [a b]
  (osa-distance (normalize a) (normalize b)))

(defn nearest
  "Up to `limit` (default 3) `candidates` closest to `s`, nearest first, within an
  edit-distance `cutoff`. `candidates` is a seq of names — e.g. `(keys (registry/all-units))`.
  Case- and underscore-insensitive. Returns `[]` when nothing is close, so a caller can
  stay silent on a wild miss. Ties break by shorter, then lexicographic, name — stable and
  deterministic. `cutoff` defaults to a conservative length-relative bound (~one edit per
  three characters); pass an explicit `long` to widen or tighten recall."
  ([s candidates] (nearest s candidates 3))
  ([s candidates limit] (nearest s candidates limit nil))
  ([s candidates limit cutoff]
   (let [target (normalize s)]
     (if (str/blank? target)
       []
       (let [tlen   (count target)
             cutoff (long (or cutoff (max 1 (quot tlen 3))))]
         (->> candidates
              (keep (fn [c]
                      (let [cn (normalize c)]
                        ;; |Δlength| is a lower bound on edit distance — skip the far ones cheaply,
                        ;; then let the cutoff-aware banded DP reject the rest without a full matrix.
                        (when (<= (Math/abs (- tlen (count cn))) cutoff)
                          (let [d (osa-distance-within target cn cutoff)]
                            (when (<= d cutoff) [c d]))))))
              ;; keyfn is a same-length [distance length name] vector; Clojure's `compare`
              ;; orders equal-length vectors element-wise, so this sorts by distance, then
              ;; shorter name, then lexicographically — no explicit comparator needed.
              (sort-by (fn [[c d]] [d (count c) c]))
              (take limit)
              (mapv first)))))))
