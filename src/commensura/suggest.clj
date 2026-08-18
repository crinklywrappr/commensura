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
                        ;; |Δlength| is a lower bound on edit distance — skip the far ones cheaply.
                        (when (<= (Math/abs (- tlen (count cn))) cutoff)
                          (let [d (osa-distance target cn)]
                            (when (<= d cutoff) [c d]))))))
              ;; keyfn is a same-length [distance length name] vector; Clojure's `compare`
              ;; orders equal-length vectors element-wise, so this sorts by distance, then
              ;; shorter name, then lexicographically — no explicit comparator needed.
              (sort-by (fn [[c d]] [d (count c) c]))
              (take limit)
              (mapv first)))))))
