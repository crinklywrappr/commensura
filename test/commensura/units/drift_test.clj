(ns commensura.units.drift-test
  "M2.7 — guards our hand-written translations of Frink's nonlinear `Name[x] :=`
  functions against upstream drift. Each translation pins the SHA-256 of the units.txt
  body it was written against (on the impl var, or in `commensura.units.manifest` for
  the affine temps); these tests re-derive those SHAs from the current units.txt and
  fail if any diverges, or if a function appears/disappears unaccounted-for."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [commensura.units.convert :as convert]
            [commensura.units.manifest :as manifest]
            [commensura.richter]))                 ; load so its vars + :frink/sha exist

(def ^:private catalogue
  ;; Frink function name -> the distinct normalized bodies the converter catalogued.
  (delay (->> (:functions (convert/parse-units-file (slurp "dev-resources/frink/units.txt")))
              (group-by :name)
              (reduce-kv (fn [m nm es] (assoc m nm (distinct (map :body es)))) {}))))

(defn- current-sha
  "SHA of the single catalogued body for `nm`, or nil if it isn't uniquely defined."
  [nm]
  (let [bodies (@catalogue nm)]
    (when (= 1 (count bodies))
      (manifest/sha256 (first bodies)))))

(deftest implemented-functions-match-units-txt
  (doseq [[nm {:keys [status vars]}] manifest/functions
          :when (= status :implemented)]
    (testing nm
      (let [current (current-sha nm)]
        (is (some? current) (str nm ": expected exactly one catalogued body"))
        (doseq [vsym vars]
          (let [m (meta (requiring-resolve vsym))]
            (is (= nm (:frink/fn m)) (str vsym " should declare :frink/fn " (pr-str nm)))
            (is (= current (:frink/sha m))
                (str vsym ": units.txt body for " nm " changed — re-verify the translation"
                     " and update :frink/sha (pinned " (:frink/sha m) ", now " current ")"))))))))

(deftest affine-functions-match-units-txt
  (doseq [[nm {:keys [status sha]}] manifest/functions
          :when (= status :affine)]
    (testing nm
      (let [current (current-sha nm)]
        (is (= current sha)
            (str nm ": affine-temperature body changed in units.txt — re-verify"
                 " convert/affine-temps and update the manifest :sha"
                 " (pinned " sha ", now " current ")"))))))

(deftest every-catalogued-function-is-classified
  (let [catalogued (set (keys @catalogue))
        unhandled  (:unhandled (manifest/classify catalogued))
        stale      (set/difference (set (keys manifest/functions)) catalogued)]
    (is (empty? unhandled)
        (str "unhandled Frink Name[x] := function(s) — classify in"
             " commensura.units.manifest: " unhandled))
    (is (empty? stale)
        (str "manifest classifies function(s) no longer in units.txt: " stale))))
