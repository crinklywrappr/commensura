(ns commensura.oracle
  "Test-support: drive the real Frink engine in-process as an oracle. Frink is proprietary and NOT
  redistributed — it is provided via the opt-in `:frink` alias (see deps.edn). This namespace loads
  fine WITHOUT the jar (it resolves `frink.parser.Frink` reflectively), so the default test suite is
  unaffected; callers guard with `(when (available?) …)`.

  `frink.parser.Frink` exposes `parseString(String) → String`. Frink prints exact values as an integer
  or a `p/q` fraction and only falls to a decimal for irrationals, e.g.
    \"10 feet -> meters\"  => \"381/125 (exactly 3.048)\"
    \"2 meter + 3 meter\"  => \"5 m (length)\"
    \"sqrt[2] meter\"      => \"1.4142135623730951 m (length)\"
  so the leading token is the source of truth: a rational ⇒ exact, a bare decimal ⇒ irrational/approx."
  (:require [clojure.string :as str]
            [commensura.registry :as registry]
            [commensura.quantity :as q]
            [commensura.units]))                 ; load so builtin units are registered

;; ---- reflective access (no :import, so the ns loads without the jar) ----
(def ^:private frink-class
  (delay (try (Class/forName "frink.parser.Frink") (catch Throwable _ nil))))

(defn available? [] (some? @frink-class))

(def ^:private engine
  (delay (clojure.lang.Reflector/invokeConstructor @frink-class (object-array 0))))

(defn eval-raw
  "Evaluate a Frink expression, returning Frink's raw result string."
  [s]
  (clojure.lang.Reflector/invokeInstanceMethod @engine "parseString" (object-array [s])))

;; ---- result parsing ----
(defn- parse-token [tok]
  (cond
    (re-matches #"[-+]?\d+/\d+" tok)  {:rational (read-string tok)}
    (re-matches #"[-+]?\d+" tok)      {:rational (bigint tok)}
    (re-matches #"[-+]?(?:\d*\.\d+|\d+\.?\d*)(?:[eE][-+]?\d+)?" tok)
    {:approx (Double/parseDouble tok)}
    :else nil))

(defn frink-eval
  "Evaluate `s` in Frink and parse the leading value:
   {:rational r :approx d} when Frink returns an exact integer/fraction,
   {:approx d} when it returns a bare decimal (irrational),
   {:error …} on a parse/eval failure or an unrecognized shape."
  [s]
  (try
    (let [raw (str/trim (eval-raw s))
          tok (first (str/split raw #"\s+"))
          m   (parse-token tok)]
      (cond
        (nil? m)      {:error raw :raw raw}
        (:rational m) (assoc m :approx (double (:rational m)) :raw raw)
        :else         (assoc m :raw raw)))
    (catch Throwable t {:error (.getMessage t)})))

;; ---- the empirical alignment filter (also a mini-oracle) ----
(defn- exact? [x] (or (integer? x) (ratio? x)))

(defn frink-base-factor
  "Frink's exact base-SI factor for a single unit `nm` (its magnitude reduced to base units), or nil
  when Frink doesn't know the name or the factor is irrational."
  [nm]
  (:rational (frink-eval (str "1 " nm))))

(defn aligned-unit-names
  "Commensura unit names whose exact base-SI magnitude Frink reproduces EXACTLY — the vetted pool the
  generators draw from. This intersection self-maintains as units.txt evolves."
  []
  (->> (registry/all-units)
       (keep (fn [[nm unit]]
               (let [m (q/magnitude unit)]
                 (when (and (exact? m) (= m (frink-base-factor nm))) nm))))
       sort))
