;;;; commensura — Frink-inspired exact unit conversion for Clojure.
;;;; Copyright (C) 2026  crinklywrappr
;;;;
;;;; This program is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.  Distributed WITHOUT ANY WARRANTY; see the GNU General
;;;; Public License <https://www.gnu.org/licenses/> for details.

(ns commensura.units.convert
  "DEV-ONLY converter: reduces Frink's units.txt to EDN.

  Reads dev-resources/frink/units.txt and evaluates each definition down to
  {:factor <exact> :dims {base-dim exponent}}, extracts the ||| dimension-name
  index, special-cases the affine temperature functions, and reports anything it
  cannot reduce. Run with `(convert!)` or `clojure -X:build ...`. The runtime
  never loads this ns — it only reads the generated resources/commensura/units.edn.

  NOTE (license): units.txt is GPL-lineage (adapted from GNU units). The generated
  EDN is a derived work — resolve redistribution before publishing."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [commensura.units.manifest :as manifest]
            [commensura.quantity :as q])
  (:import [ch.obermuhlner.math.big BigDecimalMath]))

;; ---------------------------------------------------------------- numbers
(defn- pow10 [e]
  (if (neg? e) (/ 1 (apply * (repeat (- e) 10N))) (apply * 1N (repeat e 10N))))

(defn- decimal->ratio [s]
  (let [[_ sign whole frac] (re-matches #"([-+]?)0*(\d*)\.(\d+)" s)
        digits (str whole frac)
        num    (bigint (if (str/blank? digits) "0" digits))
        den    (apply * 1N (repeat (count frac) 10N))
        v      (/ num den)]
    (if (= sign "-") (- v) v)))

(defn- normalize-int [^clojure.lang.BigInt n]
  (if (and (<= (bigint Long/MIN_VALUE) n) (<= n (bigint Long/MAX_VALUE))) (long n) n))

(defn- parse-number [s]
  (let [s (-> s str/trim (str/replace "_" ""))]       ; Frink allows 9_192_631_770
    (cond
      (re-matches #"[-+]?\d+\.?" s)        (normalize-int (bigint (str/replace s "." ""))) ; incl. trailing dot: 696342.
      (re-find   #"(?i)e" s)              (let [[m e] (str/split s #"(?i)ee?")]  ; e OR ee exponent
                                            (* (parse-number m) (pow10 (Long/parseLong e))))
      (re-matches #"[-+]?\d*\.\d+" s)      (decimal->ratio s)
      :else (throw (ex-info "bad number" {:s s})))))

(defn- number-token? [t] (and t (re-matches #"[-+]?(?:\d[\d_.]*|\.\d[\d_]*)(?:[eE][eE]?[-+]?\d+)?" t)))

;; ---------------------------------------------------------------- dims
(defn- clean [dims] (into {} (remove (comp zero? val)) dims))
(defn- merge* [a b] (clean (merge-with + a b)))
(defn- negate [d] (into {} (map (fn [[k v]] [k (- v)])) d))

;; ---------------------------------------------------------------- irrational (fractional-exponent) factors
;; A fractional exponent (planckmass = (…)^(1/2), semitone = octave^(1/12)) leaves the
;; dimensions integer but usually makes the *factor* irrational. `commensura.quantity/ratpow`
;; (shared with the runtime) computes it exactly when it's a perfect root, else as a BigDecimal
;; at build precision — its BigDecimal *type* is the "approximate" marker the runtime keys on
;; (BigDecimal factor ⇒ ApproxUnit). We bind `*math-context*` to build precision at the call site.
(def ^:private build-mc java.math.MathContext/DECIMAL128)   ; 34-digit, matches runtime default

(def ^:private one {:factor 1 :dims {}})
(defn- v* [a b] {:factor (* (:factor a) (:factor b)) :dims (merge* (:dims a) (:dims b))})
(defn- vdiv [a b] {:factor (/ (:factor a) (:factor b)) :dims (merge* (:dims a) (negate (:dims b)))})
(defn- vpow [a n]
  (let [dims' (into {} (map (fn [[k e]] [k (* e n)])) (:dims a))]
    (when-not (every? integer? (vals dims'))       ; V/√Hz &c. — rational dims are out of scope
      (throw (ex-info "fractional dimension (out of scope)" {:dims dims' :exp n})))
    ;; a rational exponent can reduce an exponent to BigInt (`(* 2 1/2)` ⇒ 1N); keep
    ;; dims Long-valued like the rest of the file
    {:factor (binding [*math-context* build-mc] (q/ratpow (:factor a) n))
     :dims   (into {} (keep (fn [[k e]] (when-not (zero? e) [k (normalize-int (bigint e))]))) dims')}))

;; ---------------------------------------------------------------- resolve names (+ prefixes + plurals)
(defn- prefix-decompose [env prefixes id]
  (some (fn [[p pv]]
          (when (and (not= p id) (> (count id) (count p)) (str/starts-with? id p))
            (when-let [u (get env (subs id (count p)))]
              {:factor (* pv (:factor u)) :dims (:dims u)})))
        (sort-by (comp - count key) prefixes)))

(defn- resolve-id [env prefixes id]
  (or (get env id)
      (prefix-decompose env prefixes id)
      ;; Frink auto-strips plurals: gallons->gallon, boxes->box, properties->property
      (when (and (> (count id) 1) (str/ends-with? id "s"))
        (or (resolve-id env prefixes (subs id 0 (dec (count id))))
            (when (str/ends-with? id "es") (resolve-id env prefixes (subs id 0 (- (count id) 2))))
            (when (str/ends-with? id "ies")
              (resolve-id env prefixes (str (subs id 0 (- (count id) 3)) "y")))))
      ;; a standalone-capable prefix used alone is its own dimensionless value (survey, kilo)
      (when-let [pv (get prefixes id)] {:factor pv :dims {}})))

;; ---------------------------------------------------------------- tokenizer + parser
;; number (optionally signed) | identifier | operator (incl. unary +/-)
(def ^:private token-re #"[-+]?(?:\d[\d_.]*|\.\d[\d_]*)(?:[eE][eE]?[-+]?\d+)?|[^\s/^()*+-]+|[/^()*+-]")

(defn- tokenize [s] (vec (re-seq token-re s)))

(defn- evaluate
  "Evaluate a Frink unit-expression string to {:factor :dims}, or throw."
  [s env prefixes]
  (let [tokens (tokenize s)
        pos    (atom 0)
        peek*  #(nth tokens @pos nil)
        next*  #(let [t (nth tokens @pos nil)] (swap! pos inc) t)]
    (letfn [(factor* []
              (let [t (next*)]
                (cond
                  (= t "-")          (let [v (factor*)] {:factor (- (:factor v)) :dims (:dims v)})
                  (= t "+")          (factor*)
                  (= t "(")          (let [v (sequence*)] (when (= (peek*) ")") (next*)) v)
                  (number-token? t)  {:factor (parse-number t) :dims {}}
                  :else (or (resolve-id env prefixes t)
                            (throw (ex-info "unresolved identifier" {:id t :expr s}))))))
            (power* []
              (let [base (factor*)]
                (if (= (peek*) "^")
                  (do (next*)
                      (let [ev (factor*)]          ; exponent: a number or a parenthesized (p/q)
                        (when (seq (:dims ev))
                          (throw (ex-info "non-scalar exponent" {:expr s :exp ev})))
                        (vpow base (:factor ev))))
                  base)))
            (sequence* []
              (loop [acc one, op :mul]
                (let [t (peek*)]
                  (cond
                    (or (nil? t) (= t ")")) acc
                    (= t "/") (do (next*) (recur acc :div))
                    (= t "*") (do (next*) (recur acc :mul))
                    :else (let [v (power*)]
                            (recur (if (= op :div) (vdiv acc v) (v* acc v)) :mul))))))]
      ;; bind the build context so a BigDecimal (irrational) factor combining with a
      ;; Ratio (e.g. plancktemperature = planckmass c^2 / k) coerces/rounds instead of
      ;; throwing "non-terminating decimal"; exact Ratio/int arithmetic ignores it.
      (binding [*math-context* build-mc]
        (let [v (sequence*)]
          (when (< @pos (count tokens))
            (throw (ex-info "trailing tokens" {:expr s :rest (subvec tokens @pos)})))
          v)))))

;; ---------------------------------------------------------------- line prep
(defn- unescape-unicode
  "Frink writes unicode names as literal \\uXXXX escape text (e.g. \\u03c3\\u2091);
  decode them to real characters, matching Frink's parser. Safe because \\u
  otherwise appears only inside comments."
  [text]
  (str/replace text #"\\u([0-9a-fA-F]{4})"
               (fn [[_ hex]] (str (char (Integer/parseInt hex 16))))))

(defn- strip-block-comments [text]
  (str/replace text #"(?s)/\*.*?\*/" " "))

(defn- split-comment [line]
  (if-let [i (str/index-of line "//")]
    [(subs line 0 i) (str/trim (subs line (+ i 2)))]
    [line nil]))

(defn- clean-doc [s]
  (some-> s str/trim (str/replace #"\s+" " ") (str/replace #"^\"+|\"+$" "") str/trim not-empty))

;; ---------------------------------------------------------------- driver
(def ^:private affine-temps
  ;; name -> {:scale s :offset o} in kelvin: K = scale*x + offset  (x in degrees)
  {"Fahrenheit" {:scale 5/9 :offset 45967/180}
   "F"          {:scale 5/9 :offset 45967/180}
   "Celsius"    {:scale 1   :offset 5463/20}
   "C"          {:scale 1   :offset 5463/20}
   "Reaumur"    {:scale 5/4 :offset 5463/20}
   "Rankine"    {:scale 5/9 :offset 0}})

(defn- consume-function
  "Given the lines vector `lv` and index `i` of a `Name[args] :=` line, return
  [collected-code-lines next-index]. A brace `{ … }` body is balanced across
  lines; a single-line ternary/alias body is just line i. `//` comments are
  stripped from the collected lines (block comments are already gone)."
  [lv i]
  (let [line0 (first (split-comment (nth lv i)))
        after (str/trim (subs line0 (+ 2 (str/index-of line0 ":="))))]
    (if (and (seq after) (not (str/starts-with? after "{")))
      [[line0] (inc i)]                                   ; single-line ternary / alias
      (loop [j i, depth 0, seen? false, acc []]           ; brace block: balance { }
        (if (>= j (count lv))
          [acc j]
          (let [ln     (first (split-comment (nth lv j)))
                depth' (+ depth (count (filter #{\{} ln)) (- (count (filter #{\}} ln))))
                seen?' (or seen? (str/includes? ln "{"))
                acc'   (conj acc ln)]
            (if (and seen?' (zero? depth'))
              [acc' (inc j)]
              (recur (inc j) depth' seen?' acc'))))))))

(defn parse-units-file [text]
  (let [lines   (str/split-lines (strip-block-comments (unescape-unicode text)))
        base    (atom [])                 ; [dim-keyword ...]
        env     (atom {})                 ; name -> {:factor :dims}
        prefixes (atom {})                ; name -> value
        aliases (atom {})
        dimnames (atom {})                ; dims-map -> name
        nonlin  (atom {})                 ; name -> {:type :affine …} (runtime affine temps)
        functions (atom [])               ; [{:name :body}] every Name[x] := def, for M2.6
        pending (atom [])                 ; [name expr] not yet resolvable
        skipped (atom [])                 ; [line reason]
        docs    (atom {})                 ; name -> docstring (from // comments)
        last-unit (atom nil)]             ; unit whose doc a following comment continues
    ;; pass 1: line-by-line, in source order; a Name[x] := function consumes its
    ;; whole (possibly multi-line brace) body as one unit rather than leaking its
    ;; body lines to the :else "unparsed" bucket.
    (let [lv (vec lines), n (count lv)]
      (loop [i 0]
        (when (< i n)
          (let [raw (nth lv i)
                [code comment] (split-comment raw)
                code (str/trim code)
                doc  (clean-doc comment)]
            (if (re-find #"^[^\s\[]+\s*\[[^\]]*\]\s*:=" code)
              ;; nonlinear function:  Name[args] := <ternary> | { …multi-line body… }
              (let [[fn-lines next-i] (consume-function lv i)
                    nm   (str/trim (re-find #"^[^\s\[]+" code))
                    body (-> (str/join " " fn-lines) (str/replace #"\s+" " ") str/trim)]
                (reset! last-unit nil)
                (when-let [aff (affine-temps nm)]        ; runtime data for the affine temps
                  (swap! nonlin assoc nm (assoc aff :type :affine :dims {:temperature 1})))
                (swap! functions conj {:name nm :body body})  ; catalogue for M2.6
                (recur next-i))
              ;; every other form is a single line
              (do
                (cond
                  ;; comment-only / blank line
                  (str/blank? code)
                  (if (and @last-unit doc)                    ; continuation of a multi-line doc
                    (swap! docs update @last-unit #(str/trim (str % " " doc)))
                    (reset! last-unit nil))

                  ;; base dimension:  dimname =!= sym
                  (str/includes? code "=!=")
                  (let [[dim sym] (map str/trim (str/split code #"=!=" 2))
                        dk (keyword dim), sym (first (str/split sym #"\s+"))]
                    (swap! base conj dk)
                    (swap! env assoc sym {:factor 1 :dims {dk 1}})
                    (reset! last-unit nil)
                    (when doc (swap! docs assoc sym doc) (reset! last-unit sym)))

                  ;; prefix (standalone-capable):  name ::- value
                  (str/includes? code "::-")
                  (let [[nm v] (map str/trim (str/split code #"::-" 2))]
                    (reset! last-unit nil)
                    (try (swap! prefixes assoc nm (:factor (evaluate v @env @prefixes)))
                         (catch Exception _ (swap! pending conj [:prefix nm v]))))

                  ;; dimension name:  expr ||| name
                  (str/includes? code "|||")
                  (let [[lhs nm] (map str/trim (str/split code #"\|\|\|" 2))
                        nm (str/replace nm "_" " ")]      ; human label; brackets allow spaces
                    (reset! last-unit nil)
                    (try (swap! dimnames assoc (:dims (evaluate lhs @env @prefixes)) nm)
                         ;; LHS unit may be defined later in the file — retry in the multi-pass loop
                         (catch Exception _ (swap! pending conj [:dimname nm lhs]))))

                  ;; prefix alias:  sym :- prefixname   (also unit alias handled by :=)
                  (re-find #"^\S+\s*:-\s" code)
                  (let [[nm v] (map str/trim (str/split code #":-" 2))]
                    (reset! last-unit nil)
                    (swap! aliases assoc nm v))

                  ;; derived unit:  name := expr
                  (str/includes? code ":=")
                  (let [[nm expr] (map str/trim (str/split code #":=" 2))]
                    (reset! last-unit nil)
                    (if (or (str/includes? expr "<<") (str/blank? expr))
                      (swap! skipped conj [nm "special-value"])
                      (do (try (swap! env assoc nm (evaluate expr @env @prefixes))
                               (catch Exception _ (swap! pending conj [:unit nm expr])))
                          ;; enable continuation only when the def itself carried a comment
                          (when doc (swap! docs assoc nm doc) (reset! last-unit nm)))))

                  :else (do (reset! last-unit nil) (swap! skipped conj [code "unparsed"])))
                (recur (inc i))))))))

    ;; prefix-name aliases resolve immediately (k :- kilo)
    (doseq [[nm v] @aliases]
      (when-let [pv (get @prefixes v)] (swap! prefixes assoc nm pv)))

    (letfn [(resolve-pending! []                    ; retry pending defs until stable
              (loop [pass 0]
                (let [todo @pending]
                  (reset! pending [])
                  (doseq [[typ nm expr] todo]
                    (try (let [v (evaluate expr @env @prefixes)]
                           (case typ
                             :prefix  (swap! prefixes assoc nm (:factor v))
                             :dimname (swap! dimnames assoc (:dims v) nm)
                             (swap! env assoc nm v)))    ; :unit
                         (catch Exception _ (swap! pending conj [typ nm expr]))))
                  (when (and (seq @pending) (< (count @pending) (count todo)) (< pass 12))
                    (recur (inc pass))))))]
      (resolve-pending!)
      ;; expression-valued `:-` prefixes (british :- 1200000/3937014 m/ft): resolve only
      ;; now that their unit deps (ft, m) are defined — otherwise `ft` resolves to a
      ;; spurious femto·tonne prefix-decomposition instead of the foot unit.
      (doseq [[nm v] @aliases]
        (when-not (contains? @prefixes nm)
          (try (swap! prefixes assoc nm (:factor (evaluate v @env @prefixes)))
               (catch Exception _ nil))))
      (resolve-pending!))                           ; units depending on those prefixes (britishinch-*)

    (doseq [[_ nm expr] @pending] (swap! skipped conj [nm (str "unresolved: " expr)]))

    {:base-dimensions (set @base)
     :prefixes @prefixes
     :prefix-aliases @aliases
     :units (into {} (map (fn [[nm u]] [nm (if-let [d (@docs nm)] (assoc u :doc d) u)])) @env)
     :dimension-names @dimnames
     :nonlinear @nonlin
     :functions @functions
     :skipped @skipped}))

(defn convert!
  "Read units.txt, reduce it, write units.edn, print a coverage report."
  ([] (convert! "dev-resources/frink/units.txt" "resources/commensura/units.edn"))
  ([in out]
   (let [{:keys [units skipped dimension-names base-dimensions] :as result}
         (parse-units-file (slurp in))]
     (io/make-parents out)
     (spit out (with-out-str (pp/pprint (dissoc result :skipped))))
     (println (format "units: %d   dimension-names: %d   base-dims: %d   skipped: %d"
                      (count units) (count dimension-names) (count base-dimensions) (count skipped)))
     (let [names (map :name (:functions result))
           cov   (manifest/classify names)]
       (println (format "functions: %d   (implemented %d, affine %d, deferred %d, unhandled %d)"
                        (count (set names))
                        (count (:implemented cov)) (count (:affine cov))
                        (count (:deferred cov)) (count (:unhandled cov))))
       (when (seq (:unhandled cov))
         (println "  unhandled (classify in commensura.units.manifest):"
                  (str/join ", " (sort (:unhandled cov))))))
     (println "wrote" out)
     result)))

(defn -main [& _] (convert!))
