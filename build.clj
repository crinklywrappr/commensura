(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]
            [commensura.units.convert :as c-convert]
            [commensura.units.gen :as c-gen]
            [commensura.cpi.fetch :as fetch]
            [commensura.cpi.frink :as frink]
            [commensura.currency.codes :as cur-codes]
            [commensura.currency.gen :as cur-gen]))

(def lib 'com.github.crinklywrappr/commensura)
; alternatively, use MAJOR.MINOR.COMMITS:
(def version (format "1.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")

(defn convert
  "Regenerate resources/commensura/units.edn from dev-resources/frink/units.txt.
  Run with: clojure -X:build convert"
  [opts]
  (c-convert/convert!)
  opts)

(defn gen-units
  "Regenerate src/commensura/units.clj from resources/commensura/units.edn.
  Run with: clojure -X:build gen-units"
  [opts]
  (c-gen/generate!)
  opts)

(defn cpi-fetch
  "Regenerate resources/commensura/cpi.edn from the FRED API (needs FRED_API_KEY + network).
  Run with: FRED_API_KEY=… clojure -X:build cpi-fetch"
  [opts]
  (fetch/fetch!)
  opts)

(defn cpi-frink
  "Regenerate the Frink oracle fixture (test/commensura/frink_cpi.edn) and bootstrap cpi.edn from
  the pinned dev-resources/frink/CPIAUCNS. Run with: clojure -X:build cpi-frink"
  [opts]
  (frink/frink!)
  opts)

(defn currency-codes
  "Regenerate resources/commensura/currency-codes.edn from CurrencyFreaks supported-currencies (no
  key). Run with: clojure -X:build currency-codes"
  [opts]
  (cur-codes/codes!)
  opts)

(defn gen-currency
  "Regenerate src/commensura/currency.clj from currency-codes.edn.
  Run with: clojure -X:build gen-currency"
  [opts]
  (cur-gen/generate!)
  opts)

(defn test "Run all the tests." [opts]
  (let [basis    (b/create-basis {:aliases [:test]})
        cmds     (b/java-command
                  {:basis      basis
                    :main      'clojure.main
                    :main-args ["-m" "cognitect.test-runner"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)

(defn- pom-template [version]
  [[:description "Frink-inspired exact unit conversion and dimensional arithmetic for Clojure."]
   [:url "https://github.com/crinklywrappr/commensura"]
   [:licenses
    [:license
     [:name "GNU General Public License v3.0 or later"]
     [:url "https://www.gnu.org/licenses/gpl-3.0.html"]]]
   [:developers
    [:developer
     [:name "Crinklywrappr"]]]
   [:scm
    [:url "https://github.com/crinklywrappr/commensura"]
    [:connection "scm:git:https://github.com/crinklywrappr/commensura.git"]
    [:developerConnection "scm:git:ssh:git@github.com:crinklywrappr/commensura.git"]
    [:tag (str "v" version)]]])

(defn- jar-opts [opts]
  (assoc opts
          :lib lib   :version version
          :jar-file  (format "target/%s-%s.jar" lib version)
          :basis     (b/create-basis {})
          :class-dir class-dir
          :target    "target"
          :src-dirs  ["src"]
          :pom-data  (pom-template version)))

(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (test opts)
  (b/delete {:path "target"})
  (let [opts (jar-opts opts)]
    (println "\nWriting pom.xml...")
    (b/write-pom opts)
    (println "\nCopying source...")
    (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})
    (println "\nBuilding JAR..." (:jar-file opts))
    (b/jar opts))
  opts)

(defn install "Install the JAR locally." [opts]
  (let [opts (jar-opts opts)]
    (b/install opts))
  opts)

(defn deploy "Deploy the JAR to Clojars." [opts]
  (let [{:keys [jar-file] :as opts} (jar-opts opts)]
    (dd/deploy {:installer :remote :artifact (b/resolve-path jar-file)
                :pom-file (b/pom-path (select-keys opts [:lib :class-dir]))}))
  opts)
