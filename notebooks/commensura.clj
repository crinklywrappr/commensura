;; # commensura — exact units & dimensional arithmetic for Clojure
;;
;; **commensura** is a [Frink](https://frinklang.org)-inspired library for unit conversion and
;; dimensional arithmetic that stays **exact**. It rides Clojure's native numeric tower
;; (`Ratio`/`BigInt`/`BigDecimal`), so an answer is a precise fraction — never a lossy float — unless
;; the value is genuinely irrational, in which case it becomes an explicit arbitrary-precision
;; approximation. Every unit is a plain, autocompletable var: `(u/feet 10)` is ten feet.
;;
;; This page is a living document — each result below is computed by commensura as the page is built.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns commensura
  (:require [nextjournal.clerk :as clerk]
            [commensura.core :refer :all]
            [commensura.units :as u]
            [commensura.quantity :as q]
            [commensura.interval :as iv]
            [commensura.uncertain :as un]
            [commensura.math :as m]
            [commensura.cpi :as cpi]
            [commensura.currency :as cur]
            [commensura.currency.rates :as rates]
            [commensura.registry :as registry]
            [commensura.discover :as discover]
            [commensura.reader]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

;; By default commensura values render as their tagless display string; a few early cells opt into the
;; full tagged-literal form (`pr-str`) via `tagged-viewer`, so the printed/serialized shape is visible.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(def tagged-viewer
  {:transform-fn (comp clerk/mark-presented (clerk/update-val pr-str))
   :render-fn '(fn [s] [:span {:style {:color "#047857" :font-family "monospace" :font-weight 500}} s])})

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(clerk/add-viewers!
 [{:name :commensura/display
   :pred q/displayable?
   :transform-fn (comp clerk/mark-presented (clerk/update-val str))
   :render-fn '(fn [s] [:span {:style {:color "#047857" :font-family "monospace" :font-weight 500}} s])}
  {:name :commensura/interval
   :pred iv/interval?
   :transform-fn (comp clerk/mark-presented
                       (clerk/update-val
                        (fn [x] (str "[" (str (iv/lo x)) "  …  " (str (iv/hi x)) "]"))))
   :render-fn '(fn [s] [:span {:style {:color "#7c3aed" :font-family "monospace"}} s])}
  {:name :commensura/uncertain
   :pred un/uncertain?
   :transform-fn (comp clerk/mark-presented (clerk/update-val str))   ; "value ± sigma [dimension]"
   :render-fn '(fn [s] [:span {:style {:color "#7c3aed" :font-family "monospace"}} s])}])

;; `demo` runs a *live* example (currency, satoshi) but shows the reader only the bare form and its
;; result. The try/catch the static build needs — Clerk aborts on any uncaught throw, e.g. when there's
;; no `CURRENCYFREAKS_API_KEY` — stays hidden (use it with the cell's own code hidden).
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defmacro demo [form]
  `(clerk/fragment
     (clerk/code '~form)
     (try ~form
          (catch Throwable _#
            (clerk/html [:em {:style {:color "#9ca3af"}} "— live result; set CURRENCYFREAKS_API_KEY to see it —"])))))

;; ## Hm, I wonder what a unit looks like?
;;
;; A unit is just a value. commensura prints its values as **tagged literals** — a `#commensura/unit`
;; or `#commensura/quantity` form that reads back through the data reader. A bare unit var is a
;; quantity of one:

^{:nextjournal.clerk/viewer tagged-viewer} u/foot

;; Call a unit to scale it — now it is a quantity:

^{:nextjournal.clerk/viewer tagged-viewer} (u/foot 10)

;; Convert with `to`. Because a bare unit is already a quantity of one, you rarely need `(u/mile 1)`:

^{:nextjournal.clerk/viewer tagged-viewer} (to u/mile u/foot)

;; Products and quotients compose with `by` (×), `per` (÷), `plus`, `minus`, `pow` (all from
;; `commensura.core`); bare numbers are dimensionless scalars. Miles per hour into metres per second,
;; exactly:

^{:nextjournal.clerk/viewer tagged-viewer} (to (per u/mile u/hour) (per u/meter u/second))

;; Those tagged literals round-trip through the reader. For readability, the rest of this page shows
;; commensura values in their cleaner **display form** — the same text, without the wrapping tag.

;; One flourish: applying a unit to **several** arguments elides the `by` — each argument scales the
;; unit and the results multiply, so *n* arguments raise the dimension to the *n*th power:

(eq? (u/foot 3 5 7) (by (u/foot 3) (u/foot 5) (u/foot 7)))

;; So `(u/foot 3 5 7)` is a volume — `105 foot³`:

(u/foot 3 5 7)

;; ## Exact, where floating point falls short
;;
;; How fast is light, in **furlongs per fortnight**? commensura knows both units and answers with a
;; whole exact rational — not a rounded float:

(to u/c (per u/furlong u/fortnight))

;; The headline is fidelity. Everest + 15 cubits of biblical floodwater deposited over 40 days gives a
;; rainfall rate that commensura reports **exactly** as `113029/320 inch/hour` — where ordinary
;; floating-point arithmetic can only manage the lossy `353.21562499999993`:

^{:nextjournal.clerk/visibility {:code :show}}
(defunit flood-depth (plus (u/feet 29030) (u/biblicalcubits 15)
                           (by (per (u/inches -24/10) u/year) (u/years 4000))))

(to (per flood-depth (u/days 40)) (per u/inch u/hour))   ; 113029/320 inch/hour — exact

;; When a result is genuinely irrational (through `π`, a root, …) commensura says so, carrying an
;; arbitrary-precision approximation instead of pretending it is a fraction:

(m/sqrt (u/meter 2))

;; ## Range types — when a number isn't a single point
;;
;; Sometimes a measurement isn't one number. commensura has two *range* values, and both ride the same
;; verbs (`by`/`per`/`plus`/`minus`/`pow`/`to`) as an ordinary quantity:
;;
;; * an **interval** — a guaranteed span `[lo, hi]`; *every* value in between is possible, and
;;   arithmetic returns the tightest range containing every outcome (Frink's interval arithmetic);
;; * an **uncertain** — a best estimate with a **± 1σ** spread that propagates in quadrature.
;;
;; They answer different questions — a hard range vs. a statistical error bar — but you compute with
;; them the same way. (Mixing the two in one operation is, deliberately, an error.)
;;
;; ### Intervals — the honest span
;;
;; Real-world numbers have slop, and interval arithmetic propagates it **rigorously**: the true result
;; is guaranteed to lie inside the computed range. Because commensura's bounds are exact rationals, that
;; guarantee is airtight — no floating-point rounding can ever let the truth slip outside.
;;
;; A 2,800-mile road trip. Your car gets somewhere between **28 and 32 mpg**; gas runs **\$3⁴⁰–\$3⁹⁰** a
;; gallon. What will the fuel actually cost — accounting for *every* combination of the two at once?

^{:nextjournal.clerk/visibility {:result :hide}}
(def mpg (iv/interval (per (u/miles 28) u/gallon) (per (u/miles 32) u/gallon)))

^{:nextjournal.clerk/visibility {:result :hide}}
(def gas (iv/interval (per (u/dollars 34/10) u/gallon) (per (u/dollars 39/10) u/gallon)))

;; Fuel burned is distance ÷ mpg; the bill is that times the price. The whole guaranteed range:

^{:nextjournal.clerk/visibility {:result :hide}}
(def fuel-cost (to (by (per (u/miles 2800) mpg) gas) u/dollars))

fuel-cost

;; Somewhere between **\$297⁵⁰ and \$390** — the honest span, not one misleading point estimate. So: is
;; **\$400 certainly enough** to cover gas?

(certainly-lt? fuel-cost (u/dollars 400))

;; ### Uncertainties — a best estimate ± error
;;
;; A tape measure gives a value with a **1σ** spread, not a hard range. `plus-minus` pairs the two; the
;; spread then propagates through the arithmetic — in **quadrature** for `×`/`÷`, so it's the *relative*
;; errors that combine. The centre stays exact; only σ goes approximate (a √ is irrational).

^{:nextjournal.clerk/visibility {:result :hide}}
(def len (un/plus-minus (u/meter 120/100) (u/cm 2)))    ; 1.20 m ± 2 cm

^{:nextjournal.clerk/visibility {:result :hide}}
(def wid (un/plus-minus (u/meter 80/100) (u/cm 1)))     ; 0.80 m ± 1 cm

;; The tabletop's area comes back carrying its own error bar, computed for you:

(def top (by len wid))

;; …and its *relative* uncertainty is one call away:

(un/relative top)

;; Two independent measurements of the **same** board — do they agree? `consistent?` asks "within 2σ?"
;; and `within?` takes any threshold — the statistical cousins of the interval `certainly?`/`possibly?`.

^{:nextjournal.clerk/visibility {:result :hide}}
(def board-a (un/plus-minus (u/meter 120/100) (u/cm 2)))

^{:nextjournal.clerk/visibility {:result :hide}}
(def board-b (un/plus-minus (u/meter 121/100) (u/cm 1)))

[(un/consistent? board-a board-b) (un/within? board-a board-b 1)]

;; ### How much, and how many? — `span` and `steps`
;;
;; Two verbs read a range — an interval's `[lo, hi]`, or an uncertain's `[value−σ, value+σ]` — and
;; answer in terms of a unit. `span` collapses it to a single **dimensioned width**; `steps` walks it
;; **unit-by-unit** (half-open `[lo, hi)`, like `range`) and returns an *eduction*, so it drops straight
;; into `into` and transducers.

;; How wide was that fuel estimate, in dollars?

(span fuel-cost u/dollar)

;; The full ±2σ band of a measurement, as one length:

(span board-a u/cm)

;; A 6–11 m doorway clearance — how many whole feet is that, marked off one at a time?

(span (iv/interval (u/meter 6) (u/meter 11)) u/foot)

(into [] (map m/round) (steps (iv/interval (u/meter 6) (u/meter 11)) u/foot))

;; ## Comparisons
;;
;; Quantities compare **physically** (unit-agnostic), and intervals get Frink's *certainly* / *possibly*
;; operators plus plain relationals that throw on a genuine overlap. (An uncertain compares on its
;; central value; for a σ-aware test use `within?` / `consistent?` above.)

(eq? (u/foot 1) (u/inch 12))                            ; 1 ft = 12 in

[(possibly-lt? (iv/interval 1 3) (iv/interval 2 4))     ; overlapping ⇒ possibly, but not certainly
 (certainly-lt? (iv/interval 1 3) (iv/interval 2 4))]

;; ## Historical purchasing power (US CPI)
;;
;; commensura ships every published month of the BLS **CPI-U** series (1913→present) as exact rationals,
;; so `dollar_YYYY` units inflation-adjust with no lossy money type. What is \$100 from 1913 worth today?

(to (by 100 (cpi/usd 1913)) u/dollar)

;; The CPI-U index itself, over more than a century:
^{:nextjournal.clerk/visibility {:code :hide}}
(def cpi-annual
  (->> (:annual (edn/read-string (slurp (io/resource "commensura/cpi.edn"))))
       (sort-by key)
       (mapv (fn [[y v]] {:year y :cpi (double v)}))))

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/vl
 {:width 680 :height 320
  :title "US CPI-U (not seasonally adjusted), annual average, 1913–present"
  :data {:values cpi-annual}
  :mark {:type "area" :line {:color "#047857"} :color {:x1 1 :y1 1 :x2 1 :y2 0 :gradient "linear"
                                                       :stops [{:offset 0 :color "white"}
                                                               {:offset 1 :color "#a7f3d0"}]}}
  :encoding {:x {:field "year" :type "quantitative" :axis {:format "d" :title "year"}}
             :y {:field "cpi"  :type "quantitative" :title "CPI-U (1982-84 = 100)"}}})

;; ## Live currency (opt-in)
;;
;; Currency conversion is **live** — commensura ships the client (CurrencyFreaks), never the rates. Each
;; ISO code is a discoverable fn, e.g. `(cur/EUR 600)`, plus the four precious metals per troy ounce.
;; This section renders live only with a `CURRENCYFREAKS_API_KEY`; the static build degrades gracefully.

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(def currency-snapshot
  (try
    (when (System/getenv "CURRENCYFREAKS_API_KEY")
      (vec (for [code ["EUR" "GBP" "JPY" "CAD" "XAU"]]
             {"code" code "USD value" (format "$%.4f" (double (q/magnitude (rates/unit code))))})))
    (catch Throwable _ nil)))

^{:nextjournal.clerk/visibility {:code :hide}}
(if currency-snapshot
  (clerk/table currency-snapshot)
  (clerk/md "> _No `CURRENCYFREAKS_API_KEY` set — run `clojure -X:build docs` locally with a key to see a live snapshot. The API and units are exercised offline in the test suite._"))

;; Each ISO code is a discoverable fn — `(cur/EUR 600)` is 600 euros, valued live in USD:

^{:nextjournal.clerk/visibility {:code :hide}}
(demo (cur/EUR 600))

;; Conversions compose as usual. 600 Thai baht into dollars:

^{:nextjournal.clerk/visibility {:code :hide}}
(demo (to (cur/THB 600) u/dollar))

;; …and the price of gold, per kilogram:

^{:nextjournal.clerk/visibility {:code :hide}}
(demo (to (per (cur/XAU) u/troyounce) (per u/dollar u/kg)))

;; Need to hand a value to the wider money ecosystem? **`quantity->money`** bridges a currency quantity
;; to a **Joda-Money** `Money` (via clojurewerkz/money), rounding to the currency's decimal places — the
;; one verb that deliberately *leaves* the exact tower. USD is the reference rate, so this needs no key;
;; note the sub-cent amount rounds (HALF_EVEN by default, or pass a `RoundingMode`):

(quantity->money (cur/USD 19.995))

;; …and **`money->quantity`** comes back the other way, re-entering the exact tower — so a `Money` can be
;; scaled, converted, and compared with everything else here (the amount is already at the currency's
;; scale, so nothing is lost):

(money->quantity (quantity->money (cur/USD 19.99)))

;; ## Extending commensura
;;
;; **`defunit`** mints your own unit as a callable var — indistinguishable from the built-ins. Define it
;; once from any expression, then use it bare, applied, or as a `to` target. Banana for scale:

(defunit banana (u/cm 18))

(to (u/meter 1) banana)          ; how many bananas tall is a metre?

(by 3 banana)                    ; …and three bananas is a length like any other

;; **`register-dimension!`** teaches commensura a name for a whole *dimension*. Miles-per-gallon is a
;; length⁻² (distance per volume) — a callback to the road trip above. Name it once, and every quantity
;; of that shape prints the friendly label:

(register-dimension! {:length -2} "fuel economy")

(per (u/miles 30) u/gallon)

;; `defunit` **snapshots** its relationship at definition time — ideal for a fixed peg like `banana`. A
;; `register-unit-resolver!` covers the two cases `defunit` can't: a name-derived *family* (one `pred`
;; matching a whole shape — e.g. the `dollar_YYYY` CPI units), and a single *moving* rate. For the
;; moving case, pair a plain `defn` that returns a freshly-minted **unit** with the resolver, so the
;; name also reifies from a literal. A **satoshi** (1e-8 BTC) is the ideal demo — defining and
;; registering it are cheap; only *calling* it touches the live BTC price:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn satoshi                                    ; a freshly-minted unit at the live BTC price
  ([]  (q/unit "satoshi" (q/magnitude (by 1/100000000 (cur/BTC))) {:currency 1}))
  ([n] (by n (satoshi))))

^{:nextjournal.clerk/visibility {:result :hide}}
(register-unit-resolver! #(= % "satoshi") (fn [unit-name] (satoshi)))   ; so a literal reifies too

;; Now use it live — 250,000 satoshi, valued in USD:

^{:nextjournal.clerk/visibility {:code :hide}}
(demo (satoshi 250000))

;; …and one bitcoin back into satoshi. Because everything is exact, `1e8` satoshi reconstitute
;; *exactly* one bitcoin — zero rounding drift, at any price:

^{:nextjournal.clerk/visibility {:code :hide}}
(demo (to (cur/BTC 1) (satoshi)))

;; The other case is a **family**: one resolver mints *many* units by parsing their names — you never
;; define each member (this is exactly how the `dollar_YYYY` CPI units work). Say a lumber yard sells
;; planks by length, so `plank_<n>` should be an *n*-foot unit. A single pred/dispatch pair covers every
;; one of them — the `pred` matches the shape, the `dispatch` parses `n` out of the name:

^{:nextjournal.clerk/visibility {:result :hide}}
(register-unit-resolver!
 (fn [nm] (re-matches #"plank_\d+" nm))                       ; pred: any plank_<n>
 (fn [nm] (q/unit nm                                          ; dispatch: build an n-foot unit
                  (q/magnitude (u/feet (parse-long (re-find #"\d+" nm))))
                  {:length 1})))

;; `plank_12` was never defined, yet the name resolves to a real unit on demand — and it's exact
;; (4 yards is exactly one 12-foot plank):

(to (u/yards 4) (registry/resolve-unit "plank_12"))

;; ## Discovering what's available — right in the middle of a calculation
;;
;; `discover` earns its keep less as a catalog than as something you reach for *mid-calculation*: you're
;; partway through, holding a value — what units could it wear? what would conform if you kept going?
;; **`units-of-dimension`** takes the value *itself*, so you needn't even name the dimension. Say you've
;; worked out a cruising speed:

(def cruising (per (u/miles 70) u/hour))

(discover/units-of-dimension cruising)

;; There's the whole vocabulary of *how fast* — so pick one and finish the thought:

(to cruising u/mach)

;; It reads **intervals** too — which is exactly what you tend to be holding mid-calculation. The road
;; trip's fuzzy `fuel-cost` is a currency, so ask what money conforms with it; it checks that every
;; endpoint agrees on the dimension (a constructor-built interval always does):

(discover/units-of-dimension fuel-cost)

;; The other two tools *find* and *inspect*. **`search-units`** matches a name — case-insensitive
;; substring or regex, ranked so the exact hit leads:

(discover/search-units "volt")

;; **`describe`** is the inspector: display string, dimensions, the human dimension name, the defining
;; namespace, and the docstring — each dropped when it doesn't apply. It reads the *live* registry, so
;; the `banana` you defined above is already discoverable, and `describe` reports it was minted right
;; here in this notebook:

(discover/describe "banana")

;; And because everything is plain **data**, a search flows straight into full descriptions — data in,
;; data out:

(map discover/describe (discover/search-units "volt"))

;; **One caveat — on-demand units.** `search-units` and `units-of-dimension` see only the *registered*
;; table: the builtins and your own `defunit`s. Units that exist only through a **resolver** are minted
;; the instant you name them and have no list to enumerate, so they never surface in a search or a
;; dimension roundup — the `dollar_YYYY` inflation units, every live currency code (`EUR`, `AAVE`, …),
;; and any `register-unit-resolver!` family (the `satoshi` and `plank_<n>` above). That's why the money
;; roundup listed a handful of slang units, not thousands of codes — and why searching for one is empty:

(discover/search-units "dollar_1960")

;; `describe` is the exception: it resolves by *exact* name, so it does reach a resolver-minted unit —
;; you just get `:value` and `:dimensions` and nothing more, since it wasn't made with `defunit` (no
;; `:namespace`, no `:doc`). And a *misspelled* resolver name gets no "did you mean?", because the
;; suggester only knows the registered names:

(discover/describe "dollar_1960")

;; So: reach for `search-units` / `units-of-dimension` to explore the registered world, and exact naming
;; (or the defining `defn` / resolver) to summon the on-demand one.

;; ## Under the hood — everything reduces to base dimensions
;;
;; A quantity is a magnitude plus a map of base-dimension exponents. Conversion is just: check the
;; dimension maps match, divide the factors. Here is that reduction for a handful of compound units.

^{:nextjournal.clerk/visibility {:code :hide}}
(clerk/table
 {:head ["unit / expression" "value (base SI)" "dimensions"]
  :rows (for [[label x] [["mile/hour"        (per u/mile u/hour)]
                         ["newton"           u/newton]
                         ["watt"             u/watt]
                         ["acre"             u/acre]
                         ["√(hertz)"         (m/sqrt (u/hertz 1))]
                         ["knot"             u/knot]]]
          [label (str x) (pr-str (q/dims x))])})

;; ---
;; _Built with [Clerk](https://github.com/nextjournal/clerk). commensura is GPL-3.0-or-later; its unit
;; data derives from Frink's `units.txt` (GNU units → Bell Labs units lineage)._
