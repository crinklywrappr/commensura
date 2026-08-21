;; # commensura, in the frinj style
;;
;; This notebook re-runs the classic **frinj** sample calculations on commensura's exact engine, through
;; the optional [`commensura.infix`](https://github.com/crinklywrappr/commensura) layer.
;;
;; The examples — and the delightful *keyword-soup* notation they use — come straight from two people who
;; deserve all the credit:
;;
;; * **Alan Eliasen**, author of [Frink](https://frinklang.org) and its wonderful
;;   [Sample Calculations](http://futureboy.us/frinkdocs/#SampleCalculations) page, from which every
;;   calculation below is taken.
;; * **Martin Trojer**, author of [frinj](https://github.com/martintrojer/frinj), the Clojure port whose
;;   `fj` / `$=` notation this layer emulates.
;;
;; commensura's twist: it never leaves the exact numeric tower and it *keeps the unit stack*, so a
;; conversion stays a dimensioned quantity (`552960/77 gallon ≈ 7181.30 [volume]`) instead of collapsing
;; to a bare number. No `str` calls, no `frinj-init!` — just values that render themselves.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns frinj-examples
  (:require [nextjournal.clerk :as clerk]
            [commensura.infix :refer [fj $= to]]
            [commensura.core :refer [defunit]]
            [commensura.quantity :as q]))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(clerk/add-viewers!
 [{:name :commensura/display
   :pred q/displayable?
   :transform-fn (comp clerk/mark-presented (clerk/update-val str))
   :render-fn '(fn [s] [:span {:style {:color "#047857" :font-family "monospace" :font-weight 500}} s])}])

;; ## Mass and Volume
;;
;; You want to fill your bedroom — 10 ft × 12 ft × 8 ft — with water. How much water is that? Numbers and
;; `:unit` keywords just sit next to each other; `:to` converts the running product. commensura keeps the
;; `[volume]` dimension and the exact fraction that frinj prints as an approximation:

(fj 10 :feet 12 :feet 8 :feet :to :gallons)

;; What would that weigh? `:water` is the density of water, so multiplying it in turns volume into mass:

(fj 10 :feet 12 :feet 8 :feet :water :to :pounds)

;; Almost 60,000 lb. If the floor holds only 2 tons, how deep a pool can you risk? `$=` is infix math
;; over the values — here a division — and `to` re-expresses the result in feet:

(to ($= (fj 2 :tons) / (fj 10 :feet 12 :feet :water)) :feet)

;; About half a foot. A sad pool party.

;; ## Liquor
;;
;; How much denser is water than alcohol? In frinj `(fj :water :per :alcohol)` collapses to `1.2669`;
;; commensura keeps the stack, so the *density ratio* is clearest as a conversion — one water is 1.2669
;; alcohols, exactly 10000/7893:

(fj :water :to :alcohol)

;; 3.2 beer is measured by weight, so a beer's worth of alcohol-by-volume is 12 floz × 3.2% ×
;; water/alcohol. `defunit` (from `commensura.core`) registers it like any commensura unit, so later
;; soups name it by keyword — no separate registration path:

^{:nextjournal.clerk/visibility {:result :hide}}
(defunit beer (fj 12 :floz 3.2 :percent :water :per :alcohol))

;; How many beers is a champagne magnum (at 13.5%)?

(fj :magnum 13.5 :percent :to :beer)

;; Now some jungle juice: a 1.75 L bottle of 190-proof Everclear in a 5-gallon bucket. What proof is the
;; result, and how many beers is a 5-cup (12 floz each) serving?

^{:nextjournal.clerk/visibility {:result :hide}}
(defunit junglejuice ($= (fj 1.75 :liter 190 :proof) / (fj 5 :gallon)))

(fj :junglejuice :to :percent)

(fj 5 12 :floz :junglejuice :to :beer)

;; ## More Liquor
;;
;; How many cases in a keg (a half beer-barrel)? And how many 12-floz cans? A **number-led** `to` target
;; — `(to (fj :keg) 12 :floz)` — asks "how many of *this* quantity", so it returns the bare count:

(fj :keg :to :case)

(to (fj :keg) 12 :floz)

;; Price of alcohol by the fluid ounce, buying a $60 keg of 3.2 beer (corrected weight→volume):

(to ($= (fj 60 :dollars) / (fj :keg 3.2 :percent :water :per :alcohol)) :dollars :per :floz)

;; A cheap bottle of wine, and a big plastic bottle of bad vodka:

(to ($= (fj 6.99 :dollars) / (fj :winebottle 13 :percent)) :dollars :per :floz)

(to ($= (fj 13.99 :dollars) / (fj 1750 :ml 80 :proof)) :dollars :per :floz)

;; ## Movie magic
;;
;; In *Independence Day*, the alien mother ship is 500 km across with a mass ¼ that of the Moon. As a
;; sphere (volume `4/3 π r³`), how dense is it, in multiples of water? `**` is exponent, binding tighter
;; than `*` and `/`:

(to ($= (fj 1/4 :moonmass) / ($= (fj 4/3 :pi) * (fj 500/2 :km) ** 3)) :water)

;; 280× denser than water — denser than any known element. And its surface gravity (`G · mass / r²`,
;; with Frink's gravitational constant `:G`), in earth gravities:

(to ($= (fj :G 1/4 :moonmass) / (fj 500/2 :km) ** 2) :gravity)

;; ## Ouch!
;;
;; A land-mine holds "51 grams of TNT". Assuming perfect efficiency (`energy = mass · gravity · height`),
;; how high could it throw a 185-pound person? The target is number-led, so we get feet as a count:

(to (fj 51 :grams :TNT) 185 :pounds :gravity :feet)

;; ## Junkyard Wars
;;
;; Floating a submerged half-ton Mini: how many oil barrels' worth of buoyancy is that?

(to (fj :half :ton) :barrels :water)

;; Hand-pumping air 2 fathoms down at 40 watts — minutes to fill a barrel, and food Calories burned:

(to (fj 2 :fathoms :water :gravity :barrel) 40 :watts :minutes)

(fj 2 :fathoms :water :gravity :barrel :to :Calories)

;; ## Body Heat
;;
;; Eat 2000 Calories a day; what's your average power output? Slightly less than a 100-watt bulb:

(fj 2000 :Calories :per :day :to :watts)

;; ## Why is Superman so lazy?
;;
;; Superman charges on sunlight. The sun's power reaching earth's distance is `sunpower / (4 π sundist²)`:

^{:nextjournal.clerk/visibility {:result :hide}}
(defunit earthpower ($= (fj :sunpower) / ($= 4 * (fj :pi) * (fj :sundist) ** 2)))

(fj :earthpower)

;; Presenting ~12 ft² to the sun, his charge rate in watts:

^{:nextjournal.clerk/visibility {:result :hide}}
(defunit chargerate (fj :earthpower 12 :ft :ft))

(fj :chargerate :to :watts)

;; So how long must he charge to lift a 2-ton truck 7 feet (`energy = mass · height · gravity`)?

(to (fj 2 :ton 7 :feet :gravity :per :chargerate) :sec)

;; 25 seconds per rescue. He could be saving a lot more people.

;; ## Hamburgers and cars
;;
;; "Pound for pound, hamburgers cost more than new cars." A 2001 Corvette Z06: 3,115 lb, $48,055. Note
;; commensura even *names* the compound dimension:

(to ($= (fj 48055 :dollars) / (fj 3115 :lb)) :dollars :per :lb)

;; $15/lb — and nobody pays that for hamburger.

;; ## E = mc²
;;
;; The energy in a teaspoon of water, as gallons of gasoline (`:c` is the speed of light):

(to ($= (fj :teaspoon :water) * (fj :c) ** 2) :gallons :gasoline)

;; Millions of gallons — you buy an astonishing amount of energy every time you fill your tank.
;;
;; ---
;; Many more await on Frink's [Sample Calculations](http://futureboy.us/frinkdocs/#SampleCalculations).
;; Thanks, Alan Eliasen and Martin Trojer.
