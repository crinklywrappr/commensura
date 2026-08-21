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
;;
;; The prose below is Eliasen's own, verbatim from the Sample Calculations page; commensura's own asides
;; are set off in 💡 callouts.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns frinj-examples
  (:require [nextjournal.clerk :as clerk]
            [commensura.infix :refer [fj $= to]]
            [commensura.core :refer [defunit register-dimension!]]
            [commensura.quantity :as q]
            [tick.core :as t]))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(clerk/add-viewers!
 [{:name :commensura/display
   :pred q/displayable?
   :transform-fn (comp clerk/mark-presented (clerk/update-val str))
   :render-fn '(fn [s] [:span {:style {:color "#047857" :font-family "monospace" :font-weight 500}} s])}])

;; a commensura aside — set off from Eliasen's prose in an amber light-bulb callout
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn note [text]
  (clerk/html
   [:div {:style {:background "#fffbeb" :border-left "4px solid #f59e0b" :border-radius "4px"
                  :padding "0.55rem 0.85rem" :margin "0.6rem 0" :color "#78350f" :font-style "italic"}}
    [:span {:style {:font-style "normal"}} "💡 "] text]))

;; ## Mass and Volume
;;
;; Let's say you wanted to fill your bedroom up with water. How much water would it take?
;; Let's say your room measures 10 feet by 12 feet wide by 8 feet high.

(fj 10 :feet 12 :feet 8 :feet :to :gallons)

^{:nextjournal.clerk/visibility {:code :hide}}
(note "Where frinj reports 552960/77 [dimensionless], commensura keeps the [volume] dimension and the exact fraction — the approximation is shown, never substituted.")

;; It would take approximately 7181 gallons to fill it. Note that you get both an exact
;; fraction and an approximation. How much would that weigh, if you filled it with water?
;; Frinj has the unit "water" which stands for the density of water.

(fj 10 :feet 12 :feet 8 :feet :water :to :pounds)

;; So it would weigh almost 60,000 pounds. What if you knew that your floor could only
;; support 2 tons? How deep could you fill the room with water?

(-> ($= (fj 2 :tons) / (fj 10 :feet 12 :feet :water))
    (to :feet))

;; So you could only fill it about 0.53 feet deep. It'll be a pretty sad pool party.

;; ## Liquor
;;
;; Let's say you want to define a new unit representing the amount of alcohol in a can
;; of (quality) 3.2 beer. Keep in mind that 3.2 beer is measured by alcohol/weight,
;; while almost all other liquors (and many beers) are usually measured in alcohol/volume.
;; The density ratio between water and alcohol is given by:

^{:nextjournal.clerk/visibility {:code :hide}}
(note "frinj's (fj :water :per :alcohol) collapses to 1.2669. commensura keeps the water/alcohol stack (its display value is 1), so the density ratio reads most naturally as a conversion — one water is exactly 10000/7893 alcohols:")

(fj :water :to :alcohol)

;; Water is thus 1.267 times denser than alcohol. 3.2 beer (measured by weight) is thus
;; actually 4.0 percent alcohol as measured by volume. Now let's set that variable in terms
;; of a beer's density of alcohol per volume so we can compare:

^{:nextjournal.clerk/visibility {:result :hide}}
(defunit beer (fj 12 :floz 3.2 :percent :water :per :alcohol))

^{:nextjournal.clerk/visibility {:code :hide}}
(note "frinj uses add-unit!; commensura defines it with the ordinary defunit, so beer is a first-class unit like any other — and later soups resolve :beer by name.")

;; Then, you wanted to find out how many beers a big bottle of champagne is equal to:

(fj :magnum 13.5 :percent :to :beer)

;; You probably don't want to drink that whole bottle. Now let's say you're mixing Jungle
;; Juice (using a 1.75 liter bottle of Everclear (190 proof!)) and Kool-Aid to fill a
;; 5-gallon bucket (any resemblance to my college parties is completely intentional.)
;; What percent alcohol is that stuff?

^{:nextjournal.clerk/visibility {:result :hide}}
(defunit junglejuice ($= (fj 1.75 :liter 190 :proof) / (fj 5 :gallon)))

(fj :junglejuice :to :percent)

;; It's really not that strong. About 8.8%. But if you drink 5 cups of that,
;; at 12 fluid ounces each, how many beers have you had?

(fj 5 12 :floz :junglejuice :to :beer)

;; Maybe that's why people were getting punched in the head. QED.

;; ## More Liquor
;;
;; How many cases in a keg? (A keg is a normal-sized keg, what those in the beer
;; industry would call a "half barrel," or 1/2 beerbarrel in Frinj notation.
;; I don't think they sell full barrels. I've never seen one. It would weigh 258 pounds.
;; A "pony keg" is a "quarter barrel" or, in Frinj notation, ponykeg or 1/4 beerbarrel)

(fj :keg :to :case)

;; How many 12 fluid ounce drinks (i.e. cans o' beer) in a keg?

(-> (fj :keg) (to 12 :floz))

^{:nextjournal.clerk/visibility {:code :hide}}
(note "A number-led `to` target — 12 :floz, a specific quantity rather than a bare unit — asks \"how many of THIS fit\", so it returns the dimensionless count (496/3), matching frinj.")

;; What is the price in dollars per fluid ounce of alcohol when buying a keg of 3.2 beer?
;; (Remember that 3.2 beer is measured in alcohol/weight, so we correct by the density
;; ratio of water/alcohol to get alcohol by volume:

^{:nextjournal.clerk/visibility {:code :hide}}
(note "The next three prices are currency ÷ volume — {:currency 1, :length -3} — which commensura doesn't name out of the box (it does ship \"price per mass\", seen in the Corvette below). register-dimension! names any dims map once, and every value of that shape prints it from then on:")

(register-dimension! {:currency 1 :length -3} "price per volume")

(-> ($= (fj 60 :dollars) / (fj :keg 3.2 :percent :water :per :alcohol))
    (to :dollars :per :floz))

;; A bottle of cheap wine? (A "winebottle" is the standard 750 ml size.)

(-> ($= (fj 6.99 :dollars) / (fj :winebottle 13 :percent))
    (to :dollars :per :floz))

;; A big plastic bottle of really bad vodka?

(-> ($= (fj 13.99 :dollars) / (fj 1750 :ml 80 :proof))
    (to :dollars :per :floz))

;; ## Movie magic
;;
;; In the movie Independence Day, the alien mother ship is said to be 500 km in diameter
;; and have a mass 1/4 that of earth's moon. If the mother ship were a sphere, what would
;; its density be? (The volume of a sphere is 4/3 pi radius3)

(-> ($= (fj 1/4 :moonmass) / ($= (fj 4/3 :pi) * (fj 500/2 :km) ** 3))
    (to :water))

;; This makes the ship 280 times denser than water. This is 36 times denser than iron and
;; more than 12 times denser than any known element! As the ship is actually more a thin disc
;; than a sphere, it would actually be even denser. Since it contains lots of empty space,
;; parts of it would have to be much, much denser.
;;
;; If the object is this dense and has such a large mass, what is its surface gravity?
;; Surface gravity is given by G mass / radius2, where G is the gravitational constant
;; (which Frinj knows about):

(-> ($= (fj :G 1/4 :moonmass) / (fj 500/2 :km) ** 2)
    (to :gravity))

^{:nextjournal.clerk/visibility {:code :hide}}
(note "** binds tighter than * and /, exactly as arithmetic expects — (fj 500/2 :km) ** 2 is squared before the division.")

;; The surface gravity of the spaceship is thus at least twice earth's gravity-- and that's
;; on the rim where gravity is weakest. It would actually be much higher since it's much,
;; much flatter than a sphere. I hope you're not the alien that has to go outside and paint it.

;; ## Fiscal Calculations
;;
;; You can calculate the day that your company will run out of cash, based on their financial
;; statements. The following is an example for a real company, based on SEC filings, which
;; read as the following:
;;
;; Cash and Cash Equivalents (in thousands)
;;
;; | December 31, 2000 | June 30, 2001 |
;; |------------------:|--------------:|
;; |           $86,481 |       $41,601 |

^{:nextjournal.clerk/visibility {:code :hide}}
(note "commensura has no calendar of its own — and doesn't need one. Here tick (juxt/tick, a maintained java.time wrapper) measures the span between the two filing dates; commensura takes it from there as a plain quantity of days. The two libraries compose cleanly, neither knowing about the other.")

;; The two SEC statement dates. `tick` counts the exact days between them — no DST fudge —
;; and commensura wears that span as a quantity of `:days`:

(def statements
  {:end-2000 (t/instant "2000-12-31T00:00:00Z")
   :mid-2001 (t/instant "2001-06-30T00:00:00Z")})

^{:nextjournal.clerk/visibility {:result :hide}}
(defunit burnrate
  ($= (fj (- 86481 41601) :thousand :dollars)
      / (fj (t/days (t/between (:end-2000 statements) (:mid-2001 statements))) :days)))

;; dollars-per-time is a dimension commensura doesn't name by default; call it what it is:
^{:nextjournal.clerk/visibility {:result :hide}}
(register-dimension! {:currency 1 :time -1} "burn rate")

(to (fj :burnrate) :dollars :per :day)

;; You can calculate the number of days until the money runs out at this rate:

(to ($= (fj 41601 :thousand :dollars) / (fj :burnrate)) :days)

^{:nextjournal.clerk/visibility {:code :hide}}
(note "commensura does the exact rational arithmetic (2509927/14960 days); when we need a calendar date back, we hand tick the magnitude in seconds and let it do the date math.")

;; Using date/time math, starting from the last report date (June 30, 2001) you can
;; find out the exact date this corresponds to:

(let [runs-out ($= (fj 41601 :thousand :dollars) / (fj :burnrate))]
  (t/>> (:mid-2001 statements)
        (t/new-duration (long (q/magnitude runs-out)) :seconds)))

;; ## Ouch!
;;
;; At the moment, I'm watching CNN which is discussing some land-mines used in Afghanistan.
;; They showed a very small mine (about the size of a bran muffin) containing "51 grams of TNT"
;; and they asked how much destructive force that carries. Frinj's data file includes how much
;; energy is in a mass of TNT, specified by the unit "TNT". How many feet in the air could 51
;; grams of TNT throw me, assuming perfect efficiency, and knowing energy = mass * gravity * height?

(-> (fj 51 :grams :TNT) (to 185 :pounds :gravity :feet))

;; Yikes. 937 feet. But the only difference between explosives and other combustible fuels
;; is the rapidity of combustion, not in the quantity of energy. How much gasoline contains
;; the same amount of energy?

(-> (fj 51 :grams :TNT) (to :teaspoons :gasoline))

^{:nextjournal.clerk/visibility {:code :hide}}
(note "commensura reports ~1.49, not frinj's 1.29 — the one place the numbers noticeably part ways. It's the `gasoline` unit: its energy content is an empirical figure Frink has revised, and commensura's units.txt (~3.20e10 J/m³) is a later snapshot than frinj's (~3.70e10). Same divergence, ~15%, shows up in E=mc² below. Everything upstream — TNT, the arithmetic — agrees exactly.")

;; 1.29 teaspoons? That's not much at all. You're buying a huge amount of energy when you fill
;; up your car.

;; ## Junkyard Wars
;;
;; I can't watch Junkyard Wars (or lots of other television shows) without having Frinj at
;; my side. This week the team has to float a submerged half-ton Cooper Mini... how many oil
;; barrels will they need to use as floats?

(-> (fj :half :ton) (to :barrels :water))

;; They're trying to hand-pump air down to the barrels, submerged "2 fathoms" below the water.
;; If the guy can sustain 40 watts of pumping power, how many minutes will it take to fill
;; the barrel?

(-> (fj 2 :fathoms :water :gravity :barrel) (to 40 :watts :minutes))

;; And how many food Calories (a food Calorie (with a capital 'C') equals 1000 calories with
;; a small 'c') will he burn to fill a barrel?

(fj 2 :fathoms :water :gravity :barrel :to :Calories)

;; Better eat a Tic-Tac first.

;; ## Body Heat
;;
;; I've seen lots of figures about how much heat the human body produces. You can easily
;; calculate the upper limit based on how much food you eat a day. Say, you eat 2000 Calories
;; a day (again, food Calories with a capital "C" are equal to 1000 calories with a little "c".)

(fj 2000 :Calories :per :day :to :watts)

;; So, your average power and/or heat output is slightly less than a 100-watt bulb.
;; (Note that your heat is radiated over a much larger area so the temperature is much lower.)
;; Many days I could be replaced entirely with a 100-watt bulb and have no discernible effect
;; on the universe.

;; ## Microwave Cookery
;;
;; I'm heating up yummy mustard greens in my microwave, but I don't want to overheat them.
;; I just want to warm them up. If I run my 1100 watt microwave for 30 seconds, how much will
;; their temperature increase? I have a big 27 ounce (mass) can, and I'll assume that their
;; specific heat is about the same as that of water (1 calorie/gram/degC):

(-> ($= (fj 1100 :W 30 :sec) / (fj 27 :oz 1 :calorie :per :gram :per :degC))
    (to :degF))

^{:nextjournal.clerk/visibility {:code :hide}}
(note "This is a temperature *change* — and commensura keeps it that way: :degF here is the interval unit (1 degF = 5/9 K), a plain multiplicative unit distinct from the affine Fahrenheit *scale*. commensura models the two separately, so a difference of degrees composes dimensionally as [temperature] while an absolute reading stays affine — no risk of adding two thermometer readings by accident.")

;; 30 seconds should raise the temperature by no more than 18 degrees Fahrenheit, assuming
;; perfect transfer of microwave energy to heat.
;; Knowing this, I could see how efficiently my microwave actually heats food. I could heat a
;; quantity of water and measure the temperature change in the water. I'll do that sometime if
;; I can find my good thermometer.

;; ## Why is Superman so Lazy?
;;
;; Superman is always rescuing school buses that are falling off of cliffs, flying to the moon,
;; lifting cars over his head, and generally showing off. So why does he still allow so many
;; accidents to happen? Shouldn't he be able to rescue everybody who has a Volkswagen parked
;; on their chest?
;; While searching for answers, I found out three interesting things about Superman:
;;
;; 1. He's 6 feet 3 inches tall.
;; 2. He weighs 225 pounds.
;; 3. He gets his strength from being charged up with solar energy.
;;
;; This is enough information to find some answers. Frinj has units called sunpower
;; (the total power radiated by the sun) and sundist (the distance between the earth and the sun.)
;; Thus, we can find the sun's power that strikes an area at the distance of the earth
;; (knowing the surface area of a sphere is 4 pi radius2):

^{:nextjournal.clerk/visibility {:result :hide}}
(defunit earthpower ($= (fj :sunpower) / ($= 4 * (fj :pi) * (fj :sundist) ** 2)))

(fj :earthpower)

^{:nextjournal.clerk/visibility {:code :hide}}
(note "commensura recognises this compound as a named dimension — [heat flux density] — rather than leaving it as bare kg s^-3.")

;; This is about 1372 watts per square meter. Superman is a pretty big guy--let's say the
;; surface area he can present to the sun is 12 square feet. (This is probably a bit high--
;; it makes him an average of 23 inches wide over his entire height.)
;; This allows Superman to charge up at a power of:

^{:nextjournal.clerk/visibility {:result :hide}}
(defunit chargerate (fj :earthpower 12 :ft :ft))

(fj :chargerate :to :watts)

;; Superman thus charges up at the rate of 1530 joules/sec or 1530 watts. At this rate,
;; how long does he have to charge up before he can lift a 2 ton truck over his head?
;; (Knowing energy = mass * height * gravity)

(to (fj 2 :ton 7 :feet :gravity :per :chargerate) :sec)

;; So, charging up for 25 seconds allows him to save one dumb kid who is acting as a speed bump.
;; So his power is huge but not infinite. He couldn't sustain a higher rate (unless he showed
;; off less by lifting the car only a foot or two.) Lifting a truck every 30 seconds or so
;; isn't bad, though. He could be saving a lot more people. So why doesn't he?

;; ## Hamburgers and Cars
;;
;; "pound for pound, hamburgers cost more than new cars."
;;
;; Let's see... let's try with a medium-expensive, light car. A 2001 Corvette Z06 weighs
;; 3,115 pounds and costs $48,055.

(to ($= (fj 48055 :dollars) / (fj 3115 :lb)) :dollars :per :lb)

^{:nextjournal.clerk/visibility {:code :hide}}
(note "commensura even names the compound dimension of the result: [price per mass].")

;; I know I don't pay $15/lb for hamburger.

;; ## E=mc2
;;
;; Everyone knows Einstein's E=mc2 equation, but to apply it is often very difficult because
;; the units come out so strange. Let's see, I have mass in pounds, and the speed of light is
;; 186,282 miles/second... ummm... what does that come out to? In Frinj the calculation becomes
;; transparently simple.
;;
;; If you took the matter in a teaspoon of water, and converted that to energy, how many gallons
;; of gasoline would that equal?

(to ($= (fj :teaspoon :water) * (fj :c) ** 2) :gallons :gasoline)

;; Unbelievable. The energy in a teaspoon of water, if we could extract it, is equal to burning
;; more than 3 million gallons of gasoline.
;;
;; ---
;; Many more await on Frink's [Sample Calculations](http://futureboy.us/frinkdocs/#SampleCalculations).
;; Thanks, Alan Eliasen and Martin Trojer.
