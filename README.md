# commensura

[![CI](https://github.com/crinklywrappr/commensura/actions/workflows/ci.yml/badge.svg)](https://github.com/crinklywrappr/commensura/actions/workflows/ci.yml)
[![Clojars Project](https://img.shields.io/clojars/v/com.github.crinklywrappr/commensura.svg)](https://clojars.org/com.github.crinklywrappr/commensura)

**Exact** unit conversion and dimensional arithmetic for Clojure, inspired by
[Frink](https://frinklang.org). Over 2,000 of Frink's units are reduced to base dimensions in a
single walkable EDN file, and quantities are ordinary Clojure values riding the exact numeric tower —
so an answer is a precise fraction, never a lossy float, unless it is genuinely irrational (in which
case it becomes an explicit arbitrary-precision approximation). How fast is light, in furlongs per
fortnight?

```clojure
(to u/c (per u/furlong u/fortnight))
;=> 99143764200264/55 furlong/fortnight ≈ 1.80261e12 [velocity]   ; exact
```

## 📖 Documentation

The **[commensura guide](https://crinklywrappr.github.io/commensura/)** is a live, rendered notebook —
a hands-on tour with worked examples, charts, and the extensibility patterns. Start there.

## Design in one breath

- **Units are callable vars.** `(u/feet 10)` is ten feet; bare `u/feet` is one foot. Everything is a
  real var (no macros, no keyword soup), so your editor autocompletes.
- **Verbs are plain functions**, deliberately named so they never shadow `clojure.core` — `:refer :all`
  is safe: `by` (×), `per` (÷), `plus`, `minus`, `pow`, `to` (convert), `ratio` (dimensionless count),
  plus comparisons (`lt?`, `certainly-lt?`, …) and `defunit`. Plain numbers are dimensionless scalars.
- **`to` is dimension-preserving** — `(to u/mile u/foot)` is `5280 foot`, still a length, not a bare
  number. (`ratio` gives the bare count when that's what you want.)
- **Exact throughout.** Decimal inputs are rationalized (`3.2` → `16/5`); results stay `Ratio`/`BigInt`
  unless the value is irrational or you ask for a double. Roots and constants that can't be rational
  become arbitrary-precision `≈` approximations, clearly marked.

## A short tour

```clojure
;; Examples show the readable display form; at the REPL a value prints as a `#commensura/quantity "…"`
;; (or `#commensura/unit "…"`) tagged literal that round-trips through the reader.
(require '[commensura.units :as u]
         '[commensura.core :refer :all])          ; collision-free with clojure.core

;; convert (dimension-preserving)
(to (per (u/mile 55) u/hour) (per u/meter u/second))
;=> 15367/625 meter/second ≈ 24.5872 [velocity]

;; a "how many" count → ratio
(ratio u/keg (u/floz 12))
;=> 496/3 ≈ 165.33 [dimensionless]   ; 12-oz cans in a keg

;; define your own unit
(defunit banana (u/cm 18))
(to (u/meter 1) banana)              ;=> 50/9 banana ≈ 5.56 [length]

;; physical comparison (unit-agnostic)
(eq? (u/foot 1) (u/inch 12))         ;=> true

;; roots scale dimensions; exact when it can be
(require '[commensura.math :as m])
(m/sqrt (by (u/meter 3) (u/meter 3)))  ;=> 3 meter [length]
```

**Intervals** propagate uncertainty rigorously — a 2,800-mile road trip at 28–32 mpg on \$3.40–\$3.90 gas:

```clojure
(require '[commensura.interval :as iv])

(def mpg (iv/interval (per (u/miles 28) u/gallon) (per (u/miles 32) u/gallon)))
(def gas (iv/interval (per (u/dollars 34/10) u/gallon) (per (u/dollars 39/10) u/gallon)))
(def cost (to (by (per (u/miles 2800) mpg) gas) u/dollars))
[(iv/lo cost) (iv/hi cost)]
;=> [595/2 dollar ≈ 297.50 [currency], 390 dollar]   ; the guaranteed cost range
```

**Historical purchasing power** and **live currency** are built in:

```clojure
(require '[commensura.cpi :as cpi] '[commensura.currency :as cur])

(to (by 100 (cpi/usd 1913)) u/dollar)   ;=> ≈ $3,379 — $100 of 1913 money, in today's dollars
(cur/EUR 600)                           ;=> 600 EUR, valued live in USD (via CurrencyFreaks)
```

## Features

- **Units + dimensional arithmetic** — 2,000+ units, all reduced to nine base dimensions; exact.
- **Interval arithmetic** — rigorous uncertainty propagation with Frink's `mainValue`; exact bounds.
- **Comparisons** — physical (unit-agnostic) ordering, plus Frink's *certainly* / *possibly* operators
  over intervals.
- **Math** — `sqrt`/`root`/rational `pow`, `abs`/`sign`/`floor`/`ceil`/`round`/`mod`/`min`/`max`, and
  fractional dimensions (`sqrt(Hz)` → `Hz^(1/2)`).
- **Approximate quantities** — irrational results carry an arbitrary-precision value, marked `≈`.
- **Tagged literals** — values print as `#commensura/quantity "…"` / `#commensura/unit "…"` and read
  back through the data reader.
- **Historical US price data (CPI)** — inflation-adjust with `dollar_YYYY` units; exact rationals,
  1913→present, shipped.
- **Live currency** — every ISO code as a discoverable fn (`(cur/EUR 600)`), plus precious metals; the
  client ships, the rates are fetched live.
- **Extensible** — `defunit` (a fixed unit), `register-dimension!` (name a dimension), and
  `register-unit-resolver!` (a whole family of units, e.g. a live-rate `satoshi`).

## Scope

**In scope:** units, dimensional and interval arithmetic, comparisons, runtime math, historical CPI,
and live currency. **Out of scope:** Frink's date/time arithmetic and natural-language features.

## For contributors

The unit data is generated from Frink's `units.txt` (pinned in `dev-resources/`); the runtime reads
only the shipped EDN. Common tasks:

    clojure -X:build test          # offline suite (oracle/live tests self-skip)
    clojure -X:build coverage      # cloverage report → target/coverage/
    clojure -X:build docs          # render the Clerk notebook → target/doc/
    clojure -X:build convert       # units.txt → resources/commensura/units.edn
    clojure -X:build gen-units     # units.edn → src/commensura/units.clj

An opt-in **Frink oracle** cross-checks commensura against the real Frink engine
(`clojure -M:test:frink -m cognitect.test-runner`, or `clojure -X:build test-oracle`). Frink is
proprietary and not redistributed — provide `frink.jar` via the `:frink` alias.

See **[`CONTRIBUTING.md`](CONTRIBUTING.md)** for the full data-regeneration workflow, the data refresh
tasks (FRED CPI, CurrencyFreaks codes), and the nonlinear-function drift procedure.

## License

Copyright © 2026 crinklywrappr

Distributed under the **[GNU General Public License v3.0 or later](https://www.gnu.org/licenses/gpl-3.0.html)**.
Because commensura is GPL, applications that depend on it must be GPL-compatible.

### Attribution

The bundled unit data (`resources/commensura/units.edn`) is derived from
[Frink](https://frinklang.org)'s `units.txt` by Alan Eliasen, itself adapted from the **GNU units**
database by Adrian Mariano, descended from the Bell Labs UNIX `units` program. Values trace to NIST
SP 811, CODATA, the SI Brochure, and the CRC Handbook. See [`NOTICE`](NOTICE) for full attribution —
this lineage is why commensura is GPL. (Note: a few constants — e.g. gasoline energy, TNT, G — track
the current `units.txt`, which has been updated over the years.)
