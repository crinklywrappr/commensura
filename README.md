# com.github.crinklywrappr/commensura

Exact unit conversion and dimensional arithmetic for Clojure, inspired by
[Frink](https://frinklang.org). All ~2000 of Frink's units are reduced to base
dimensions in a single walkable EDN file; quantities are ordinary Clojure values
with an exact numeric tower — so `10 ft × 12 ft × 8 ft` in gallons is *exactly*
`552960/77`, not a lossy double.

## Design in one breath

- **Units are callable vars.** `(u/feet 10)` is ten feet; bare `u/feet` is one foot.
  Everything is a real var (no macros, no keyword soup), so your editor autocompletes.
- **Verbs are plain functions.** `by` (×), `per` (÷), `plus`, `minus`, `pow`, `to`
  (convert), `ratio` (dimensionless count). Plain numbers are dimensionless scalars.
- **`to` is dimension-preserving** — `(to (u/mile 1) u/foot)` is `5280 foot`, still a
  length, not a bare number.
- **Exact throughout.** Decimal inputs are rationalized (`3.2` → `16/5`), matching
  Frink; results stay `Ratio`/`BigInt` unless you ask for a double.

## Usage

```clojure
(require '[commensura.units :as u]
         '[commensura.core :refer [by per plus minus pow to ratio defunit]])

;; fill a 10×12×8 ft room with water — how many gallons?
(to (by (u/feet 10) (u/feet 12) (u/feet 8)) u/gallons)
;=> 552960/77 gallon ≈ 7181.30 [volume]   ; the REPL shows this wrapped as
                                          ; #commensura/quantity "…" (round-trips)

;; speed
(to (per (u/mile 55) u/hour) (per u/meter u/second))
;=> … m/s ≈ 24.5872 [velocity]

;; how many 12-oz cans in a keg? (a count → ratio, not to)
(ratio u/keg (u/floz 12))
;=> 496/3 ≈ 165.33 [dimensionless]

;; define your own unit
(defunit beer (by (u/floz 12) (u/percent 3.2) (per u/water u/alcohol)))
(to (by u/magnum (u/percent 13.5)) beer)   ; how many beers in a magnum of champagne
```

Intervals (with midpoint / uncertainty):

```clojure
(require '[commensura.interval :as iv])

(iv/interval-pm 10 3)                 ;=> #commensura/interval [7, 13]
(c/by (iv/interval 1 2) (iv/interval 3 4))   ;=> #commensura/interval [3, 8]
(iv/midpoint (iv/interval 2 8))       ;=> 5
```

## Scope

In scope: units, dimensional arithmetic, and interval arithmetic — fully offline
and deterministic. **Out of scope** (for now): Frink's date/time arithmetic and
natural-language features. Currency exchange rates and historical purchasing power
are planned for later milestones.

## Regenerating the data (contributors)

The unit data is generated from Frink's `units.txt` (pinned in `dev-resources/`):

    clojure -X:build convert     # units.txt  -> resources/commensura/units.edn
    clojure -X:build gen-units   # units.edn   -> src/commensura/units.clj

Run the tests (example/oracle + generative):

    clojure -X:build test

### Nonlinear-function drift

A few Frink units are *functions*, not values — `Richter`, the affine temperatures
(`Fahrenheit`/`Celsius`/…) — so they're hand-translated in Clojure. `drift-test`
pins the SHA-256 of each function's `units.txt` body next to its translation and fails
if they diverge. After re-pinning `units.txt`, run `clojure -X:build test`; if the
drift test fails, follow the case it names. In both cases the failing assertion prints
`(pinned <old>, now <new>)` — `<new>` is the value you paste.

**Case 1 — a pinned body changed** (`implemented-…`/`affine-…-match-units-txt`):

1. Open `dev-resources/frink/units.txt`, find the `<Name>[…] :=` block, read the new body.
2. Look up `<Name>` in `functions` in `dev/commensura/units/manifest.clj` — that map is
   the index of where each nonlinear function is handled. Correct the translation there
   if the formula (not just whitespace) changed:
   - `:implemented` → the fully-qualified `:vars` are the function(s) to rewrite.
   - `:affine` → the converter's `affine-temps` table (see the manifest docstring).
3. Paste the failure's `now <new>` SHA into the fingerprint co-located with that code:
   - `:implemented` → the `:frink/sha` on each `:vars` var.
   - `:affine` → the `:sha` in the manifest entry itself.
4. Re-run `clojure -X:build test`. Drift goes green; the function's own tests
   (e.g. `richter-test`) confirm the rewritten formula still gives the right numbers.

**Case 2 — a function was added or removed** (`every-catalogued-function-is-classified`):

1. Read the listed `<Name>[…] :=` block in `units.txt` (or, if removed upstream,
   delete its entry from `commensura.units.manifest` and you're done).
2. Add an entry to `functions` in `dev/commensura/units/manifest.clj`:
   - out of scope → `"<Name>" {:status :deferred}` — done, no SHA.
   - an affine temperature → implement it as a var in `commensura.temperature`
     (like `celsius`), i.e. the `:implemented` path below; `:affine` is now only for
     the redundant single-letter aliases (`F`/`C`).
   - implementable → write the translation, tag its var(s) with
     `{:frink/fn "<Name>" :frink/sha ""}`, and add
     `"<Name>" {:status :implemented :vars '[the.ns/the-fn …]}`.
3. Re-run `clojure -X:build test`. For `:affine`/`:implemented`, the SHA check now
   fails with `now <new>` — paste `<new>` into the `:sha` / `:frink/sha` you left `""`.
   Re-run once more for green.

`commensura.units.manifest` is the index of which nonlinear functions exist and where
each is handled.

## License

Copyright © 2026 crinklywrappr

Distributed under the **[GNU General Public License v3.0 or later](https://www.gnu.org/licenses/gpl-3.0.html)**.
Because commensura is GPL, applications that depend on it must be GPL-compatible.

### Attribution

The bundled unit data (`resources/commensura/units.edn`) is derived from
[Frink](https://frinklang.org)'s `units.txt` by Alan Eliasen, itself adapted from
the **GNU units** database by Adrian Mariano, descended from the Bell Labs UNIX
`units` program. Values trace to NIST SP 811, CODATA, the SI Brochure, and the CRC
Handbook. See [`NOTICE`](NOTICE) for full attribution — this lineage is why
commensura is GPL. (Note: a few constants — e.g. gasoline energy, TNT, G — track the
current `units.txt`, which has been updated over the years.)
