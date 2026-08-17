# Contributing to commensura

commensura's runtime reads only shipped, committed artifacts (`resources/commensura/*.edn`, the
generated `src/commensura/{units,currency,dimensions}.clj`). Everything else — Frink's `units.txt`, the
converters, the data refreshers — is **dev-only** and lives under `dev/` and `dev-resources/`. Build,
test, and runtime are fully offline against the committed files; every network fetch is an occasional
**maintainer** action, never per-build.

## Layout

- `src/` — the library (jarred). `resources/` — shipped data (jarred).
- `dev/` — converters and data-refresh scripts (not jarred). `dev-resources/frink/` — pinned upstream
  inputs (`units.txt`, the CPI HTML backup).
- `notebooks/commensura.clj` — the Clerk documentation notebook.
- `test/` — the suite, including the opt-in Frink oracle (`oracle.clj` / `oracle_test.clj`).

## Build tasks

    clojure -X:build test           # offline suite (oracle/live tests self-skip)
    clojure -X:build test-oracle    # full suite incl. the Frink oracle (needs the :frink jar)
    clojure -X:build coverage       # cloverage report → target/coverage/index.html
    clojure -X:build docs           # render the Clerk notebook → target/doc/index.html
    clojure -X:build convert        # units.txt → resources/commensura/units.edn
    clojure -X:build gen-units      # units.edn → src/commensura/units.clj (+ dimensions.clj)
    clojure -X:build cpi-fetch      # refresh resources/commensura/cpi.edn from FRED (needs FRED_API_KEY)
    clojure -X:build cpi-frink      # regenerate the Frink CPI oracle fixture from the pinned HTML
    clojure -X:build currency-codes # refresh resources/commensura/currency-codes.edn (no key)
    clojure -X:build gen-currency   # currency-codes.edn → src/commensura/currency.clj
    clojure -X:build ci             # test + build the jar
    clojure -X:build install        # install the jar locally
    clojure -X:build deploy         # deploy to Clojars

## Regenerating the unit data

The unit vars are generated from Frink's `units.txt` (pinned at `dev-resources/frink/units.txt`):

    clojure -X:build convert     # units.txt → resources/commensura/units.edn
    clojure -X:build gen-units   # units.edn → src/commensura/units.clj (+ dimensions.clj)

After bumping the pinned `units.txt`, run both, then `clojure -X:build test`. If the drift test fails,
follow the procedure below.

## Refreshing the live data (maintainer, networked)

- **CPI** (`cpi.edn`): `FRED_API_KEY=… clojure -X:build cpi-fetch`. The BLS CPI-U series (CPIAUCNS) is
  not seasonally adjusted and never revised, so a refresh only extends the latest month.
- **Currency codes** (`currency-codes.edn`): `clojure -X:build currency-codes` (the supported-currencies
  endpoint needs no key), then `clojure -X:build gen-currency` to regenerate `commensura.currency`.

Live *rates* are never committed — only the client ships. Live tests (`^:live`) run only when the
relevant key (`FRED_API_KEY`, `CURRENCYFREAKS_API_KEY`) is in the environment.

## The Frink oracle

`test/commensura/oracle_test.clj` cross-checks commensura against the **real Frink** engine in-process:
for the exact (rational) subset, commensura's base-SI magnitude must be bit-identical to Frink's.
Frink is proprietary and **not redistributed**; drop `frink.jar` in the project root (gitignored by
`*.jar`; the `:frink` alias picks it up). The oracle namespace loads fine without the jar and its
tests self-skip, so the default suite is unaffected.

The oracle feeds Frink commensura's **own** pinned `dev-resources/frink/units.txt` (via
`parseFilename`, which overrides the jar's auto-loaded embedded copy — `setUnitsFile` is a no-op after
construction). So the comparison is like-for-like on the exact unit data commensura is built from, and
the jar serves only as the *engine* — its bundled units may lag the live upstream without weakening the
oracle. The monthly `frink-sync` workflow keeps `units.txt` current from `frinklang.org/frinkdata`.

    clojure -M:test:frink -m cognitect.test-runner    # or: clojure -X:build test-oracle

## Nonlinear-function drift

A few Frink units are *functions*, not values — `Richter`, the affine temperatures
(`Fahrenheit`/`Celsius`/…) — so they're hand-translated in Clojure. `drift-test` pins the SHA-256 of
each function's `units.txt` body next to its translation and fails if they diverge. After re-pinning
`units.txt`, run `clojure -X:build test`; if the drift test fails, follow the case it names. Both cases
print `(pinned <old>, now <new>)` — `<new>` is the value you paste.

`commensura.units.manifest` is the index of which nonlinear functions exist and where each is handled.

### Case 1 — a pinned body changed (`implemented-…` / `affine-…-match-units-txt`)

1. Open `dev-resources/frink/units.txt`, find the `<Name>[…] :=` block, read the new body.
2. Look up `<Name>` in `functions` in `dev/commensura/units/manifest.clj` — that map is the index of
   where each nonlinear function is handled. Correct the translation there if the formula (not just
   whitespace) changed:
   - `:implemented` → the fully-qualified `:vars` are the function(s) to rewrite.
   - `:affine` → the converter's `affine-temps` table (see the manifest docstring).
3. Paste the failure's `now <new>` SHA into the fingerprint co-located with that code:
   - `:implemented` → the `:frink/sha` on each `:vars` var.
   - `:affine` → the `:sha` in the manifest entry itself.
4. Re-run `clojure -X:build test`. Drift goes green; the function's own tests (e.g. `richter-test`)
   confirm the rewritten formula still gives the right numbers.

### Case 2 — a function was added or removed (`every-catalogued-function-is-classified`)

1. Read the listed `<Name>[…] :=` block in `units.txt` (or, if removed upstream, delete its entry from
   `commensura.units.manifest` and you're done).
2. Add an entry to `functions` in `dev/commensura/units/manifest.clj`:
   - out of scope → `"<Name>" {:status :deferred}` — done, no SHA.
   - an affine temperature → implement it as a var in `commensura.temperature` (like `celsius`), i.e.
     the `:implemented` path below; `:affine` is now only for the redundant single-letter aliases
     (`F`/`C`).
   - implementable → write the translation, tag its var(s) with `{:frink/fn "<Name>" :frink/sha ""}`,
     and add `"<Name>" {:status :implemented :vars '[the.ns/the-fn …]}`.
3. Re-run `clojure -X:build test`. For `:affine`/`:implemented`, the SHA check now fails with
   `now <new>` — paste `<new>` into the `:sha` / `:frink/sha` you left `""`. Re-run once more for green.
