# Littorina

**Tidal's patterns, mini-notation and line language, as one PureScript source
that compiles to both the BEAM and JS**, bug-compatible with Haskell Tidal
1.10.1 and held to it by a GHC oracle on both. Consumed by purerl-tidal (the
BEAM: scheduler, voices, the rig), Triggerfish (the browser) and reef (the
machines, whose quantisers take Tidal patterns).

Named for the periwinkle, the snail of the intertidal zone: the strip the tide
covers and uncovers, beside Limulus, the other creature of the tides.

Laid out as a [polyglot-template](../../polyglot/polyglot-template) program:

```
core/                  the engine; the only place its logic lives
  src/Haskell/*        reference-semantics types: Int, Integer, Rational,
                       Double, Parsec, each specified as "what GHC does"
  src/Haskell/Double.{js,erl}   the one seam (libm, and floor)
  src/Tidal/*          patterns, parser, interpreter, controls, line language
  src/Tidal/Conformance*        the suite, pure, with its goldens
  oracle/              how the goldens are made: GHC, Text.Parsec and
                       GHCi running Tidal 1.10.1 (via Limulus's GHC)
columns/node/          the registry package set, the JS backend
columns/erlang/        the purerl package set, purs-backend-erl
vendor/js-bigints/     the purerl port of the registry's js-bigints
```

`make conformance` (polyglot-template's `poly run all`, after `npm install`
for the erlang column's backend) runs the suite in every column: the claim is
that GHC, the BEAM and JS give the same answers, case for case.

**What core may depend on:** only packages whose API is the same in both
package sets. That is why the engine has its own Parsec rather than the
`parsing` library (6 in the purerl set, 11 in the registry), its own
`Rational` rather than `Data.Rational` (an alias over `Int` in one, a newtype
over `BigInt` in the other), and `Haskell.Double` rather than `Math`.
`js-bigints` is the registry's on JS and the purerl port in
`vendor/js-bigints` on the BEAM.

**Divergence is flagged:** where the engine means something other than
PureScript's standard libraries, it imports a module named for the reference
(`Haskell.*`), and each one is held to GHC by `oracle/haskell-prim.txt`.
`src/Haskell` imports nothing from `Tidal.*` (`make haskell-is-standalone`).

Regenerate the goldens with `make oracle-prim` (GHC) and `make oracle-tidal`
(GHCi Tidal behind Limulus on :3036). Both use Limulus's GHC, so they expect
`../live-coding/limulus` beside this repo.

**History.** Moved out of purerl-tidal on 2026-10-01 (where it was
`engine/`, and before that `src/Tidal/`), so that reef could depend on it
without the two repos depending on each other. This repo carries the two
commits made as `engine/`; the port's earlier history is purerl-tidal's.

## Licence

GPL-3.0-or-later (`LICENSE`), as Tidal is: Littorina is a port of Tidal 1.10.1
(its grammar, chord table and randomness are transcribed from Tidal's source),
so it is a derivative work. `vendor/js-bigints` keeps its upstream MIT licence
(`vendor/js-bigints/LICENSE`).

What this means for the rest of the suite: anything that compiles Littorina
in and is distributed (purerl-tidal, already GPL; a Triggerfish bundle) is
distributed under the GPL as a whole. Its own source can still be MIT, which is
GPL-compatible. Things that only talk to it over a socket (Limulus, the
browser pages talking to the rig) are not combined with it.

