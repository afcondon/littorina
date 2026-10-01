# Littorina: the conformance suite in every column, and the oracles that make
# its goldens. Run from this directory.
POLY ?= ../../polyglot/polyglot-template/bin/poly
GHC  ?= ../live-coding/limulus/ghc-tidal

.PHONY: conformance haskell-is-standalone oracle-prim oracle-tidal deps

# GHC, the BEAM and JS must agree case for case (columns/node, columns/erlang).
conformance: deps haskell-is-standalone
	$(abspath $(POLY)) run all

deps:
	@[ -d node_modules ] || npm install --silent

# The reference-semantics types (core/src/Haskell/*) know nothing of Tidal,
# so they can leave for their own package the day a second port needs them.
haskell-is-standalone:
	@! grep -rn "^import Tidal" core/src/Haskell || (echo "core/src/Haskell must not import Tidal.*" && false)

# Regenerate Tidal.Conformance.HaskellGolden by asking GHC, Tidal's
# Sound.Tidal.UI and Text.Parsec (the GHC that Limulus boots).
oracle-prim:
	$(GHC)/bin/runghc core/oracle/haskell-prim.hs \
		< core/oracle/haskell-prim.txt > core/src/Tidal/Conformance/HaskellGolden.purs

# Regenerate Tidal.Conformance.TidalGolden from a GHCi running Tidal behind
# Limulus (:3036).
oracle-tidal:
	node core/oracle/generate.mjs
