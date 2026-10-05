-- | **Run the conformance suite on whichever backend compiled it.**
-- |
-- | `main` is the entry point of every column (`engine/columns/*`); purerl-
-- | tidal's own test suite calls `runConformance` too. A difference prints
-- | both answers and fails the run, since parity reached 100% (2026-10-01).
module Tidal.Conformance.Main
  ( main
  , runConformance
  ) where

import Prelude

import Data.Array (filter, length)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Tidal.Conformance (Result, harmony, haskell, scales, tidal, tidalVersion, voicing)

main :: Effect Unit
main = runConformance

runConformance :: Effect Unit
runConformance = do
  h <- report "Haskell.* against GHC" haskell
  log ("  " <> show h <> " of " <> show (length haskell) <> " cases identical to GHC")
  t <- report ("Parity with Haskell Tidal " <> tidalVersion) tidal
  log ("  parity: " <> show t <> " of " <> show (length tidal) <> " cases identical to Tidal " <> tidalVersion)
  m <- report "Tidal.Harmony against its specification in GHCi" harmony
  log ("  " <> show m <> " of " <> show (length harmony) <> " harmony cases identical")
  v <- report "Tidal.Harmony's voicings against their specification in GHCi" voicing
  log ("  " <> show v <> " of " <> show (length voicing) <> " voicing cases identical")
  c <- report "Tidal.Scales against its specification in GHCi" scales
  log ("  " <> show c <> " of " <> show (length scales) <> " scale cases identical")
  let failed = (length haskell - h) + (length tidal - t) + (length harmony - m) + (length voicing - v) + (length scales - c)
  when (failed > 0) $ throw (show failed <> " case(s) differ from the reference")

report :: String -> Array Result -> Effect Int
report title results = do
  log ""
  log "=========================================="
  log ("  " <> title)
  log "=========================================="
  log ""
  let diffs = filter (\r -> r.expected /= r.actual) results
  for_ diffs \r -> do
    log ("  DIFF  " <> r.input)
    log ("        reference: " <> r.expected)
    log ("        ours:      " <> r.actual)
  pure (length results - length diffs)
