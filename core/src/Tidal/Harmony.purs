-- | **Harmony: a Tidal pattern as a pitch-class set over time.** A machine
-- | that quantises (Odonus) is told its harmony as mini-notation, as Tidal
-- | writes chords: `"<c'maj7 a'min7>/2"`. The host parses it once with
-- | `parseHarmony` and, each step, asks `harmonyAt` for the pitch classes
-- | sounding at that point in the cycle, then hands the machine a plain set.
-- | The machines never import Littorina (`reef` stays outside the GPL); the
-- | hosts that run them (purerl-tidal, Triggerfish) do.
-- |
-- | The pattern is read as Tidal reads `note "..."`, so chord names,
-- | modifiers, stacks (`"[0,4,7]"`), alternation and slowing all mean what
-- | they mean in Tidal. Sampling is ours and is specified in Haskell, in
-- | `oracle/render.hs`, so GHC holds this module to it case by case:
-- |
-- |     harmonyAt p t = sort (nub [ floor (unNote (value e) + 0.5) `mod` 12
-- |                               | e <- queryArc p (Arc t t)
-- |                               , Just (Arc s e') <- [whole e], s <= t, t < e' ])
-- |
-- | A point query, keeping only the events whose whole holds `t` from its
-- | start, so at a boundary the harmony beginning there wins over the one
-- | ending. Pitch classes count from C as 0; a note between semitones goes
-- | to the nearer, halves upwards.
module Tidal.Harmony
  ( Harmony
  , parseHarmony
  , harmonyAt
  , harmonySampler
  ) where

import Prelude

import Data.Array (mapMaybe, nub, sort)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Haskell.Double as Double
import Haskell.Integer as Integer
import Haskell.Rational (ratio)
import Tidal.Core.Types (Time)
import Tidal.Eval.Interpret (tpatToPattern)
import Tidal.Parse.Haskell (TNote(..))
import Tidal.Parse.Parser (parseTPat)
import Tidal.Pattern.Core (queryArc)
import Tidal.Pattern.Types (Arc(..), Event(..), Pattern)

-- | A parsed harmony: a pattern of notes, as `note` would play it.
newtype Harmony = Harmony (Pattern Number)

-- | Mini-notation, read at Tidal's note type.
parseHarmony :: String -> Either String Harmony
parseHarmony src = case parseTPat src of
  Right tpat -> Right (Harmony (map (\(TNote v) -> v) (tpatToPattern tpat)))
  Left err -> Left ("harmony \"" <> src <> "\": " <> show err)

-- | The pitch classes (0 = C) sounding at cycle position `t`, ascending.
harmonyAt :: Harmony -> Time -> Array Int
harmonyAt (Harmony p) t =
  sort (nub (map pitchClass (mapMaybe holding (queryArc p t t))))
  where
  holding = case _ of
    Digital { whole: Arc w, value } | w.start <= t && t < w.stop -> Just value
    _ -> Nothing
  pitchClass x = Integer.toInt (Integer.mod (Double.floor (x + 0.5)) (Integer.fromInt 12))

-- | What a host hands `Reef.Odonus.followHarmony` for one step: pattern text
-- | to the pitch classes at cycle `num / den`. Text that does not parse reads
-- | as a rest. A host at step `n`, each step `q` quarter-beats long, samples
-- | at `harmonySampler (n * q) 16`: four beats to the cycle, as Tidal counts.
harmonySampler :: Int -> Int -> String -> Array Int
harmonySampler num den txt = case parseHarmony txt of
  Right h -> harmonyAt h (ratio (Integer.fromInt num) (Integer.fromInt den))
  Left _ -> []
