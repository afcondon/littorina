-- | **Tidal's scales, by name.** A port of Tidal 1.10.1's `Sound.Tidal.Scales`
-- | (lvm (Mauro) and contributors, GPL-3.0-or-later): the same table, in the
-- | same order, and `scale`, which turns a pattern of degrees into notes:
-- |
-- |     n (scale "<major dorian>" "0 .. 7" + "c")
-- |
-- | A scale counts from 0; the root is added as a note, as in Tidal. A degree
-- | past the end wraps into the next octave (`7` in a seven-note scale is
-- | 12), a negative one into the octave below. An unknown name is the scale
-- | `[0]`, so every degree is a whole octave: Tidal's behaviour, kept.
-- |
-- | Structure comes from the degrees (`<*`): the name is sampled over each
-- | degree's whole. Held to GHC by the Tidal corpus (`core/oracle/corpus.txt`).
module Tidal.Scales
  ( scale
  , getScale
  , scaleTable
  , scaleList
  , lookupScale
  , ScalePattern
  , parseScalePattern
  , scaleAt
  , scaleSampler
  , checkScalePattern
  ) where

import Prelude

import Data.Array (concatMap, filter, find, length, mapMaybe, nub, sort, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..), fst, snd)
import Haskell.Int as HInt
import Haskell.Integer as Integer
import Haskell.Double as Double
import Haskell.Rational (ratio)
import Tidal.Core.Types (Time)
import Tidal.Eval.Interpret (tpatToPattern)
import Tidal.Parse.Haskell (Vocable(..))
import Tidal.Parse.Parser (parseTPat)
import Tidal.Pattern.Core (queryArc)
import Tidal.Pattern.Types (Arc(..), Event(..), Pattern, applyLeft)

-- | Notes from degrees, in the scale each degree's event meets.
scale :: Pattern String -> Pattern Int -> Pattern Number
scale = getScale scaleTable

-- | `scale` over a table of your own.
getScale :: Array (Tuple String (Array Number)) -> Pattern String -> Pattern Int -> Pattern Number
getScale table sp p = applyLeft (map (\n name -> noteInScale (fromMaybe [ 0.0 ] (lookupIn table name)) n) p) sp
  where
  -- Haskell's `div` and `mod` floor. PureScript's Int ones differ by runtime
  -- (Euclidean on JS, truncating on the BEAM), so the arithmetic is Haskell's.
  noteInScale s x =
    let
      len = HInt.fromInt (length s)
      x' = HInt.fromInt x
    in
      fromMaybe 0.0 (s !! Integer.toInt (HInt.toInteger (HInt.mod x' len)))
        + HInt.toNumber (HInt.fromInt 12 * HInt.div x' len)

lookupIn :: Array (Tuple String (Array Number)) -> String -> Maybe (Array Number)
lookupIn table name = snd <$> find (\t -> fst t == name) table

-- | A scale's steps from 0, by Tidal's name.
lookupScale :: String -> Maybe (Array Number)
lookupScale = lookupIn scaleTable

-- | A pattern of scale names, for a machine to follow (Odonus's `scale`
-- | move): `"<dorian mixolydian>/4"`, read as Tidal reads a `Pattern String`.
newtype ScalePattern = ScalePattern (Pattern String)

parseScalePattern :: String -> Either String ScalePattern
parseScalePattern src = case parseTPat src of
  Right tpat -> Right (ScalePattern (map (\(Vocable v) -> v) (tpatToPattern tpat)))
  Left err -> Left ("scale \"" <> src <> "\": " <> show err)

-- | The steps (semitones from the root, ascending) of the scales named at
-- | cycle position `t`, sampled as `Tidal.Harmony.harmonyAt` samples: the
-- | events whose whole holds `t` from its start. Stacked names give the union;
-- | a microtonal step goes to the nearer semitone, halves upwards; an unknown
-- | name gives nothing. Ours, not Tidal's, and specified in Haskell in
-- | `oracle/render.hs` (`scaleAt`), which GHC holds this to.
scaleAt :: ScalePattern -> Time -> Array Int
scaleAt (ScalePattern p) t =
  sort (nub (map semitone (concatMap steps (mapMaybe holding (queryArc p t t)))))
  where
  holding = case _ of
    Digital { whole: Arc w, value } | w.start <= t && t < w.stop -> Just value
    _ -> Nothing
  steps name = fromMaybe [] (lookupScale name)
  semitone x = Integer.toInt (Double.floor (x + 0.5))

-- | What a host hands `Reef.Odonus.followScale` for one step, as
-- | `Tidal.Harmony.harmonySampler` does for the harmony: text to the steps at
-- | cycle `num / den`; text that does not parse is a rest.
scaleSampler :: Int -> Int -> String -> Array Int
scaleSampler num den txt = case parseScalePattern txt of
  Right sp -> scaleAt sp (ratio (Integer.fromInt num) (Integer.fromInt den))
  Left _ -> []

-- | What a host checks before handing a machine a scale pattern: that Tidal
-- | can read it, and that every name in it is in the table, since an unknown
-- | name would sample to nothing and leave the scale silently where it was.
-- | Mini-notation's only words are its atoms, so the names are the words.
checkScalePattern :: String -> Either String String
checkScalePattern src = case parseScalePattern src of
  Left err -> Left err
  Right _ -> case filter (\w -> lookupScale w == Nothing) (nub (names src)) of
    [] -> Right src
    bad -> Left ("scale \"" <> src <> "\": no scale named " <> joinWith ", " bad
      <> " (Tidal's scaleList has " <> show (length scaleTable) <> ": major, dorian, minPent, ...)")
  where
  names s = filter startsWithLetter (String.split (String.Pattern " ") (CU.fromCharArray (map spaceUnlessWord (CU.toCharArray s))))
  spaceUnlessWord c = if isWord c then c else ' '
  isWord c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
  startsWithLetter w = case CU.charAt 0 w of
    Just c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    Nothing -> false

-- | Every name, space-separated, in the table's order: Tidal's `scaleList`.
scaleList :: String
scaleList = joinWith " " (map fst scaleTable)

-- | Tidal's table. Names that alias (`ionian`, `wholetone`, `octatonic`,
-- | `bartok`, ...) are listed as Tidal lists them.
scaleTable :: Array (Tuple String (Array Number))
scaleTable =
  [ Tuple "minPent" [ 0.0, 3.0, 5.0, 7.0, 10.0 ]
  , Tuple "majPent" [ 0.0, 2.0, 4.0, 7.0, 9.0 ]
  , Tuple "ritusen" [ 0.0, 2.0, 5.0, 7.0, 9.0 ]
  , Tuple "egyptian" [ 0.0, 2.0, 5.0, 7.0, 10.0 ]
  , Tuple "kumai" [ 0.0, 2.0, 3.0, 7.0, 9.0 ]
  , Tuple "hirajoshi" [ 0.0, 2.0, 3.0, 7.0, 8.0 ]
  , Tuple "iwato" [ 0.0, 1.0, 5.0, 6.0, 10.0 ]
  , Tuple "chinese" [ 0.0, 4.0, 6.0, 7.0, 11.0 ]
  , Tuple "indian" [ 0.0, 4.0, 5.0, 7.0, 10.0 ]
  , Tuple "pelog" [ 0.0, 1.0, 3.0, 7.0, 8.0 ]
  , Tuple "prometheus" [ 0.0, 2.0, 4.0, 6.0, 11.0 ]
  , Tuple "scriabin" [ 0.0, 1.0, 4.0, 7.0, 9.0 ]
  , Tuple "gong" [ 0.0, 2.0, 4.0, 7.0, 9.0 ]
  , Tuple "shang" [ 0.0, 2.0, 5.0, 7.0, 10.0 ]
  , Tuple "jiao" [ 0.0, 3.0, 5.0, 8.0, 10.0 ]
  , Tuple "zhi" [ 0.0, 2.0, 5.0, 7.0, 9.0 ]
  , Tuple "yu" [ 0.0, 3.0, 5.0, 7.0, 10.0 ]
  , Tuple "whole" [ 0.0, 2.0, 4.0, 6.0, 8.0, 10.0 ]
  , Tuple "wholetone" [ 0.0, 2.0, 4.0, 6.0, 8.0, 10.0 ]
  , Tuple "augmented" [ 0.0, 3.0, 4.0, 7.0, 8.0, 11.0 ]
  , Tuple "augmented2" [ 0.0, 1.0, 4.0, 5.0, 8.0, 9.0 ]
  , Tuple "hexMajor7" [ 0.0, 2.0, 4.0, 7.0, 9.0, 11.0 ]
  , Tuple "hexDorian" [ 0.0, 2.0, 3.0, 5.0, 7.0, 10.0 ]
  , Tuple "hexPhrygian" [ 0.0, 1.0, 3.0, 5.0, 8.0, 10.0 ]
  , Tuple "hexSus" [ 0.0, 2.0, 5.0, 7.0, 9.0, 10.0 ]
  , Tuple "hexMajor6" [ 0.0, 2.0, 4.0, 5.0, 7.0, 9.0 ]
  , Tuple "hexAeolian" [ 0.0, 3.0, 5.0, 7.0, 8.0, 10.0 ]
  , Tuple "major" [ 0.0, 2.0, 4.0, 5.0, 7.0, 9.0, 11.0 ]
  , Tuple "ionian" [ 0.0, 2.0, 4.0, 5.0, 7.0, 9.0, 11.0 ]
  , Tuple "dorian" [ 0.0, 2.0, 3.0, 5.0, 7.0, 9.0, 10.0 ]
  , Tuple "phrygian" [ 0.0, 1.0, 3.0, 5.0, 7.0, 8.0, 10.0 ]
  , Tuple "lydian" [ 0.0, 2.0, 4.0, 6.0, 7.0, 9.0, 11.0 ]
  , Tuple "mixolydian" [ 0.0, 2.0, 4.0, 5.0, 7.0, 9.0, 10.0 ]
  , Tuple "aeolian" [ 0.0, 2.0, 3.0, 5.0, 7.0, 8.0, 10.0 ]
  , Tuple "minor" [ 0.0, 2.0, 3.0, 5.0, 7.0, 8.0, 10.0 ]
  , Tuple "locrian" [ 0.0, 1.0, 3.0, 5.0, 6.0, 8.0, 10.0 ]
  , Tuple "harmonicMinor" [ 0.0, 2.0, 3.0, 5.0, 7.0, 8.0, 11.0 ]
  , Tuple "harmonicMajor" [ 0.0, 2.0, 4.0, 5.0, 7.0, 8.0, 11.0 ]
  , Tuple "melodicMinor" [ 0.0, 2.0, 3.0, 5.0, 7.0, 9.0, 11.0 ]
  , Tuple "melodicMinorDesc" [ 0.0, 2.0, 3.0, 5.0, 7.0, 8.0, 10.0 ]
  , Tuple "melodicMajor" [ 0.0, 2.0, 4.0, 5.0, 7.0, 8.0, 10.0 ]
  , Tuple "bartok" [ 0.0, 2.0, 4.0, 5.0, 7.0, 8.0, 10.0 ]
  , Tuple "hindu" [ 0.0, 2.0, 4.0, 5.0, 7.0, 8.0, 10.0 ]
  , Tuple "todi" [ 0.0, 1.0, 3.0, 6.0, 7.0, 8.0, 11.0 ]
  , Tuple "purvi" [ 0.0, 1.0, 4.0, 6.0, 7.0, 8.0, 11.0 ]
  , Tuple "marva" [ 0.0, 1.0, 4.0, 6.0, 7.0, 9.0, 11.0 ]
  , Tuple "bhairav" [ 0.0, 1.0, 4.0, 5.0, 7.0, 8.0, 11.0 ]
  , Tuple "ahirbhairav" [ 0.0, 1.0, 4.0, 5.0, 7.0, 9.0, 10.0 ]
  , Tuple "superLocrian" [ 0.0, 1.0, 3.0, 4.0, 6.0, 8.0, 10.0 ]
  , Tuple "romanianMinor" [ 0.0, 2.0, 3.0, 6.0, 7.0, 9.0, 10.0 ]
  , Tuple "hungarianMinor" [ 0.0, 2.0, 3.0, 6.0, 7.0, 8.0, 11.0 ]
  , Tuple "neapolitanMinor" [ 0.0, 1.0, 3.0, 5.0, 7.0, 8.0, 11.0 ]
  , Tuple "enigmatic" [ 0.0, 1.0, 4.0, 6.0, 8.0, 10.0, 11.0 ]
  , Tuple "spanish" [ 0.0, 1.0, 4.0, 5.0, 7.0, 8.0, 10.0 ]
  , Tuple "leadingWhole" [ 0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 11.0 ]
  , Tuple "lydianMinor" [ 0.0, 2.0, 4.0, 6.0, 7.0, 8.0, 10.0 ]
  , Tuple "neapolitanMajor" [ 0.0, 1.0, 3.0, 5.0, 7.0, 9.0, 11.0 ]
  , Tuple "locrianMajor" [ 0.0, 2.0, 4.0, 5.0, 6.0, 8.0, 10.0 ]
  , Tuple "diminished" [ 0.0, 1.0, 3.0, 4.0, 6.0, 7.0, 9.0, 10.0 ]
  , Tuple "octatonic" [ 0.0, 1.0, 3.0, 4.0, 6.0, 7.0, 9.0, 10.0 ]
  , Tuple "diminished2" [ 0.0, 2.0, 3.0, 5.0, 6.0, 8.0, 9.0, 11.0 ]
  , Tuple "octatonic2" [ 0.0, 2.0, 3.0, 5.0, 6.0, 8.0, 9.0, 11.0 ]
  , Tuple "messiaen1" [ 0.0, 2.0, 4.0, 6.0, 8.0, 10.0 ]
  , Tuple "messiaen2" [ 0.0, 1.0, 3.0, 4.0, 6.0, 7.0, 9.0, 10.0 ]
  , Tuple "messiaen3" [ 0.0, 2.0, 3.0, 4.0, 6.0, 7.0, 8.0, 10.0, 11.0 ]
  , Tuple "messiaen4" [ 0.0, 1.0, 2.0, 5.0, 6.0, 7.0, 8.0, 11.0 ]
  , Tuple "messiaen5" [ 0.0, 1.0, 5.0, 6.0, 7.0, 11.0 ]
  , Tuple "messiaen6" [ 0.0, 2.0, 4.0, 5.0, 6.0, 8.0, 10.0, 11.0 ]
  , Tuple "messiaen7" [ 0.0, 1.0, 2.0, 3.0, 5.0, 6.0, 7.0, 8.0, 9.0, 11.0 ]
  , Tuple "chromatic" [ 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0 ]
  , Tuple "bayati" [ 0.0, 1.5, 3.0, 5.0, 7.0, 8.0, 10.0 ]
  , Tuple "hijaz" [ 0.0, 1.0, 4.0, 5.0, 7.0, 8.5, 10.0 ]
  , Tuple "sikah" [ 0.0, 1.5, 3.5, 5.5, 7.0, 8.5, 10.5 ]
  , Tuple "rast" [ 0.0, 2.0, 3.5, 5.0, 7.0, 9.0, 10.5 ]
  , Tuple "saba" [ 0.0, 1.5, 3.0, 4.0, 6.0, 8.0, 10.0 ]
  , Tuple "iraq" [ 0.0, 1.5, 3.5, 5.0, 6.5, 8.5, 10.5 ]
  ]
