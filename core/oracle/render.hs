-- Renders Haskell Tidal events in the form littorina's conformance suite uses:
-- whole|part|key=value,... with values tagged f (float), n (note), quoted
-- strings and bare ints, three decimals. Load into a Tidal GHCi session
-- (Limulus's, for one) with `:script core/oracle/render.hs`, then:
--   render (s "bd*2" # n "1 2 3") 0 1
import qualified Data.Map.Strict as RenderMap
import Text.Printf (printf)
import Data.Ratio (numerator, denominator)
import Data.List (intercalate)
import qualified Data.List

:{
renderRat :: Rational -> String
renderRat x = if denominator x == 1 then show (numerator x) else show (numerator x) ++ "/" ++ show (denominator x)

renderArc :: Arc -> String
renderArc (Arc a b) = renderRat a ++ "-" ++ renderRat b

renderValue :: Value -> String
renderValue v = case v of
  VF x -> "f" ++ printf "%.3f" x
  VN x -> "n" ++ printf "%.3f" (unNote x)
  VS x -> show x
  VI x -> show x
  other -> show other

renderEvent :: Event ValueMap -> String
renderEvent e = maybe "~" renderArc (whole e) ++ "|" ++ renderArc (part e) ++ "|"
  ++ intercalate "," [ k ++ "=" ++ renderValue x | (k, x) <- RenderMap.toList (value e) ]

render :: ControlPattern -> Rational -> Rational -> IO ()
render p a b = print (map renderEvent (queryArc p (Arc a b)))

-- The specification of Tidal.Harmony.harmonyAt (ours, not Tidal's): the
-- pitch classes whose events hold t from their start, rounded halves up.
harmonyAt :: Pattern Note -> Rational -> [Int]
harmonyAt p t = Data.List.sort (Data.List.nub
  [ floor (unNote (value e) + 0.5) `mod` 12
  | e <- queryArc p (Arc t t)
  , Just (Arc s e') <- [whole e], s <= t, t < e' ])

-- The specification of Tidal.Harmony.voicingAt (ours): harmonyAt with the
-- octaves kept, the notes as voiced (Tidal's note numbers, 0 = c5), so a
-- ninth stays a ninth (docs/kb/plans/harmony-routes-coherent.md).
voicingAt :: Pattern Note -> Rational -> [Int]
voicingAt p t = Data.List.sort (Data.List.nub
  [ floor (unNote (value e) + 0.5)
  | e <- queryArc p (Arc t t)
  , Just (Arc s e') <- [whole e], s <= t, t < e' ])

-- The specification of Tidal.Scales.scaleAt (ours): the steps of the scales
-- named at t, sampled as harmonyAt samples, rounded halves up; an unknown
-- name gives nothing. scaleTable at Double, as Odonus reads it.
scaleAt :: Pattern String -> Rational -> [Int]
scaleAt p t = Data.List.sort (Data.List.nub
  [ floor (x + 0.5)
  | e <- queryArc p (Arc t t)
  , Just (Arc s e') <- [whole e], s <= t, t < e'
  , Just steps <- [lookup (value e) (scaleTable :: [(String, [Double])])]
  , x <- steps ])
:}
