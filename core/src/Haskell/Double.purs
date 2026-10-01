-- | **Haskell's `Double` functions**: the `Floating Double` methods Tidal
-- | uses, and `floor :: Double -> Integer`.
-- |
-- | Part of the reference-semantics types (`Haskell.*`). PureScript's
-- | `Number` is already an IEEE double, so only the functions are here, and
-- | only because the two package sets disagree on where they live (`Math`
-- | in the purerl set, `Data.Number` in the registry): the engine imports
-- | them from one place that compiles on both.
-- |
-- | GHC calls the C library for `sin`, `cos` and `sqrt`, and so does
-- | Erlang's `math` module, so on the BEAM the results are GHC's to the bit.
-- | V8's `Math.sin` is its own port of fdlibm and may differ from the C
-- | library in the last bit.
-- |
-- | `throughDouble` is GHC's `toRational (fromRational q :: Double)`: a value
-- | rounded to the nearest Double and read back exactly. Tidal reads a
-- | mini-notation number as a Double and then takes `toRational` of it, so
-- | `0.1` is not 1/10 but the Double nearest it. Computed in Integers, not
-- | with a float, because the BEAM has no Infinity and `1e999` is legal Tidal.
module Haskell.Double
  ( sin
  , cos
  , sqrt
  , pi
  , floor
  , throughDouble
  ) where

import Prelude

import Haskell.Integer (Integer)
import Haskell.Integer as Integer
import Haskell.Rational (Rational, denominator, numerator, ratio)
import JS.BigInt (BigInt)
import Data.Tuple (fst, snd)

foreign import sin :: Number -> Number
foreign import cos :: Number -> Number
foreign import sqrt :: Number -> Number
foreign import pi :: Number
foreign import floorImpl :: Number -> BigInt

-- | Exact: the largest integer not above the argument.
floor :: Number -> Integer
floor = Integer.fromBigInt <<< floorImpl

-- | `toRational (fromRational q :: Double)`, exactly: the nearest Double to
-- | `q`, ties to even, through the subnormals; past the largest Double it is
-- | Infinity, whose `toRational` GHC gives as 2^1024.
throughDouble :: Rational -> Rational
throughDouble q =
  let
    n = numerator q
    d = denominator q
  in
    if n == zero then q
    else if n < zero then negate (positive (negate n) d)
    else positive n d

positive :: Integer -> Integer -> Rational
positive n d =
  let
    e = log2 n d
    s = max (e - 52) (-1074)
    -- q / 2^s as a fraction num/den
    num = if s < 0 then n * pow2 (negate s) else n
    den = if s < 0 then d else d * pow2 s
    qr = Integer.quotRem num den
    m0 = fst qr
    r2 = snd qr * two
    m = if r2 > den || (r2 == den && not (Integer.even m0)) then m0 + one else m0
    value = if s < 0 then ratio m (pow2 (negate s)) else ratio (m * pow2 s) one
  in
    if value >= ratio (pow2 1024) one then ratio (pow2 1024) one else value
  where
  two = Integer.fromInt 2

-- | The `e` with 2^e <= n/d < 2^(e+1), for positive n and d.
log2 :: Integer -> Integer -> Int
log2 n d =
  let
    e0 = bits n - bits d
    below = if e0 >= 0 then n < d * pow2 e0 else n * pow2 (negate e0) < d
  in
    if below then e0 - 1 else e0

bits :: Integer -> Int
bits = go 0
  where
  go k x = if x == zero then k else go (k + 1) (Integer.quot x (Integer.fromInt 2))

pow2 :: Int -> Integer
pow2 k = if k <= 0 then one else Integer.fromInt 2 * pow2 (k - 1)
