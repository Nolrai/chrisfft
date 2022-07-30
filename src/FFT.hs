{-# LANGUAGE ScopedTypeVariables, NoImplicitPrelude, OverloadedStrings #-}

-- | Example of a library file. It is also used for testing the test suites.
module FFT
  (
    -- * Exported functions
    fft,
    invFft,
    cleanComplex,
    cleanDouble
  ) where

import Data.Vector as V
    -- ( (!), drop, generate, imap, length, map, Vector )
import Data.Complex ( cis, Complex (..) )
import Relude
    -- ( ($),
    --   fromIntegral,
    --   Floating(pi),
    --   Fractional((/)),
    --   Integral(div),
    --   Num((-), (*), (+)),
    --   Ord((<)),
    --   Double,
    --   Int )

data ForwardOrInverse = Forward | Inverse

fft :: HasCallStack => Vector (Complex Double) -> Vector (Complex Double)
fft v =
  if good 
    then ditfft2 Forward v (V.length v) 1 
    else V.fromList []
  where
    logSize :: Double
    logSize = logBase 2 (fromIntegral $ V.length v)
    good :: Bool
    good = (floor logSize :: Int) == ceiling logSize

invFft :: HasCallStack => Vector (Complex Double) -> Vector (Complex Double)
invFft v = (/ fromIntegral (V.length v)) `V.map` ditfft2 Inverse v (V.length v) 1

ditfft2 :: HasCallStack =>
  ForwardOrInverse ->
  Vector (Complex Double) ->
  Int ->
  Int ->
    Vector (Complex Double)
ditfft2 _ v 1 _ = v
ditfft2 b v fullSize stride =
  generate fullSize $ \ ix -> cleanComplex (if ix < halfSize then p!ix + q!ix else p!(ix - halfSize) - q!(ix - halfSize))
  where
  p, q :: Vector (Complex Double)
  p = next v 
  q = (\ ix -> ( wPow ix * )) `imap` next (V.drop stride v)
  wPow :: Int -> Complex Double
  wPow n = cis $ sign * 2 * pi * fromIntegral n / fromIntegral fullSize
  next :: Vector (Complex Double) -> Vector (Complex Double)
  next v' = ditfft2 b v' halfSize (2 * stride)
  halfSize = fullSize `div` 2
  sign :: Double
  sign =
    case b of
    Forward -> 1
    Inverse -> -1

cleanComplex :: Floating a => Complex a -> Complex a
cleanComplex (a :+ b) = cleanDouble a :+ cleanDouble b

cleanDouble :: Floating a => a -> a
cleanDouble d = (big + d) - big
  where
    -- we are just trying to round off the last couple of bits of persision.
    big = 8
