{-# LANGUAGE TupleSections  #-}

-- | Example of a library file. It is also used for testing the test suites.
module Lib
  (
    -- * Exported functions
    fft,
    invFft
  ) where

import Data.Vector.Unboxed as V
    ( (!), drop, generate, imap, length, map, Vector )
import Data.Complex ( cis, Complex )

data ForwardOrInverse = Forward | Inverse

fft :: Vector (Complex Double) -> Vector (Complex Double)
fft v = ditfft2 Forward v (V.length v) 1
invFft :: Vector (Complex Double) -> Vector (Complex Double)
invFft v = (/ fromIntegral (V.length v)) `V.map` ditfft2 Inverse v (V.length v) 1

ditfft2 ::
  ForwardOrInverse ->
  Vector (Complex Double) ->
  Int ->
  Int ->
    Vector (Complex Double)
ditfft2 _ v 1 _ = v
ditfft2 b v fullSize stride =
  generate fullSize $ \ ix -> if ix < halfSize then (p ! ix) + (q ! ix) else (p ! ix) - (q ! ix)
  where
  p, q :: Vector (Complex Double)
  p = next v 
  q = (\ ix -> ( wPow ix * )) `imap` next (V.drop 1 v)
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