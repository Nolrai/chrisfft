{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main where

-- import Test.QuickCheck.Gen as QC

import Data.Complex
import Data.Matrix
import Data.Vector as V
import FFT (cleanComplex, fft)
import GHC.Float (fromRat)
import Relude
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Monadic as QCM
import Test.Tasty.SmallCheck as SC


-- import qualified Data.ByteString as B
-- import Foreign (Storable(sizeOf))

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "QuickCheck" qcTests
  , testGroup "Unit tests" huTests
  , testGroup "SmallCheck" scTests
  ]

epsilon :: Double
epsilon = 0.01

qcTests :: [TestTree]
qcTests =
  [ QC.testProperty "can generate vectors" $ QC.forAll mkV (\v -> v == v),
    QC.testProperty "Equals spec" . monadicIO $ QCM.forAllM mkV (QCM.run . equalsSpec)
  ]

rms :: Vector (Complex Double) -> Double
rms v = (** 0.5) . V.sum $ (** 2) . magnitude <$> v

scTests :: [TestTree]
scTests =
  [ SC.testProperty "can generate vectors" $ SC.over pow2Vector (\v -> v == v)
  , SC.testProperty "Equals spec" . SC.over pow2Vector $ SC.monadic . equalsSpec
  ]

equalsSpec :: Vector (Complex Double) -> IO Bool
equalsSpec v = do
  let sizeOfV = V.length v
  if sizeOfV <= (10 ^ (4 :: Int))
    then do
      -- putStrLn $ "length v = " <> show (V.length v)
      -- putStrLn $ "length (fft v) = " <> show (V.length $ fft v)
      let err = vMag (V.zipWith (-) (fft v) (fftSpec v))
      -- putStrLn $ "err = " <> show err
      pure $ err < epsilon * (vMag v + 0.01)
    else do
      discard

vMag :: Vector (Complex Double) -> Double
vMag = V.sum . V.map magnitude

doubleSeries :: Monad m => Series m Double
doubleSeries = do
  d <- getDepth 
  case d of
    0 -> pure 0
    1 -> pure 0 \/ pure 1 
    _ -> fromRat <$> series

pow2Vector :: (Monad m) => Series m (Vector (Complex Double))
pow2Vector = vectorSeries 4

complexSeries :: Monad m => Series m (Complex Double)
complexSeries = (:+) <$> doubleSeries <~> doubleSeries

vectorSeries :: Monad m => Int -> Series m (Vector (Complex Double))
vectorSeries size = V.foldl' push (pure V.empty) (V.replicate size (localDepth (`div` size) complexSeries))
  where
    push :: Monad m => Series m (Vector (Complex Double)) -> Series m (Complex Double) -> Series m (Vector (Complex Double))
    push mb ma = V.cons <$> ma <~> mb

toPow2 :: Int -> Int
toPow2 n = 2 ^ (floor (logBase 2 (fromIntegral n :: Double)) :: Int)

huTests :: [TestTree]
huTests =
  [ testCase "45°" $ cleanComplex (((1 / sqrt 2) :+ (1 / sqrt 2) :: Complex Double) - cis (pi / 4)) @?= 0
  ]

mkV :: Gen (Vector (Complex Double))
mkV = do
  n <- toPow2 <$> getSize
  V.replicateM n $ (:+) <$> chooseAny <*> chooseAny

fftSpec :: Vector (Complex Double) -> Vector (Complex Double)
fftSpec v = getCol 1 $ fftMatrix (V.length v) `multStd2` colVector v

fftMatrix :: Int -> Matrix (Complex Double)
fftMatrix n = matrix n n f
  where
    -- for some stupid reason Matrix is 1 indexed.
    f :: (Int, Int) -> Complex Double
    f (i, j) = cleanComplex $ cis (2 * pi / fromIntegral n) ^ ((i - 1) * (j - 1))