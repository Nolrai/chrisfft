
{-# LANGUAGE ScopedTypeVariables, NoImplicitPrelude #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Gen as QC
import Test.SmallCheck.Series
import Relude
import Data.Vector as V
import Data.Complex
import Data.Matrix
import Lib (fft, invFft)
-- import qualified Data.ByteString as B
import Foreign (Storable(sizeOf))

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "SmallCheck" qcTests
  , testGroup "QuickCheck" scTests
  , testGroup "Unit tests" huTests
  ]

qcTests :: [TestTree]
qcTests =
  [
    QC.testProperty "Equals spec" (QC.forAll mkV (\ v -> fft v == fftSpec v))
  ]

scTests :: [TestTree]
scTests =
  [
        SC.testProperty "Equals spec" (SC.over pow2Vector (\ v -> fft v == fftSpec v))
  ]

pow2Vector :: (Monad m, Serial m a) => Series m (Vector a)
pow2Vector = do
  vecSize <- toPow2 <$> getDepth
  localDepth (const vecSize) (decDepth pow2Vector)
    \/ localDepth (\ n -> n - vecSize) (V.replicateM vecSize series)

toPow2 :: Int -> Int
toPow2 n = floor (2 ** logBase 2 (fromIntegral n :: Double))

huTests :: [TestTree]
huTests =
  [
  ]

mkV :: Gen (Vector (Complex Double))
mkV = do
  n <- (`div` 2) <$> getSize
  V.replicateM (2 ^ n) $ (:+) <$> chooseAny <*> chooseAny

fftSpec v = getRow 1 $ fftMatrix (V.length v) `multStd2` colVector v

fftMatrix :: Int -> Matrix (Complex Double)
fftMatrix n = matrix n n f
  where
  -- for some stupid reason Matrix is 1 indexed.
  f :: (Int, Int) -> Complex Double
  f (i, j) = cis (2 * pi / fromIntegral n) 

isEmptyNotPow2 :: Vector (Complex Double) -> Bool
isEmptyNotPow2 v = (fft v == V.fromList []) == isPow2 (V.length v)

isPow2 :: Int -> Bool
isPow2 n = (floor size :: Int) == (ceiling size :: Int)
  where
  size :: Double
  size = logBase 2 (fromIntegral n)