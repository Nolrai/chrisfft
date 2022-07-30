{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.Complex (Complex (..), cis, magnitude)
import Data.Random.Normal (normalIO')
import Data.Vector as V
  ( Vector,
    foldl',
    freeze,
    filter,
    generate,
    length,
    map,
    mapM_,
    replicateM,
    scanl1,
    sum,
    thaw,
    zip,
    zipWith,
    zipWith3,
    zipWith5, forM
  )
import qualified Data.Vector.Mutable as VM (take)
import Data.Vector.Algorithms.Intro as A (partialSortBy, sortBy)
import FFT (fft, invFft)
import GHC.Float (sqrtDouble)
import Relude
-- import System.Process (callProcess)

meanSquare :: Vector (Complex Double) -> Double
meanSquare = mean . V.map magnitude
  where
    mean :: Vector Double -> Double
    mean u = V.sum u / fromIntegral (V.length u)

rootMeanSquare :: Vector (Complex Double) -> Double
rootMeanSquare = sqrtDouble . meanSquare

mkWave :: HasCallStack => Double -> Double -> Int -> Complex Double
mkWave freq deltaX ix = let t = deltaX * fromIntegral ix in cis (2 * pi * freq * t :: HasCallStack => Double)

mkSignal :: HasCallStack => Int -> Complex Double
mkSignal ix = mkWave 60 0.01 ix + mkWave 100 0.01 ix

signal :: HasCallStack => Double -> Double -> Vector (Complex Double)
signal deltaX duration = V.generate ((2 :: Int) ^ (floor (logBase 2 (duration / deltaX)) :: Int)) mkSignal

noisePoint :: HasCallStack => IO (Complex Double)
noisePoint = do
  x <- normalIO' (0, 1 / sqrtDouble 2)
  y <- normalIO' (0, 1 / sqrtDouble 2)
  pure (x :+ y)

whiteNoise :: HasCallStack => RealFrac a => a -> a -> IO (Vector (Complex Double))
whiteNoise deltaX duration = let numPoints = floor (duration / deltaX) in V.replicateM numPoints noisePoint

maximumMagnitude :: HasCallStack => (Ord a, Num a, RealFloat a) => Vector (Complex a) -> a
maximumMagnitude = V.foldl' (\a b -> max a (magnitude b)) 0

brownNoise :: HasCallStack => Double -> Double -> IO (Vector (Complex Double))
brownNoise deltaX duration = do
  rawBrown <- V.scanl1 (+) <$> whiteNoise deltaX duration
  let (maxValue :: Double) = maximumMagnitude rawBrown
  let (scale :: Double) = duration / maxValue
  pure $ ((scale :+ 0) *) `V.map` rawBrown -- scale it to an average drift of 1 unit per unit time.

noise :: HasCallStack => Double -> Double -> IO (Vector (Complex Double))
noise deltaX duration = V.zipWith (+) <$> whiteNoise deltaX duration <*> brownNoise deltaX duration

noisySignal :: HasCallStack => Double -> Double -> IO (Vector (Complex Double))
noisySignal deltaX duration = V.zipWith (+) (signal deltaX duration) <$> noise deltaX duration

time :: HasCallStack => Double -> Double -> Vector Double
time deltaX duration = generate (floor (duration / deltaX)) (\ix -> fromIntegral ix * deltaX)

putInfoLn :: Text -> Vector (Complex Double) -> IO ()
putInfoLn name v = do
  putTextLn $ name <> " length : " <> show (V.length v)
  putTextLn $ name <> " power : " <> show (meanSquare v)
  putTextLn $ name <> " rms : " <> show (rootMeanSquare v)

-- flipped so we get _decending_ order when we sort
byPower :: (Double, Double) -> (Double, Double) -> Ordering
byPower = flip compare `on` snd

main :: HasCallStack => IO ()
main = do
  (noisySignal' :: Vector (Complex Double)) <- noisySignal deltaT duration
  putInfoLn "noisySignal" noisySignal'
  let transformed = fft noisySignal'
  putInfoLn "transformed" transformed
  let (maxPower :: Double) = maximumMagnitude transformed
  putTextLn $ "maxPower : " <> show (V.length transformed)
  let (clipped :: Vector (Complex Double)) = (\x -> if magnitude x > (maxPower / 2) then x else 0) `V.map` transformed
  putTextLn $ "number uncliped : " <> show (V.length . V.filter (\ x -> magnitude x > maxPower / 2) $ clipped)
  putInfoLn "clipped" clipped
  let reconstructed = invFft clipped
  putInfoLn "reconstructed" reconstructed
  let errorV = V.zipWith (-) signal' reconstructed
  putInfoLn "errorV" errorV
  let errorMagnitude = V.map magnitude errorV
  
  let timeData = "timeData.txt"
  writeFileText timeData "T Signal NoisySignal Reconstructed Error \n"
  appendFileText timeData `V.mapM_` V.zipWith5 toSpacedStrings5 time' signal' noisySignal' reconstructed errorMagnitude

  let freqData = "freqData.txt"
  writeFileText freqData "W transformed clipped \n"
  appendFileText freqData `V.mapM_` V.zipWith3 toSpacedStrings3 time' transformed clipped

  v <- V.thaw . V.zip time' $ V.map magnitude transformed
  A.partialSortBy byPower v 10
  let top' = VM.take 10 v
  A.sortBy byPower top'
  top <- V.freeze top'

  putTextLn ""
  putTextLn "Top 10 frequencies and powers"
  _ <- top `V.forM` \ (freq, power) -> 
    putStrLn $ "freq: " <> show freq <> " Power: " <> show power

  exitSuccess

  -- callProcess "gnuplot" ["-s", "-p", "simple.1.gnu"]
  where
    deltaT, duration :: HasCallStack => Double
    deltaT = 0.000025
    duration = 0.1
    time' = time deltaT duration
    signal' = signal deltaT duration

toSpacedStrings3 :: HasCallStack => Double -> Complex Double -> Complex Double -> Text
toSpacedStrings3 i0 i1 i2 = show i0 <> " " <> show (magnitude i1) <> " " <> show (magnitude i2) <> " \n"

toSpacedStrings5 :: HasCallStack => Double -> Complex Double -> Complex Double -> Complex Double -> Double -> Text
toSpacedStrings5 i0 i1 i2 i3 i4 = show i0 <> " " <> show (magnitude i1) <> " " <> show (magnitude i2) <> " " <> show (magnitude i3) <> " " <> show i4 <> " \n"