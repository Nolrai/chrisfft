import System.Process
import Data.Vector.Unboxed as V
import GHC.Float (sqrtDouble)
import GHC.IO.Handle.Text (hPutStrLn)

mkWave :: Double -> Double -> Int -> Complex Double
mkWave freq deltaX ix = let t = deltaX * fromIntegral ix in cis (2 pi * freq * t)

mkSignal ix = wave (60) 0.01 ix + wave (100) 0.01 ix

signal deltaX length = generate (floor (length / deltaX)) mkSignal

noisePoint :: IO (Complex Double)
noisePoint = do
    x <- normalIO' 0 (sqrtDouble 2)
    y <- normalIO' 0 (sqrtDouble 2)
    pure (x :+ y)

whiteNoise numPoints = V.replicateM numPoints noisePoint

brownNoise :: Double -> Double -> IO (Vector (Complex Double))
brownNoise deltaX length = do
    let numPoints = floor (length / deltaX)
    rawBrown <- scanl1 (+) whiteNoise numPoints
    let maxValue = foldl' (\ a b -> max a (abs b)) rawBrown
    ((length / maxValue) * ) <$> rawBrown -- scale it down to a total drift of 1

noise deltaX length = zipWith (+) <$> whiteNoise <*> brownNoise

noisySignal deltaX length = zipWith (+) (signal deltaX length) <$> noise deltaX length

time deltaX length = generate (floor (length / deltaX)) (\ ix -> ix * deltaX)
 
main :: IO ()
main = do
    
    let time' = time deltaT length
    let signal' = signal deltaT length
    noisySignal' <- noisySignal deltaT length
    let transformed = fft noisySignal'
    let maxPower = foldl' (\ a b -> max a (abs b)) transformed
    let clippedFft = (\ x -> if abs x > (maxPower / 2) then x else 0) `V.map` maxPower
    let reconstructed = invFft clippedFft
    let errorV = (\ a b -> magnitude (a - b)) `V.zipWith` signal' reconstructed
    
    timeData <- openFile WriteMode "timeData.txt"
    hPutStrLn timeData "T Signal NoisySignal Reconstructed Error"
    hPutStrLn dataFile <$> V.zipWith5 formatVectors5 time' signal' noisySignal' reconstructed errorV
    hClose timeData

    freqData <- openFile WriteMode "timeData.txt"
    hPutStrLn freqData "W transformed"
    hPutStrLn freqData <$> V.zipWith5 formatVectors4 time' transformed clipped
    hClose freqData

    callProcess "gnuplot" ["-s", "-p", "simple.1.gnu"]

