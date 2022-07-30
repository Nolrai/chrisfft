{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

import Criterion
import Criterion.Main
import FFT (fft)
import Relude

main :: IO ()
main = defaultMain [bench "fft 4" (whnf fft [0,1,0,1])]
