{-# LANGUAGE ScopedTypeVariables, NoImplicitPrelude #-}

import System.FilePath.Glob ( glob )
import Test.DocTest ( doctest )
import Relude ( Monad((>>=)), IO )

main :: IO ()
main = glob "src/**/*.hs" >>= doctest
