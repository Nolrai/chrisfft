-- | Example of a library file. It is also used for testing the test suites.
module Lib
  (
    -- * Exported functions
    ditfft2
  ) where

import Data.Vector.Unboxed

ditfft2 x 0 s = x
ditfft2 x twoPow s =
  