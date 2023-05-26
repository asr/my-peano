
module Main ( main ) where

import Data.Peano      ( PeanoNat )
import Test.QuickCheck ( (==>), Property, quickCheck )

-- From:
-- https://downloads.haskell.org/~ghc/7.8.4/docs/html/libraries/base-4.7.0.2/Prelude.html#v:signum
prop_signum :: PeanoNat -> Bool
prop_signum x = abs x * signum x == x

-- In non-negative integers @div@ and @quot@ should be have the same
-- behaviour.
prop_div_quot :: PeanoNat -> PeanoNat -> Property
prop_div_quot n d = n >= 0 && d > 0 ==> n `div` d == n `quot` d

main :: IO ()
main = do
  quickCheck prop_signum
  quickCheck prop_div_quot
