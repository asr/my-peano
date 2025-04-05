-----------------------------------------------------------------------------
-- |
-- Module      : Data.Peano
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2025
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@mail.com>
-- Stability   : experimental
--
-- Peano numbers.
-----------------------------------------------------------------------------

module Data.Peano
  ( Nat(Zero, Succ)
  , fromNatural
  , int2nat
  , nat2int
  , toNatural
  )
where

import Numeric.Natural ( Natural )

import Test.QuickCheck
  ( Arbitrary(arbitrary)
    , arbitrarySizedNatural
  )

-----------------------------------------------------------------------------
-- Auxiliary functions

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-----------------------------------------------------------------------------
-- From http://byorgey.wordpress.com/2010/11/:
--
-- Note that the auto-derived Ord instance have exactly the right
-- behavior due to the fact that we happened to list the Z constructor
-- first.

-- | Peano natural numbers.
data Nat = Zero | Succ Nat
         deriving (Eq, Ord)


nat2integer :: Nat -> Integer
nat2integer Zero     = 0
nat2integer (Succ n) = 1 + nat2integer n

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat n | n < 0 = error "int2Nat: negative argument"
int2nat 0         = Zero
int2nat n         = Succ $ int2nat (n - 1)

integer2nat :: Integer -> Nat
integer2nat n | n < 0 = error "integer2Nat: negative argument"
integer2nat 0         = Zero
integer2nat n         = Succ $ integer2nat (n - 1)

fromNatural :: Natural -> Nat
fromNatural 0 = Zero
fromNatural n = Succ (fromNatural $ pred n)

toNatural :: Nat -> Natural
toNatural Zero     = 0
toNatural (Succ n) = succ $ toNatural n

-- Adapted from http://byorgey.wordpress.com/2010/11/.
instance Num Nat where
  Zero   + n = n
  Succ m + n = Succ (m + n)

  Zero   * _ = Zero
  Succ m * n = n + m * n

  m      - Zero   = m
  Zero   - Succ _ = Zero
  Succ m - Succ n = m - n

  abs n = n

  -- In the @Integral@ class, @div@ is defined via @divMod@ which uses
  -- @negate@. See
  -- https://downloads.haskell.org/~ghc/7.8.4/docs/html/libraries/base-4.7.0.2/src/GHC-Real.html#div
  negate n = n

  signum Zero     = 0
  signum (Succ _) = 1

  fromInteger 0 = Zero
  fromInteger n = if n < 0
                  then error "fromInteger: negative value"
                  else Succ (fromInteger (n - 1))

instance Real Nat where
  toRational = toRational . nat2integer

instance Enum Nat where
  fromEnum = fromEnum . nat2int

  toEnum 0 = Zero
  toEnum n = if n > 0
             then Succ (toEnum (n - 1))
             else error "toEnum: negative value"

instance Integral Nat where
  quotRem m n = mapTuple integer2nat $ quotRem (nat2integer m) (nat2integer n)

  -- TODO (07 July 2014). Why is this definition necessary?
  divMod m n = mapTuple integer2nat $ divMod (nat2integer m) (nat2integer n)

  toInteger = nat2integer

instance Show Nat where
  show = show . toNatural

instance Arbitrary Nat where
  arbitrary = arbitrarySizedNatural
