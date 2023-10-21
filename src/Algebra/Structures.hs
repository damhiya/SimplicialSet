{-# LANGUAGE FunctionalDependencies #-}

module Algebra.Structures where

import GHC.Natural
import Data.Semigroup

-- commutative monoid
class Monoid g => CommutativeMonoid g where

-- group
class Monoid g => Group g where
  invert :: g -> g

(<>-) :: Group g => g -> g -> g
a <>- b = a <> invert b

-- abelian group
type Abelian g = (CommutativeMonoid g, Group g)

-- semiring
class Semiring r where
  zero :: r
  one :: r
  (+) :: r -> r -> r
  (*) :: r -> r -> r

-- ring
class Semiring r => Ring r where
  negate :: r -> r

-- semimodule
class (Semiring r, CommutativeMonoid m) => Semimodule r m | m -> r where
  scale :: r -> m -> m

-- module
type Module r m = (Ring r, Group m, Semimodule r m)

-- instances
instance Num a => CommutativeMonoid (Sum a)

instance Semiring Bool where
  zero = False
  one = True
  (+) = (/=)
  (*) = (&&)

instance Ring Bool where
  negate = id

instance Semiring Natural where
  zero = 0
  one = 1
  (+) = (Prelude.+)
  (*) = (Prelude.*)

instance Semiring Integer where
  zero = 0
  one = 1
  (+) = (Prelude.+)
  (*) = (Prelude.*)

instance Ring Integer where
  negate = Prelude.negate
