{-# LANGUAGE FunctionalDependencies #-}

module Algebra where

import GHC.Natural

-- commutative monoid
class Monoid g => CommutativeMonoid g where

-- group
class Monoid g => Group g where
  invert :: g -> g

-- abelian group
class (CommutativeMonoid g, Group g) => Abelian g where

-- semiring
class Semiring r where
  zero :: Semiring r => r
  one :: r
  (+) :: Semiring r => r -> r -> r
  (*) :: Ring r => r -> r -> r

-- ring
class Semiring r => Ring r where
  inv :: r -> r

-- module
class (Ring r, Abelian m) => Module r m | m -> r where
  scale :: r -> m -> m

instance Semiring Bool where
  zero = False
  one = True
  (+) = (/=)
  (*) = (&&)

instance Ring Bool where
  inv x = x

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
  inv x = -x
