{-# LANGUAGE FunctionalDependencies #-}

module Algebra where

import GHC.Natural

-- commutative monoid
class Monoid g => CommutativeMonoid g where

-- group
class Monoid g => Group g where
  invert :: g -> g

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
  inv :: r -> r

-- module
class (Semiring r, CommutativeMonoid m) => Semimodule r m | m -> r where
  scale :: r -> m -> m

type Module r m = (Ring r, Group m, Semimodule r m)

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
