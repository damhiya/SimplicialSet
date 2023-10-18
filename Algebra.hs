module Algebra where

-- commutative monoid
class Monoid g => CommutativeMonoid g where

-- group
class Monoid g => Group g where
  invert :: g -> g
  pow :: Integral a => g -> a -> g

-- abelian group
class (CommutativeMonoid g, Group g) => Abelian g where
