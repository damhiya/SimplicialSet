module Group where

-- group
class Monoid g => Group g where
  invert :: g -> g
  pow :: Integral a => g -> a -> g

-- abelian group
class Group g => Abelian g where
