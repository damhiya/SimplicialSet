module FreeZMod where

import Prelude hiding ((+), (*), negate)
import Algebra
import Data.Map qualified as M
import Data.Map.Merge.Strict qualified as M

-- free abelian group
newtype FreeZMod a = FreeZMod (M.Map a Integer)
  deriving (Eq, Ord, Show)

instance Ord a => Semigroup (FreeZMod a) where
  FreeZMod x <> FreeZMod y = FreeZMod (merge x y)
    where
      merge =
        M.merge
          M.preserveMissing
          M.preserveMissing
          (M.zipWithMaybeMatched
            (\_ m n -> if m + n == 0 then Nothing else Just (m + n)))

instance Ord a => Monoid (FreeZMod a) where
  mempty = FreeZMod M.empty

instance Ord a => CommutativeMonoid (FreeZMod a) where

instance Ord a => Group (FreeZMod a) where
  invert (FreeZMod x) = FreeZMod (M.map negate x)

instance Ord a => Semimodule Integer (FreeZMod a) where
  scale n (FreeZMod x) = FreeZMod (M.map (n*) x)

free :: a -> FreeZMod a
free x = FreeZMod (M.singleton x 1)

mkCommutativeMonoidHom :: CommutativeMonoid m => (a -> Integer -> m) -> FreeZMod a -> m
mkCommutativeMonoidHom f (FreeZMod x) = M.foldMapWithKey f x
