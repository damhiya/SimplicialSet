module FreeAbelianGroup where

import Prelude hiding ((+), (*))
import Algebra
import Data.Map qualified as M
import Data.Map.Merge.Strict qualified as M

-- free abelian group
newtype FreeAbelianGroup a = FreeAbelianGroup (M.Map a Integer)
  deriving (Eq, Ord, Show)

instance Ord a => Semigroup (FreeAbelianGroup a) where
  FreeAbelianGroup x <> FreeAbelianGroup y = FreeAbelianGroup (merge x y)
    where
      merge =
        M.merge
          M.preserveMissing
          M.preserveMissing
          (M.zipWithMaybeMatched
            (\_ m n -> if m + n == 0 then Nothing else Just (m + n)))

instance Ord a => Monoid (FreeAbelianGroup a) where
  mempty = FreeAbelianGroup M.empty

instance Ord a => CommutativeMonoid (FreeAbelianGroup a) where

instance Ord a => Group (FreeAbelianGroup a) where
  invert (FreeAbelianGroup x) = FreeAbelianGroup (M.map negate x)

instance Ord a => Abelian (FreeAbelianGroup a) where

instance Ord a => Module Integer (FreeAbelianGroup a) where
  scale n (FreeAbelianGroup x) = FreeAbelianGroup (M.map (toInteger n *) x)

free :: a -> FreeAbelianGroup a
free x = FreeAbelianGroup (M.singleton x 1)

mkCommutativeMonoidHom :: CommutativeMonoid m => (a -> Integer -> m) -> FreeAbelianGroup a -> m
mkCommutativeMonoidHom f (FreeAbelianGroup x) = M.foldMapWithKey f x
