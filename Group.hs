module Group where

import Data.Map qualified as M
import Data.Map.Merge.Strict qualified as M

-- group
class Monoid g => Group g where
  invert :: g -> g
  pow :: Integral a => g -> a -> g

-- abelian group
class Group g => Abelian g where

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
  mempty = FreeAbelianGroup (M.empty)

instance Ord a => Group (FreeAbelianGroup a) where
  invert (FreeAbelianGroup x) = FreeAbelianGroup (M.map negate x)
  pow (FreeAbelianGroup x) n = FreeAbelianGroup (M.map (toInteger n *) x)

instance Ord a => Abelian (FreeAbelianGroup a) where

free :: a -> FreeAbelianGroup a
free x = FreeAbelianGroup (M.singleton x 1)

mkMonoidHom :: Monoid m => (a -> Integer -> m) -> FreeAbelianGroup a -> m
mkMonoidHom f (FreeAbelianGroup x) = M.foldMapWithKey f x
