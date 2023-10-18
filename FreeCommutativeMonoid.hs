module FreeCommutativeMonoid where

import Prelude hiding ((+))
import Algebra
import GHC.Natural
import Data.Map qualified as M
import Data.Map.Merge.Strict qualified as M

newtype FreeCommutativeMonoid a = FreeCommutativeMonoid (M.Map a Natural)
  deriving (Eq, Ord)

instance (Ord a, Show a) => Show (FreeCommutativeMonoid a) where
  show x = "fromMultiList " ++ show [a | (a, n) <- toList x, i <- [1..n]]

instance Ord a => Semigroup (FreeCommutativeMonoid a) where
  FreeCommutativeMonoid x <> FreeCommutativeMonoid y = FreeCommutativeMonoid (merge x y)
    where
      merge =
        M.merge
          M.preserveMissing
          M.preserveMissing
          (M.zipWithMaybeMatched
            (\_ m n -> if m + n == 0 then Nothing else Just (m + n)))

instance Ord a => Monoid (FreeCommutativeMonoid a) where
  mempty = FreeCommutativeMonoid M.empty

instance Ord a => CommutativeMonoid (FreeCommutativeMonoid a) where

free :: a -> FreeCommutativeMonoid a
free x = FreeCommutativeMonoid (M.singleton x 1)

mkCommutativeMonoidHom :: CommutativeMonoid m => (a -> Natural -> m) -> FreeCommutativeMonoid a -> m
mkCommutativeMonoidHom f (FreeCommutativeMonoid x) = M.foldMapWithKey f x

size :: FreeCommutativeMonoid a -> Natural
size (FreeCommutativeMonoid x) = sum x

toList :: Ord a => FreeCommutativeMonoid a -> [(a,Natural)]
toList (FreeCommutativeMonoid x) = M.toList x

fromList :: Ord a => [(a,Natural)] -> FreeCommutativeMonoid a
fromList xs = FreeCommutativeMonoid (M.fromList . filter ((/=0) . snd) $ xs)

fromMultiList :: Ord a => [a] -> FreeCommutativeMonoid a
fromMultiList xs = FreeCommutativeMonoid (foldr go M.empty xs)
  where
    go k m = M.insertWith (+) k 1 m
