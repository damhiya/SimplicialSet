module FreeZ2Mod where

import Algebra
import Data.Set qualified as S

newtype FreeZ2Mod a = FreeZ2Mod (S.Set a)
  deriving (Eq, Ord, Show)

instance Ord a => Semigroup (FreeZ2Mod a) where
  FreeZ2Mod x <> FreeZ2Mod y = FreeZ2Mod (merge x y)
    where
      merge x y = S.union x y S.\\ S.intersection x y

instance Ord a => Monoid (FreeZ2Mod a) where
  mempty = FreeZ2Mod S.empty

instance Ord a => CommutativeMonoid (FreeZ2Mod a) where

instance Ord a => Group (FreeZ2Mod a) where
  invert x = x

instance Ord a => Semimodule Bool (FreeZ2Mod a) where
  scale True  x = x
  scale False x = mempty

free :: a -> FreeZ2Mod a
free x = FreeZ2Mod (S.singleton x)

instance Foldable FreeZ2Mod where
  foldMap f (FreeZ2Mod x) = foldMap f x
