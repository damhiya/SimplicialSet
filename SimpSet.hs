{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module SimpSet where

import Prelude hiding ((+))
import GHC.Types
import GHC.TypeNats
import Algebra
import FreeZMod as Z
import FreeZ2Mod as Z2
import Vector

-- augmented simplicial set
class ASSet (s :: Natural -> Type) where
  level :: s n -> Natural
  indices :: s n -> [Natural] -- 0 <= i < level
  face :: s (1 + n) -> Natural -> s n
  degenerate :: s n -> Natural -> s (1 + n)

-- alternating boundary
boundary1 :: (Ord (s n), ASSet s) => s (1 + n) -> FreeZMod (s n)
boundary1 s
  | level s == 0 = error "No boundary for a (-1)-simplex"
  | level s >= 1 = mconcat [scale ((-1)^i) (Z.free (face s i)) | i <- indices s]

boundary :: (Ord (s n), ASSet s) => FreeZMod (s (1 + n)) -> FreeZMod (s n)
boundary = Z.mkCommutativeMonoidHom (\s k -> scale k (boundary1 s))

-- non alternating boundary
naboundary1 :: (Ord (s n), ASSet s) => s (1 + n) -> FreeZ2Mod (s n)
naboundary1 s
  | level s == 0 = error "No boundary for a (-1)-simplex"
  | level s >= 1 = mconcat [Z2.free (face s i) | i <- indices s]

naboundary :: (Ord (s n), ASSet s) => FreeZ2Mod (s (1 + n)) -> FreeZ2Mod (s n)
naboundary = Z2.mkCommutativeMonoidHom naboundary1

-- an instance
newtype StdASSet a n = StdASSet (Vec a n)
  deriving (Show, Eq, Ord)

mkStdASSet :: Ord a => Vec a n -> StdASSet a n
mkStdASSet xs = StdASSet (sort xs)

instance Ord a => ASSet (StdASSet a) where
  level (StdASSet xs) = vlength xs
  indices s = takeWhile (< level s) [0..]
  face (StdASSet xs) i = StdASSet (go xs i)
    where
      go :: Vec a (1 + n) -> Natural -> Vec a n
      go (Cons x xs)            0 = xs
      go (Cons x Nil)           i = error "face : index out of range"
      go (Cons x xs@(Cons _ _)) i = Cons x (go xs (i-1))
  degenerate (StdASSet xs) i = StdASSet (go xs i)
    where
      go :: Vec a n -> Natural -> Vec a (1 + n)
      go (Cons x xs)            0 = Cons x (Cons x xs)
      go (Cons x Nil)           i = error "degenerate : index out of range"
      go (Cons x xs@(Cons _ _)) i = Cons x (go xs (i-1))
