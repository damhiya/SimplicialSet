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
class ASSet (x :: Natural -> Type) where
  level :: x n -> Natural
  indices :: x n -> [Natural] -- 0 <= i < level
  face :: x (1 + n) -> Natural -> x n
  degenerate :: x n -> Natural -> x (1 + n)

-- alternating boundary
boundary1 :: (Ord (x n), ASSet x) => x (1 + n) -> FreeZMod (x n)
boundary1 s
  | level s == 0 = error "No boundary for a (-1)-simplex"
  | level s >= 1 = mconcat [scale ((-1)^i) (Z.free (face s i)) | i <- indices s]

boundary :: (Ord (x n), ASSet x) => FreeZMod (x (1 + n)) -> FreeZMod (x n)
boundary = Z.mkCommutativeMonoidHom (\s k -> scale k (boundary1 s))

-- non alternating boundary
naboundary1 :: (Ord (x n), ASSet x) => x (1 + n) -> FreeZ2Mod (x n)
naboundary1 s
  | level s == 0 = error "No boundary for a (-1)-simplex"
  | level s >= 1 = mconcat [Z2.free (face s i) | i <- indices s]

naboundary :: (Ord (x n), ASSet x) => FreeZ2Mod (x (1 + n)) -> FreeZ2Mod (x n)
naboundary = Z2.mkCommutativeMonoidHom naboundary1

-- an instance
newtype StdASSet a n = StdASSet (Vec a n)
  deriving (Show, Eq, Ord)

mkStdASSet :: Ord a => Vec a n -> StdASSet a n
mkStdASSet xs = StdASSet (sort xs)

getSequence :: StdASSet a n -> Vec a n
getSequence (StdASSet xs) = xs

instance ASSet (StdASSet a) where
  level (StdASSet xs) = vlength xs
  indices s = takeWhile (< level s) [0..]
  face (StdASSet xs) i = StdASSet (go xs i)
    where
      go :: Vec a (1 + n) -> Natural -> Vec a n
      go (x :+ xs)            0 = xs
      go (x :+ Nil)           i = error "face : index out of range"
      go (x :+ xs@(_:+_)) i = x :+ go xs (i-1)
  degenerate (StdASSet xs) i = StdASSet (go xs i)
    where
      go :: Vec a n -> Natural -> Vec a (1 + n)
      go (x :+ xs)        0 = x :+ x :+ xs
      go (x :+ Nil)       i = error "degenerate : index out of range"
      go (x :+ xs@(_:+_)) i = x :+ go xs (i-1)
