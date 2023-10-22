{-# LANGUAGE DataKinds #-}

module Geometry.Mesh where

import GHC.Natural
import Algebra.Structures
import Algebra.FreeZMod as Z
import Algebra.FreeZ2Mod as Z2
import Data.IndexedVector
import Algebra.SimplicialSet
import Geometry.Volume
import Geometry.Euclidean

data Face =
  Face
    {-# UNPACK #-} !Word
    {-# UNPACK #-} !Word
    {-# UNPACK #-} !Word
  deriving Show

parity :: Integral a => Face -> a
parity (Face x y z)
  | x == y || y == z || x == z = 1
  | x < y && y < z = 1
  | x < y && z < x = 1
  | x < y && otherwise = -1
  | y < x && x < z = -1
  | y < x && z < y = -1
  | y < x && otherwise = 1

-- interpret as oriented surface
oriented :: [Face] -> FreeZMod (StdASSet Word 3)
oriented = foldMap go
  where
    simplex (Face x y z) = mkStdASSet (x :+ y :+ z :+ Nil)
    go f = (scale (parity f) . Z.free) (simplex f)

-- interpret as unoriented surface
unoriented :: [Face] -> FreeZ2Mod (StdASSet Word 3)
unoriented = foldMap go
  where
    simplex (Face x y z) = mkStdASSet (x :+ y :+ z :+ Nil)
    go f = Z2.free (simplex f)

-- interpret as closed oriented surface, compute the inside of it
inside :: [Face] -> FreeZMod (StdASSet (Maybe Word) 4)
inside = foldMap go
  where
    simplex (Face x y z) = mkStdASSet (Nothing :+ Just x :+ Just y :+ Just z :+ Nil)
    go f = (scale (parity f) . Z.free) (simplex f)

-- shift vertex map
shift :: (Word -> Vec3) -> (Maybe Word -> Vec3)
shift m Nothing = mempty
shift m (Just x) = m x

-- assigning volume to n-simplices
apply :: (a -> Vec3) -> StdASSet a n -> Natural -> Vec3
apply m s i = m (vlookup (getSequence s) i)

svolume1 :: (a -> Vec3) -> StdASSet a 2 -> Double
svolume1 m s = volume1 (p 0) (p 1)
  where
    p = apply m s

svolume2 :: (a -> Vec3) -> StdASSet a 3 -> Double
svolume2 m s = volume2 (p 0) (p 1) (p 2)
  where
    p = apply m s

svolume3' :: (a -> Vec3) -> StdASSet a 4 -> Double
svolume3' m s = volume3' (p 0) (p 1) (p 2) (p 3)
  where
    p = apply m s

-- oriented
len :: (a -> Vec3) -> FreeZMod (StdASSet a 2) -> Double
len m x = zvolume (svolume1 m) x

area :: (a -> Vec3) -> FreeZMod (StdASSet a 3) -> Double
area m x = zvolume (svolume2 m) x

volume :: (a -> Vec3) -> FreeZMod (StdASSet a 4) -> Double
volume m x = szvolume (svolume3' m) x

-- unoriented
len' :: (a -> Vec3) -> FreeZ2Mod (StdASSet a 2) -> Double
len' m x = z2volume (svolume1 m) x

area' :: (a -> Vec3) -> FreeZ2Mod (StdASSet a 3) -> Double
area' m x = z2volume (svolume2 m) x
