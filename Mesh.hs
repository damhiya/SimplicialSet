{-# LANGUAGE DataKinds #-}

module Mesh where

import Algebra
import FreeZMod as Z
import FreeZ2Mod as Z2
import Vector
import SimpSet
import Geometry
import EuclideanGeometry

data Face =
  Face
    {-# UNPACK #-} !Word
    {-# UNPACK #-} !Word
    {-# UNPACK #-} !Word
  deriving Show

parity :: Integral a => Face -> a
parity (Face x y z)
  | x == y || y == z || x == z = 0
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
    simplex (Face x y z) = mkStdASSet (Cons x (Cons y (Cons z Nil)))
    go f = (scale (parity f) . Z.free) (simplex f)

-- interpret as unoriented surface
unoriented :: [Face] -> FreeZ2Mod (StdASSet Word 3)
unoriented = foldMap go
  where
    simplex (Face x y z) = mkStdASSet (Cons x (Cons y (Cons z Nil)))
    go f = Z2.free (simplex f)

-- interpret as closed oriented surface, compute the inside of it
inside :: [Face] -> FreeZMod (StdASSet (Maybe Word) 4)
inside = foldMap go
  where
    simplex (Face x y z) = mkStdASSet (Cons Nothing (Cons (Just x) (Cons (Just y) (Cons (Just z) Nil))))
    go f = (scale (parity f) . Z.free) (simplex f)

-- boundary of inside
binside :: FreeZMod (StdASSet (Maybe Word) 4) -> FreeZMod (StdASSet (Maybe Word) 3)
binside = Z.mkCommutativeMonoidHom (\s k -> scale k (Z.free (face s 0)))

-- shift vertex map
shift :: (Word -> Vec3) -> (Maybe Word -> Vec3)
shift m Nothing = mempty
shift m (Just x) = m x

-- perimeter of oriented surface
length :: (a -> Vec3) -> FreeZMod (StdASSet a 2) -> Double
length m x = zvolume (svolume1 m) x

-- area of oriented surface
area :: (a -> Vec3) -> FreeZMod (StdASSet a 3) -> Double
area m x = zvolume (svolume2 m) x

-- volume of closed oriented surface
volume :: (a -> Vec3) -> FreeZMod (StdASSet a 4) -> Double
volume m x = zvolume (svolume3' m) x

-- perimeter of unoriented surface
perimeter' :: (a -> Vec3) -> FreeZ2Mod (StdASSet a 2) -> Double
perimeter' m x = z2volume (svolume1 m) x

-- area of unoriented surface
area' :: (a -> Vec3) -> FreeZ2Mod (StdASSet a 3) -> Double
area' m x = z2volume (svolume2 m) x
