{-# LANGUAGE DataKinds #-}

module Examples where

import Algebra
import FreeZMod as Z
import FreeZ2Mod as Z2
import Vector
import SimpSet

type X n = StdASSet Int n
type ZX n = FreeZMod (X n)
type Z2X n = FreeZ2Mod (X n)

simp3 :: Ord a => (a, a, a) -> StdASSet a 3
simp3 (x,y,z) = mkStdASSet (Cons x (Cons y (Cons z Nil)))

parity :: Integral a => (Int, Int, Int) -> a
parity (x,y,z)
  | x == y || y == z || x == z = 0
  | x < y && y < z = 1
  | x < y && z < x = 1
  | x < y && otherwise = -1
  | y < x && x < z = -1
  | y < x && z < y = -1
  | y < x && otherwise = 1

oriented :: (Int, Int, Int) -> ZX 3
oriented x = (scale (parity x) . Z.free) (simp3 x)

unoriented :: (Int, Int, Int) -> Z2X 3
unoriented x@(a,b,c) = Z2.free (simp3 x)

cylinder :: ZX 3
cylinder = foldMap oriented faces
  where
    faces =
      [ (0,1,3)
      , (0,3,2)
      , (2,3,5)
      , (2,5,4)
      , (4,5,1)
      , (4,1,0)
      ]

moebius :: Z2X 3
moebius = foldMap unoriented faces
  where
    faces =
      [ (0,1,3)
      , (0,3,2)
      , (2,3,5)
      , (2,5,4)
      , (4,5,0)
      , (4,0,1)
      ]
