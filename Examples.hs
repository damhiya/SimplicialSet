module Examples where

import Algebra
import FreeZMod as Z
import FreeZ2Mod as Z2
import FreeCommutativeMonoid
import SimpSet

type X = FreeCommutativeMonoid Int
type ZX = FreeZMod X
type Z2X = FreeZ2Mod X

parity :: Integral a => (Int, Int, Int) -> a
parity (x,y,z)
  | x == y || y == z || x == z = 0
  | x < y && y < z = 1
  | x < y && z < x = 1
  | x < y && otherwise = -1
  | y < x && x < z = -1
  | y < x && z < y = -1
  | y < x && otherwise = 1

oriented :: (Int, Int, Int) -> ZX
oriented x@(a,b,c) = (scale (parity x) . Z.free . fromMultiList) [a,b,c]

unoriented :: (Int, Int, Int) -> Z2X
unoriented x@(a,b,c) = (Z2.free . fromMultiList) [a,b,c]

cylinder :: ZX
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

moebius :: Z2X
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
