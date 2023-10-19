module Geometry where

import Data.Semigroup
import FreeZMod as Z
import FreeZ2Mod as Z2
import SimpSet

type X a n = StdASSet a n
type ZX a n = FreeZMod (X a n)
type Z2X a n = FreeZ2Mod (X a n)

-- ZX unsigned volume
zvolume :: (X a n -> Double) -> ZX a n -> Double
zvolume v x = getSum (Z.mkCommutativeMonoidHom f x)
  where
    f s n = Sum (fromIntegral (abs n) * v s)

-- ZX signed volume
szvolume :: (X a n -> Double) -> ZX a n -> Double
szvolume v x = getSum (Z.mkCommutativeMonoidHom f x)
  where
    f s n = Sum (fromIntegral n * v s)

-- Z2X unsigned volume
z2volume :: (X a n -> Double) -> Z2X a n -> Double
z2volume m = getSum . Z2.mkCommutativeMonoidHom (Sum . m)
