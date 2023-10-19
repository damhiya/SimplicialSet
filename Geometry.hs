module Geometry where

import Data.Semigroup
import FreeZMod as Z
import FreeZ2Mod as Z2
import SimpSet

-- ZX unsigned volume
zvolume :: ASSet x => (x n -> Double) -> FreeZMod (x n) -> Double
zvolume v x = getSum (Z.mkCommutativeMonoidHom f x) -- this is not a monoid hom, indeed.
  where
    f s n = Sum (fromIntegral (abs n) * v s)

-- ZX signed volume
szvolume :: ASSet x => (x n -> Double) -> FreeZMod (x n) -> Double
szvolume v x = getSum (Z.mkCommutativeMonoidHom f x)
  where
    f s n = Sum (fromIntegral n * v s)

-- Z2X unsigned volume
z2volume :: ASSet x => (x n -> Double) -> FreeZ2Mod (x n) -> Double
z2volume m = getSum . Z2.mkCommutativeMonoidHom (Sum . m)
