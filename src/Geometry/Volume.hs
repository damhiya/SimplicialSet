module Geometry.Volume where

import Data.Semigroup
import Algebra.FreeZMod as Z
import Algebra.FreeZ2Mod as Z2
import Algebra.SimplicialSet

-- ZX unsigned volume
zvolume :: ASSet x => (x n -> Double) -> FreeZMod (x n) -> Double
zvolume v x = getSum (foldMapWithNum f x)
  where
    f n s = Sum (fromIntegral (abs n) * v s)

-- ZX signed volume
szvolume :: ASSet x => (x n -> Double) -> FreeZMod (x n) -> Double
szvolume v x = getSum (foldMapWithNum f x)
  where
    f n s = Sum (fromIntegral n * v s)

-- Z2X unsigned volume
z2volume :: ASSet x => (x n -> Double) -> FreeZ2Mod (x n) -> Double
z2volume m = getSum . foldMap (Sum . m)
