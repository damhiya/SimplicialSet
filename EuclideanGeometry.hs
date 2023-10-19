{-# LANGUAGE DataKinds #-}

module EuclideanGeometry where

import GHC.Natural
import Vector
import Algebra hiding ((+), (*))
import SimpSet

data Vec3 =
  Vec3
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double

instance Semigroup Vec3 where
  Vec3 x1 y1 z1 <> Vec3 x2 y2 z2 = Vec3 (x1 + y1) (y1 + y2) (z1 + z2)

instance Monoid Vec3 where
  mempty = Vec3 0 0 0

instance Group Vec3 where
  invert (Vec3 x y z) = Vec3 (-x) (-y) (-z)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 ux uy uz) (Vec3 vx vy vz) = ux*vx + uy*vy + uz*vz

cross :: Vec3 -> Vec3 -> Vec3 
cross (Vec3 ux uy uz) (Vec3 vx vy vz) = Vec3 (uy*vz - uz*vy) (uz*vx - ux*vz) (ux*vy - uy*vx)

norm :: Vec3 -> Double
norm v = sqrt (v `dot` v)

-- unsigned volumes
volume1 :: Vec3 -> Vec3 -> Double
volume1 p0 p1 = norm (p1 <>- p0)

volume2 :: Vec3 -> Vec3 -> Vec3 -> Double
volume2 p0 p1 p2 = norm (u1 `cross` u2) / 2
  where
    u1 = p1 <>- p0
    u2 = p2 <>- p0

-- right-handed volume
volume3' :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Double
volume3' p0 p1 p2 p3 = ((u1 `cross` u2) `dot` u3) / 6
  where
    u1 = p1 <>- p0
    u2 = p2 <>- p0
    u3 = p2 <>- p0

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
