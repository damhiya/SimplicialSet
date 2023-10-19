module EuclideanGeometry where

import Algebra hiding ((+), (*))

data Vec3 = Vec3 Double Double Double

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

volume1 :: Vec3 -> Vec3 -> Double
volume1 p0 p1 = norm (p1 <>- p0)

volume2 :: Vec3 -> Vec3 -> Vec3 -> Double
volume2 p0 p1 p2 = norm (u1 `cross` u2) / 2
  where
    u1 = p1 <>- p0
    u2 = p2 <>- p0

volume3 :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Double
volume3 p0 p1 p2 p3 = (u1 `cross` u2) `dot` u3 / 6
  where
    u1 = p1 <>- p0
    u2 = p2 <>- p0
    u3 = p2 <>- p0
