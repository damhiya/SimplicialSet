{-# LANGUAGE DataKinds #-}

module Examples where

import Algebra
import FreeZMod as Z
import FreeZ2Mod as Z2
import Vector
import SimpSet
import EuclideanGeometry
import Mesh

m0 :: Word -> Vec3
m0 0 = Vec3 0 0 0
m0 1 = Vec3 1 0 0
m0 2 = Vec3 0 1 0
m0 3 = Vec3 0 0 1

tetrahedron :: [Face]
tetrahedron =
  [ Face 0 2 1
  , Face 0 1 3
  , Face 0 3 2
  , Face 1 2 3
  ]

-- area m0 (oriented tetrahedron) = (3 + sqrt 3)/2
-- volume (shift m0) (inside tetrahedron) = 1/6
-- area' m0 (unoriented tetrahedron) = (3 + sqrt 3)/2

cylinder :: [Face]
cylinder =
  [ Face 0 1 3
  , Face 0 3 2
  , Face 2 3 5
  , Face 2 5 4
  , Face 4 5 1
  , Face 4 1 0
  ]

moebius :: [Face]
moebius =
  [ Face 0 1 3
  , Face 0 3 2
  , Face 2 3 5
  , Face 2 5 4
  , Face 4 5 0
  , Face 4 0 1
  ]
