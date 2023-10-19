{-# LANGUAGE DataKinds #-}

module Examples where

import Algebra
import FreeZMod as Z
import FreeZ2Mod as Z2
import Vector
import SimpSet
import Mesh

tetrahedron :: [Face]
tetrahedron =
  [ Face 0 2 1
  , Face 0 1 3
  , Face 0 3 2
  , Face 1 2 3
  ]

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
