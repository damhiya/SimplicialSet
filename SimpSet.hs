module SimpSet where

import GHC.Natural
import Algebra
import FreeAbelianGroup as FAG
import FreeCommutativeMonoid as FCM

class Ord s => ASSet s where
  level :: s -> Natural -- dimension + 1
  indices :: s -> [Natural] -- 0 <= i < level
  face :: s -> Natural -> s
  degenerate :: s -> Natural -> s

boundary1 :: ASSet s => s -> FreeAbelianGroup s
boundary1 s
  | level s == 0 = error "No boundary for a (-1)-simplex"
  | level s >= 1 = mconcat [pow (FAG.free (face s i)) ((-1)^i) | i <- indices s]

boundary :: ASSet s => FreeAbelianGroup s -> FreeAbelianGroup s
boundary = FAG.mkCommutativeMonoidHom (\s k -> pow (boundary1 s) k)

instance Ord a => ASSet (FreeCommutativeMonoid a) where
  level s = FCM.size s
  indices s = takeWhile (< level s) [0..]
  face s i = fromList (go (toList s) i)
    where
      go [] i = error "face : index out of range"
      go ((x,n):xs) i
        | i <  n = (x,n-1) : xs
        | i >= n = (x,n) : go xs (i-n)
  degenerate s i = fromList (go (toList s) i)
    where
      go [] i = error "degenerate : index out of range"
      go ((x,n):xs) i
        | i <  n = (x,n+1) : xs
        | i >= n = (x,n) : go xs (i-n)
