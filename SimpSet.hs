module SimpSet where

import GHC.Natural
import Data.Map qualified as M
import Group
import FreeAbelianGroup as FAG
import FreeCommutativeMonoid as FCM

class Ord s => ASSet s where
  level :: s -> Natural -- dimension + 1
  indices :: s -> [Natural] -- 0 <= i < level
  face :: s -> Natural -> s
  degenerate :: s -> Natural -> s

boundary1 :: ASSet s => s -> FreeAbelianGroup s
boundary1 s
  | level s == 0 = error "No boundary for (-1)-simplicis"
  | level s >= 1 = mconcat [pow (free (face s i)) ((-1)^i) | i <- indices s]

boundary :: ASSet s => FreeAbelianGroup s -> FreeAbelianGroup s
boundary = mkMonoidHom (\s k -> pow (boundary1 s) k)

instance Ord a => ASSet (FreeCommutativeMonoid a) where
  level s = FCM.size s
  indices s = takeWhile (< level s) [0..]
  face (FreeCommutativeMonoid s) i = FCM.fromList (go (M.toList s) i)
    where
      go [] i = error "face : index out of range"
      go ((x,n):xs) i
        | i <  n = (x,n-1) : xs
        | i >= n = (x,n) : go xs (i-n)
  degenerate (FreeCommutativeMonoid s) i = FCM.fromList (go (M.toList s) i)
    where
      go [] i = error "degenerate : index out of range"
      go ((x,n):xs) i
        | i <  n = (x,n+1) : xs
        | i >= n = (x,n) : go xs (i-n)
