module SimpSet where

import GHC.Natural
import Group
import Data.Map qualified as M
import MultiSet qualified as MS

class Ord s => SimpSet s where
  dim :: s -> Natural
  face :: s -> Natural -> s
  degenerate :: s -> Natural -> s

boundary1 :: SimpSet s => s -> FreeAbelianGroup s
boundary1 s = mconcat [pow (free (face s i)) ((-1)^i) | i <- [0..dim s]]

boundary :: SimpSet s => FreeAbelianGroup s -> FreeAbelianGroup s
boundary = mkMonoidHom (\s k -> pow (boundary1 s) k)

instance Ord a => SimpSet (MS.MultiSet a) where
  dim s = MS.size s - 1
  face (MS.MultiSet s) i = MS.fromList (go (M.toList s) i)
    where
      go [] i = error "face : out of range"
      go ((x,n):xs) i
        | i <  n = (x,n-1) : xs
        | i >= n = (x,n) : go xs (i-n)
  degenerate (MS.MultiSet s) i = MS.fromList (go (M.toList s) i)
    where
      go [] i = error "degenerate : out of range"
      go ((x,n):xs) i
        | i <  n = (x,n+1) : xs
        | i >= n = (x,n) : go xs (i-n)
