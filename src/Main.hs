module Main where

import Data.Semigroup
import Control.Monad
import Algebra.FreeZMod as Z
import Algebra.SimplicialSet
import Geometry.Euclidean
import Geometry.Mesh

readInput :: IO (Word -> Vec3, [Face])
readInput = do
  m <- readLn
  vs <- replicateM m readVertex
  n <- readLn
  fs <- replicateM n readFace
  pure ((vs !!) . fromIntegral, fs)
  where
    readVertex :: IO Vec3
    readVertex = do
      (x, y, z) <- readLn
      pure (Vec3 x y z)
    readFace :: IO Face
    readFace = do
      (f0, f1, f2) <- readLn
      pure (Face f0 f1 f2)

main :: IO ()
main = do
  (m, faces) <- readInput
  let o  = oriented faces
      o' = boundary o
      u  = unoriented faces
      u' = naboundary u
  if o' == mempty then do
    putStrLn "closed and oriented"
    let i = inside faces
        v = volume (shift m) i
        a = area m o
    putStrLn ("volume : " ++ show v)
    putStrLn ("area   : " ++ show a)
  else if getAll (Z.foldMapWithNum (\n s -> All (abs n <= 1)) o') then do
    putStrLn "oriented"
    let a = area m o
        l = len m o'
    putStrLn ("area      : " ++ show a)
    putStrLn ("perimeter : " ++ show l)
  else do
    putStrLn "unoriented"
    let a = area' m u
        l = len' m u'
    putStrLn ("area      : " ++ show a)
    putStrLn ("perimeter : " ++ show l)
