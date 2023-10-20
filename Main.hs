module Main where

import Data.Semigroup
import Control.Monad
import FreeZMod as Z
import SimpSet
import EuclideanGeometry
import Mesh

readInput :: IO (Word -> Vec3, [Face])
readInput = do
  (m, n) <- readLn
  vs <- replicateM m readVertex
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
  let o = oriented faces
      d = boundary o
      u = unoriented faces
      i = inside faces
  if d == mempty then do
    putStrLn "closed and oriented"
  else if getAll (Z.foldMapWithNum (\n s -> All (abs n <= 1)) d) then do
    putStrLn "oriented"
  else do
    putStrLn "unoriented"
