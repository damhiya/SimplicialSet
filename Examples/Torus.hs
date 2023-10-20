module Examples.Torus where

import EuclideanGeometry
import Mesh

x :: Double -> Double -> Double -> Double -> Vec3
x a b phi theta =
  Vec3
    (a * cos phi + b * cos phi * cos theta)
    (a * sin phi + b * sin phi * cos theta)
    (b * sin theta)

torusMesh :: Double -> Double -> Word -> Word -> (Word -> Vec3, [Face])
torusMesh a b m n = (vertices, faces)
  where
    phi   i = 2*pi / fromIntegral m * fromIntegral i
    theta j = 2*pi / fromIntegral n * fromIntegral j
    vertices k = x a b (phi i) (theta j)
      where
        i = k `div` n
        j = k `mod` n
    faces =
      [ f
      | i <- [0..m-1]
      , j <- [0..n-1]
      , let i' = (i+1) `mod` m
      , let j' = (j+1) `mod` n
      , f <- [ Face (n*i + j ) (n*i' + j) (n*i  + j')
             , Face (n*i + j') (n*i' + j) (n*i' + j')
             ]
      ]

test :: Double -> Double -> Word -> Word -> (Double, Double)
test a b m n = ( volume (shift v) (inside torus)   - 2*pi^2 * a * b^2
               , area   v         (oriented torus) - 4*pi^2 * a * b
               )
  where
    (v, torus) = torusMesh a b m n
