module MultiSet where

import GHC.Natural
import Data.Map qualified as M
import Data.Map.Merge.Strict qualified as M

newtype MultiSet a = MultiSet (M.Map a Natural)
  deriving (Eq, Ord, Show)

size :: MultiSet a -> Natural
size (MultiSet x) = sum x

union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union (MultiSet x) (MultiSet y) = MultiSet (merge x y)
  where
    merge =
      M.merge
        M.preserveMissing
        M.preserveMissing
        (M.zipWithMaybeMatched
          (\_ m n -> if m + n == 0 then Nothing else Just (m + n)))

fromList :: Ord a => [(a,Natural)] -> MultiSet a
fromList xs = MultiSet (M.fromList . filter ((/=0) . snd) $ xs)

fromMultiList :: Ord a => [a] -> MultiSet a
fromMultiList xs = MultiSet (foldr go M.empty xs)
  where
    go k m = M.insertWith (+) k 1 m
