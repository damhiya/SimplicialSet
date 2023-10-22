{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Data.IndexedVector where

import GHC.Types
import GHC.TypeNats
import Data.Fin

-- vector
infixr 5 :+

data Vec a :: Natural -> Type where
  Nil :: Vec a 0
  (:+) :: a -> Vec a n -> Vec a (1 + n)

deriving instance Show a => Show (Vec a n)

instance Eq a => Eq (Vec a n) where
  (==) :: Vec a n -> Vec a n -> Bool
  Nil == Nil = True
  (x:+xs) == (y:+ys) = x == y && xs == ys

instance Ord a => Ord (Vec a n) where
  (<=) :: Vec a n -> Vec a n -> Bool
  Nil <= Nil = True
  (x:+xs) <= (y:+ys) = x < y || x == y && xs <= ys

  (<) :: Vec a n -> Vec a n -> Bool
  Nil < Nil = False
  (x:+xs) < (y:+ys) = x < y || x == y && xs < ys

vlength :: Vec a n -> Natural
vlength Nil         = 0
vlength (x:+xs) = 1 + vlength xs

vlookup :: Vec a n -> Fin n -> a
vlookup (x:+xs) Zero    = x
vlookup (x:+xs) (Suc i) = vlookup xs i

insert :: Ord a => a -> Vec a n -> Vec a (1+n)
insert x Nil                 = x :+ Nil
insert x (y:+ys) | x <= y    = x :+ y :+ ys
                 | otherwise = y :+ insert x ys

sort :: Ord a => Vec a n -> Vec a n
sort Nil = Nil
sort (x:+xs) = insert x (sort xs)
