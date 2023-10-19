{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Vector where

import GHC.Types
import GHC.TypeNats

-- vector
data Vec a :: Natural -> Type where
  Nil :: Vec a 0
  Cons :: a -> Vec a n -> Vec a (1 + n)

deriving instance Show a => Show (Vec a n)

instance Eq a => Eq (Vec a n) where
  (==) :: Vec a n -> Vec a n -> Bool
  Nil == Nil = True
  Cons x xs == Cons y ys = x == y && xs == ys

instance Ord a => Ord (Vec a n) where
  (<=) :: Vec a n -> Vec a n -> Bool
  Nil <= Nil = True
  Cons x xs <= Cons y ys = x < y || x == y && xs <= ys

  (<) :: Vec a n -> Vec a n -> Bool
  Nil < Nil = False
  Cons x xs < Cons y ys = x < y || x == y && xs < ys

vlength :: Vec a n -> Natural
vlength Nil         = 0
vlength (Cons x xs) = 1 + vlength xs

vlookup :: Vec a n -> Natural -> a
vlookup Nil i = error "lookup : index out of range"
vlookup (Cons x xs) i | i == 0    = x
                      | otherwise = vlookup xs (i-1)

insert :: Ord a => a -> Vec a n -> Vec a (1+n)
insert x Nil = Cons x Nil
insert x (Cons y ys) | x <= y    = Cons x (Cons y ys)
                     | otherwise = Cons y (insert x ys)

sort :: Ord a => Vec a n -> Vec a n
sort Nil = Nil
sort (Cons x xs) = insert x (sort xs)
