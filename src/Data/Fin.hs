{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Data.Fin where

import GHC.Types
import GHC.TypeNats

data Fin :: Natural -> Type where
  Zero :: Fin (1+n)
  Suc  :: Fin n -> Fin (1+n)

fin0 :: Fin (1+n)
fin0 = Zero

fin1 :: Fin (2+n)
fin1 = Suc Zero

fin2 :: Fin (3+n)
fin2 = Suc (Suc Zero)

fin3 :: Fin (4+n)
fin3 = Suc (Suc (Suc Zero))
