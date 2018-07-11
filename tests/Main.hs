{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Data.Kind
import Data.Nat
import Data.Singletons.GADT.Prelude ()
import Data.Singletons.GADT.TH

main :: IO ()
main = pure ()

$(singletons1 [d|
  data Foo :: Type -> Type where
    MkFoo :: Foo Bool

  data Quux (a :: Type) where
    MkQuux1 :: Quux Bool
    MkQuux2 :: Quux Ordering

  data Fin :: Nat -> Type where
    FZ :: Fin (S n)
    FS :: Fin n -> Fin (S n)

  data Vec (n :: Nat) (a :: Type) where
    VNil  :: Vec Z a
    VCons :: a -> Vec n a -> Vec (S n) a

  data Prox (a :: k) = P

  data HList :: [Type] -> Type where
    HNil  :: HList '[]
    HCons :: x -> HList xs -> HList (x:xs)
  |])
$(genSingKindInsts2 [''Foo, ''Quux, ''Fin, ''Vec, ''Prox, ''HList])
