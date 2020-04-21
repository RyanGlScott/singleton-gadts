{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
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

  data Quux :: Type -> Type where
    MkQuux1 :: Quux Bool
    MkQuux2 :: Quux Ordering

  data Fin :: Nat -> Type where
    FZ :: Fin (S n)
    FS :: Fin n -> Fin (S n)

  data Vec :: Nat -> Type -> Type where
    VNil  :: Vec Z a
    VCons :: a -> Vec n a -> Vec (S n) a

  data Prox :: forall k. k -> Type where
    P :: forall k (a :: k). Prox a

  data HList :: [Type] -> Type where
    HNil  :: HList '[]
    HCons :: x -> HList xs -> HList (x:xs)
  |])
$(singKindInstances2 [''Foo, ''Quux, ''Fin, ''Vec, ''Prox, ''HList])
