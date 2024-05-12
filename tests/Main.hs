{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
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
  type Foo :: Type -> Type
  data Foo a where
    MkFoo :: Foo Bool

  type Quux :: Type -> Type
  data Quux a where
    MkQuux1 :: Quux Bool
    MkQuux2 :: Quux Ordering

  type Fin :: Nat -> Type
  data Fin n where
    FZ :: Fin (S n)
    FS :: Fin n -> Fin (S n)

  type Vec :: Nat -> Type -> Type
  data Vec n a where
    VNil  :: Vec Z a
    VCons :: a -> Vec n a -> Vec (S n) a

  type Prox :: k -> Type
  data Prox a where
    P :: forall k (a :: k). Prox a

  type HList :: [Type] -> Type
  data HList l where
    HNil  :: HList '[]
    HCons :: x -> HList xs -> HList (x:xs)
  |])
$(singKindInstances2 [''Foo, ''Quux, ''Fin, ''Vec, ''Prox, ''HList])
