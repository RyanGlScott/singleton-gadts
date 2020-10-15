{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Data.Singletons.GADT.Prelude
Copyright:   (C) 2018 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Defines 'SingKind' instances for many data types, including some types which
cannot be given instances of the @SingKind@ variant offered by the @singletons@
library.
-}
module Data.Singletons.GADT.Prelude (
    type (%:~:)(..), type (%:~~:)(..)

    -- * Defunctionalization symbols
  , ReflSym0, HReflSym0

    -- * The rest of "Data.Singletons.GADT"
  , module Data.Singletons.GADT

    -- * The rest of "Prelude.Singletons"
  , module Prelude.Singletons
  ) where

import           Data.Functor.Const.Singletons (SConst(..))
import           Data.Functor.Identity.Singletons (SIdentity(..))
import           Data.Monoid.Singletons hiding (SFirst(..), SLast(..))
import           Data.List.NonEmpty.Singletons (SNonEmpty(..))
import           Data.Nat
import           Data.Semigroup.Singletons hiding (SFirst(..), SLast(..))
import           Data.Singletons.GADT
import           Data.Singletons.GADT.Internal
import           Data.Singletons.GADT.TH
import qualified Data.Text as T
import           Data.Text (Text)

import           GHC.TypeLits (SomeSymbol(..), someSymbolVal, symbolVal)
import           GHC.TypeLits.Singletons (SNat(..), SSymbol(..))
import qualified GHC.TypeNats as TN
import           GHC.TypeNats (SomeNat(..))

import           Numeric.Natural (Natural)

import           Prelude.Singletons hiding ( SingKind(..), DemoteSym0, DemoteSym1
                                           , FromSing, demote, singThat, withSomeSing )

$(singKindInstances1 existingSingInstNames)
$(singKindInstances2 existingSingInstNames)

type instance Demote TN.Nat = Natural
type instance Promote Natural = TN.Nat
type instance SingKindC (n :: TN.Nat) = ()
instance SingKind TN.Nat where
  fromSing (SNat :: Sing n) = TN.natVal (Proxy :: Proxy n)
  toSing n = case TN.someNatVal n of
               SomeNat (_ :: Proxy n) -> SomeSing (SNat :: Sing n)

type instance Demote Symbol = Text
type instance Promote Text = Symbol
type instance SingKindC (s :: Symbol) = ()
instance SingKind Symbol where
  fromSing (SSym :: Sing n) = T.pack (symbolVal (Proxy :: Proxy n))
  toSing s = case someSymbolVal (T.unpack s) of
               SomeSymbol (_ :: Proxy n) -> SomeSing (SSym :: Sing n)

-- Time to show off
$(genSingletons1     newSingInstNames)
$(singKindInstances2 newSingInstNames)
