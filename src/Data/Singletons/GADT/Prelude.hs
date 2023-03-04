{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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

import           GHC.TypeLits ( fromSChar, fromSSymbol
                              , withSomeSChar, withSomeSSymbol )
import qualified GHC.TypeNats as TN

import           Numeric.Natural (Natural)

import           Prelude.Singletons hiding ( SingKind(..), DemoteSym0, DemoteSym1
                                           , FromSing, demote, singThat, withSomeSing )

$(singKindInstances1 existingSingInstNames)
$(singKindInstances2 existingSingInstNames)

type instance Demote Char = Char
type instance Promote Char = Char
type instance SingKindC (c :: Char) = ()
instance SingKind Char where
  fromSing = fromSChar
  toSing c = withSomeSChar c SomeSing

type instance Demote Natural = Natural
type instance Promote Natural = Natural
type instance SingKindC (n :: Natural) = ()
instance SingKind Natural where
  fromSing = TN.fromSNat
  toSing n = TN.withSomeSNat n SomeSing

type instance Demote Symbol = Text
type instance Promote Text = Symbol
type instance SingKindC (s :: Symbol) = ()
instance SingKind Symbol where
  fromSing = T.pack . fromSSymbol
  toSing s = withSomeSSymbol (T.unpack s) SomeSing

-- Time to show off
$(genSingletons1     newSingInstNames)
$(singKindInstances2 newSingInstNames)
