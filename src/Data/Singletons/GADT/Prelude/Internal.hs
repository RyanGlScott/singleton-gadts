{-# LANGUAGE TemplateHaskell #-}
{-|
Module:      Data.Singletons.GADT.Prelude.Internal
Copyright:   (C) 2018 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Defines some lists of 'Name's in their own module to avoid Template
Haskell staging restrictions.
-}
module Data.Singletons.GADT.Prelude.Internal (
    existingSingInstNames
  , newSingInstNames
  ) where

import           Data.Functor.Const (Const)
import           Data.Functor.Identity (Identity)
import           Data.List.NonEmpty (NonEmpty)
-- import qualified Data.Monoid as Monoid
import           Data.Monoid hiding (First, Last)
import           Data.Nat (Nat)
-- import qualified Data.Semigroup as Semigroup
import           Data.Semigroup hiding (First, Last)
import           Data.Type.Equality
import           Data.Void

import           Language.Haskell.TH (Name)

-- | 'Name's of data types that already have @Sing@ instances defined in the
-- @singletons@ library (or otherwise).
existingSingInstNames :: [Name]
existingSingInstNames = [ ''Maybe
                        , ''[]
                        , ''Either
                        , ''NonEmpty
                        , ''Void
                        , ''(,)
                        , ''(,,)
                        , ''(,,,)
                        , ''(,,,,)
                        , ''(,,,,,)
                        , ''(,,,,,,)
                        , ''Identity
                        , ''Const
                        , ''Bool
                        , ''Ordering
                        , ''()
                        , ''Dual
                        , ''All
                        , ''Any
                        , ''Sum
                        , ''Product
                        , ''Min
                        , ''Max
                        -- TODO RGS: Figure out what to do here
                        {-
                        , ''Semigroup.First
                        , ''Semigroup.Last
                        -}
                        , ''WrappedMonoid
                        {-
                        , ''Monoid.First
                        , ''Monoid.Last
                        -}
                        , ''Nat
                        ]

-- | 'Name's of data types which do not have @Sing@ instances defined in the
-- @singletons@ library (or otherwise).
newSingInstNames :: [Name]
newSingInstNames = [''(:~:), ''(:~~:)]
