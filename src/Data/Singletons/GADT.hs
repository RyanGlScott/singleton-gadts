{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module:      Data.Singletons.GADT
Copyright:   (C) 2018 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Defines a 'SingKind' class (and supporting type families) which can support
GADTs and poly-kinded data types.
-}
module Data.Singletons.GADT (
    -- * Basic definitions
    Promote, PromoteX
  , Demote, DemoteX
  , PromoteDemoteInverse
  , SingKindC, SingKindX, SingKind(..)
    -- $working_around_staging

    -- * Useful combinators
  , withSomeSing
  , singThat
  , demote
  , pattern FromSing

    -- * Defunctionalization symbols
  , DemoteSym0, DemoteSym1
  , PromoteSym0, PromoteSym1
  {-
  , DemoteXSym0, DemoteXSym1
  , PromoteXSym0, PromoteXSym1
  -}
  , PromoteDemoteInverseSym0, PromoteDemoteInverseSym1
  , SingKindCSym0, SingKindCSym1
  , SingKindXSym0, SingKindXSym1

    -- * The rest of "Data.Singletons"
  , module Data.Singletons
  ) where

import Data.Kind
import Data.Singletons hiding
  ( SingKind(..), DemoteSym0, DemoteSym1, FromSing
  , demote, singThat, withSomeSing )
import Data.Singletons.TH (genDefunSymbols)
import Data.Type.Equality
import Unsafe.Coerce

-----
-- Basic defintions
-----

-- | Get a promoted kind from the base type. For example:
--
-- * @'Promote' 'Bool' = 'Bool'@
-- * @'Promote' ('Maybe' a) = 'Maybe' ('Promote' a)@
-- * @'Promote' ('Either' a b) = 'Either' ('Promote' a) ('Promote' b)@
--
-- Rarely, the type and kind do not match. For example:
--
-- * @'Promote' (a -> b) = 'Promote' a '~>' 'Promote' b@
-- * @'Promote' Natural = Nat@
-- * @'Promote' Text = Symbol@
type family Promote (k :: Type) :: Type
-- | A generalized form of 'Promote' which can work over types of any kind.
-- @'PromoteX' (a :: k)@ is @'Promote' a@ when @k@ is @Type@.
-- For some non-@Type@ examples:
--
-- * @'PromoteX' 'True' = 'True'@
-- * @'PromoteX' ('Just' x) = 'Just' ('PromoteX' x)@
-- * @'PromoteX' ((:) a) = 'TyCon1' ((:) ('PromoteX' a))@
type family PromoteX (a :: k) :: Promote k

-- | Get a base type from the promoted kind. For example:
--
-- * @'Demote' 'Bool' = 'Bool'@
-- * @'Demote' ('Maybe' a) = 'Maybe' ('Demote' a)@
-- * @'Demote' ('Either' a b) = 'Either' ('Demote' a) ('Demote' b)@
--
-- Rarely, the type and kind do not match. For example:
--
-- * @'Demote' (a '~>' b) = 'Demote' a -> 'Demote' b@
-- * @'Demote' Nat = Natural@
-- * @'Demote' Symbol = Text@
type family Demote (k :: Type) :: Type
-- | A generalized form of 'Demote' which can work over types of any kind.
-- @'DemoteX' (a :: k)@ is @'Demote' a@ when @k@ is @Type@.
-- For some non-@Type@ examples:
--
-- * @'DemoteX' 'True' = 'True'@
-- * @'DemoteX' ('Just' x) = 'Just' ('DemoteX' x)@
-- * @'DemoteX' ('TyCon1' ((:) a)) = (:) ('DemoteX' a)@
type family DemoteX (a :: k) :: Demote k

-- | A proposition which states that 'PromoteX' and 'DemoteX' are inverses.
-- This is a particularly valuable hint for type inference in typical
-- 'SingKind' instances for GADTs.
type PromoteDemoteInverse (a :: k) = (PromoteX (DemoteX a) ~~ a)

-- | @'SingKindC' a@ is @'SingKind' a@ when @a@ is a 'Type'. If @a@ is not
-- a 'Type', then @'SingKindC' a@ recurs into @a@, gathering up 'SingKind'
-- constraints when necessary. (For instance,
-- @'SingKindC' (x:xs) = ('SingKindC' x, 'SingKindC' xs)@.)
type family SingKindC (a :: k) :: Constraint
-- | A combination of 'SingKindC' and 'PromoteDemoteInverse'. Typically, a
-- 'SingKind' instance will have a 'SingKindX' constraint on each of the
-- type parameters to the data type receiving and instance.
-- (For example,
-- @instance ('SingKindX' a_1, ..., 'SingKindX' a_n) => 'SingKind' (D a_1 ... a_n)@.)
type SingKindX (a :: k) = (SingKindC a, PromoteDemoteInverse a)

-- | The 'SingKind' class is a /kind/ class. It classifies all kinds
-- for which singletons are defined. The class supports converting between a singleton
-- type and the base (unrefined) type which it is built from.
--
-- For a 'SingKind' instance to be well behaved, it should obey the following laws:
--
-- @
-- 'toSing' . 'fromSing' ≡ 'SomeSing'
-- (\\x -> 'withSomeSing' x 'fromSing') ≡ 'id'
-- @
--
-- The final law can also be expressed in terms of the 'FromSing' pattern
-- synonym:
--
-- @
-- (\\('FromSing' sing) -> 'FromSing' sing) ≡ 'id'
-- @
class PromoteDemoteInverse k => SingKind k where
  -- | Convert a singleton to its unrefined version.
  fromSing :: Sing (a :: k) -> Demote k
  -- | Convert an unrefined type to an existentially-quantified singleton type.
  toSing :: Demote k -> SomeSing k

{- $work_around_staging
Due to GHC Trac #12088, a naïve attempt at defining 'Demote'/'Promote' and
'DemoteX'/'PromoteX' instances all in one go will not succeed. For instance,
the following will fail to typecheck:

@
type instance 'Demote'  [a] = ['Demote'  a]
type instance 'Promote' [a] = ['Promote' a]

type instance 'DemoteX' \'[]   = \'[]
type instance 'DemoteX' (x:xs) = 'DemoteX' x : 'DemoteX' xs

type instance 'PromoteX' \'[]   = \'[]
type instance 'PromoteX' (x:xs) = 'PromoteX' x : 'PromoteX' xs
@

The errors will probably look something to the effect of:

@
    • Expected kind ‘Demote [k0]’,
        but ‘DemoteX x : DemoteX xs’ has kind ‘[Demote k0]’
    • In the type ‘DemoteX x : DemoteX xs’
      In the type instance declaration for ‘DemoteX’
@

Because of Trac #12088, the @'Demote' [k0]@ in the kind of the
@'DemoteX' (x:xs)@ instance does not reduce properly. Don't despair, though:
one can work around this issue by explicitly separating the two groups of
declarations using a Template Haskell splice:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;

type instance 'Demote'  [a] = ['Demote'  a]
type instance 'Promote' [a] = ['Promote' a]

$(return [])

type instance 'DemoteX' \'[]   = \'[]
type instance 'DemoteX' (x:xs) = 'DemoteX' x : 'DemoteX' xs

type instance 'PromoteX' \'[]   = \'[]
type instance 'PromoteX' (x:xs) = 'PromoteX' x : 'PromoteX' xs
@

It's ugly, but it works.
-}

-----
-- Useful combinators
-----

-- | Convert a normal datatype (like 'Bool') to a singleton for that datatype,
-- passing it into a continuation.
withSomeSing :: forall k r
              . SingKind k
             => Demote k                          -- ^ The original datatype
             -> (forall (a :: k). Sing a -> r)    -- ^ Function expecting a singleton
             -> r
withSomeSing x f =
  case toSing x of
    SomeSing x' -> f x'

-- | A convenience function that names a singleton satisfying a certain
-- property.  If the singleton does not satisfy the property, then the function
-- returns 'Nothing'. The property is expressed in terms of the underlying
-- representation of the singleton.
singThat :: forall (a :: k). (SingKind k, SingI a)
         => (Demote k -> Bool) -> Maybe (Sing a)
singThat p = withSing $ \x -> if p (fromSing x) then Just x else Nothing

-- | A convenience function that takes a type as input and demotes it to its
-- value-level counterpart as output. This uses 'SingKind' and 'SingI' behind
-- the scenes, so @'demote' = 'fromSing' 'sing'@.
--
-- This function is intended to be used with @TypeApplications@. For example:
--
-- >>> demote @True
-- True
--
-- >>> demote @(Nothing :: Maybe Ordering)
-- Nothing
demote :: forall a. (SingKind (KindOf a), SingI a) => Demote (KindOf a)
demote = fromSing (sing @a)

-- | An explicitly bidirectional pattern synonym for going between a
-- singleton and the corresponding demoted term.
--
-- As an __expression__: this takes a singleton to its demoted (base)
-- type.
--
-- >>> :t FromSing \@Bool
-- FromSing \@Bool :: Sing a -> Bool
-- >>> FromSing SFalse
-- False
--
-- As a __pattern__: It extracts a singleton from its demoted (base)
-- type.
--
-- @
-- singAnd :: 'Bool' -> 'Bool' -> 'SomeSing' 'Bool'
-- singAnd ('FromSing' singBool1) ('FromSing' singBool2) =
--   'SomeSing' (singBool1 %&& singBool2)
-- @
--
-- instead of writing it with 'withSomeSing':
--
-- @
-- singAnd bool1 bool2 =
--   'withSomeSing' bool1 $ \singBool1 ->
--     'withSomeSing' bool2 $ \singBool2 ->
--       'SomeSing' (singBool1 %&& singBool2)
-- @
pattern FromSing :: SingKind k => forall (a :: k). Sing a -> Demote k
pattern FromSing sng <- ((\demotedVal -> withSomeSing demotedVal SomeSing) -> SomeSing sng)
  where FromSing sng = fromSing sng

-----
-- Particularly important instances
-----

type instance Demote  Type = Type
type instance Promote Type = Type
type instance DemoteX   (a :: Type) = Demote a
type instance PromoteX  (a :: Type) = Promote a
type instance SingKindC (a :: Type) = SingKind a

type instance Demote  (a ~> b) = DemoteX  a -> DemoteX  b
type instance Promote (a -> b) = PromoteX a ~> PromoteX b
instance (SingKindX k1, SingKindX k2) => SingKind (k1 ~> k2) where
  fromSing sFun x = withSomeSing x $ fromSing . applySing sFun
  toSing f = SomeSing slam
    where
      slam :: forall (f :: k1 ~> k2). Sing f
      slam = singFun1 @f lam
        where
          lam :: forall (t :: k1). Sing t -> Sing (f @@ t)
          lam x = withSomeSing (f (fromSing x)) (\(r :: Sing res) -> unsafeCoerce r)

-----
-- Defunctionalization symbols
-----

$(genDefunSymbols [ ''Demote , ''Promote
                  -- Can't do these yet due to https://ghc.haskell.org/trac/ghc/ticket/12564
                  -- , ''DemoteX, ''PromoteX
                  , ''PromoteDemoteInverse, ''SingKindC, ''SingKindX
                  ])
