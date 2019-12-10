{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module:      Data.Singletons.GADT.Prelude
Copyright:   (C) 2018 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Defines Template Haskell functions for quickly generating instances of
'SingKind' (and supporting type families).
-}
module Data.Singletons.GADT.TH (
    -- $working_around_staging
    -- * Reimplementations of functionality from "Data.Singletons.TH"
    genSingletons
  , genSingletons1
  , singletons
  , singletons1
  , singletonsOnly
  , singletonsOnly1

    -- * Generating instances of classes/families in "Data.Singletons.GADT"
  , genSingKindInsts
  , genSingKindInsts1
  , genSingKindInsts2

    -- * "Data.Singletons.GADT" reexports
  , module Data.Singletons.GADT

    -- * The rest of "Data.Singletons.TH"
  , module Data.Singletons.TH
  ) where

import           Control.Monad

import           Data.Foldable
import           Data.Maybe
import qualified Data.Singletons as Old (SingKind)
import           Data.Singletons.GADT
import qualified Data.Singletons.TH as TH
import           Data.Singletons.TH.Options hiding (genSingKindInsts)
import           Data.Singletons.TH hiding
                   ( genSingletons, singletons, singletonsOnly
                   , SingKind(..), DemoteSym0, DemoteSym1, FromSing
                   , demote, singThat, withSomeSing )
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Desugar

{- $working_around_staging
As "Data.Singletons.GADT" explains, one often has to work around issues
caused by GHC Trac #12088. To make this task somewhat easier, the functions
in "Data.Singletons.GADT.TH" all have variants which only generate certain
subsets of instances so that one can predictably sequence Template Haskell
splices in the right order to avoid #12088.

This is all better explained by example. If you wanted to generate the
following instances using Template Haskell.

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

Then one should /not/ do the following:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
$('genSingletons' [''[]])
@

As this will generate all of the above instances at once, without any explicit
separation. Instead, one should do the following:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
$('genSingletons1' [''[]])
$('genSingletons2' [''[]])
@

'genSingletons1' will first generate the 'Demote'/'Promote' instances, within
a single declaration group, and then the next declaration group generated by
'genSingletons2' will contain the 'DemoteX'/'PromoteX' instances.

'genSingletons' is still provided in the event that Trac #12088 gets fixed one
day, but chances are, it won't be very useful to you.
-}

-----
-- Reimplementations of Data.Singletons.TH functionality
-----

-- | Generate singleton definitions from a type that is already defined.
-- For example, the singletons package itself uses
--
-- > $(genSingletons [''Bool, ''Maybe, ''Either, ''[]])
--
-- to generate singletons for Prelude types.
genSingletons :: OptionsMonad q => [Name] -> q [Dec]
genSingletons names = do
  (decs1, decs2) <- genSingletons' names
  pure $ decs1 ++ decs2

-- | Like 'genSingletons', but only generates 'Demote', 'Promote', and
-- 'SingKind' instances.
genSingletons1 :: OptionsMonad q => [Name] -> q [Dec]
genSingletons1 = fmap fst . genSingletons'

genSingletons' :: OptionsMonad q => [Name] -> q ([Dec], [Dec])
genSingletons' names = do
  oldDecs <- filterOutOldSingKind <$> TH.genSingletons names
  (newDecs1, newDecs2) <- genSingKindInsts' names
  pure (oldDecs ++ newDecs1, newDecs2)

-- | Make promoted and singleton versions of all declarations given, retaining
-- the original declarations.
-- See <https://github.com/goldfirere/singletons/blob/master/README.md> for
-- further explanation.
singletons :: OptionsMonad q => q [Dec] -> q [Dec]
singletons qdecs = do
  (decs1, decs2) <- singletons' TH.singletons qdecs
  pure $ decs1 ++ decs2

-- | Like 'singletons', but only generates 'Demote', 'Promote', and
-- 'SingKind' instances.
singletons1 :: OptionsMonad q => q [Dec] -> q [Dec]
singletons1 = fmap fst . singletons' TH.singletons

-- | Make promoted and singleton versions of all declarations given, discarding
-- the original declarations. Note that a singleton based on a datatype needs
-- the original datatype, so this will fail if it sees any datatype declarations.
-- Classes, instances, and functions are all fine.
singletonsOnly :: OptionsMonad q => q [Dec] -> q [Dec]
singletonsOnly qdecs = do
  (decs1, decs2) <- singletons' TH.singletonsOnly qdecs
  pure $ decs1 ++ decs2

-- | Like 'singletonsOnly', but only generates 'Demote', 'Promote', and
-- 'SingKind' instances.
singletonsOnly1 :: OptionsMonad q => q [Dec] -> q [Dec]
singletonsOnly1 = fmap fst . singletons' TH.singletonsOnly

singletons' :: OptionsMonad q => (q [Dec] -> q [Dec]) -> q [Dec] -> q ([Dec], [Dec])
singletons' genSings qdecs = do
  decs <- qdecs
  oldDecs <- filterOutOldSingKind <$> genSings (pure decs)
  (newDecs1, newDecs2) <- singDecs decs
  pure (oldDecs ++ decsToTH newDecs1, decsToTH newDecs2)

-----
-- Macros for generating instances of classes/families from Data.Singletons.GADT
-----

-- | Generate 'Demote', 'Promote', 'SingKind', 'DemoteX', 'PromoteX', and
-- 'SingKindC' instances for each provided data type. Ignores non–data type
-- declarations.
genSingKindInsts :: OptionsMonad q => [Name] -> q [Dec]
genSingKindInsts names = do
  (decs1, decs2) <- genSingKindInsts' names
  pure $ decs1 ++ decs2

-- | Like 'genSingKindInsts', but only generates 'Demote', 'Promote', and
-- 'SingKind' instances. Ignores non–data type declarations.
genSingKindInsts1 :: OptionsMonad q => [Name] -> q [Dec]
genSingKindInsts1 = fmap fst . genSingKindInsts'

-- | Like 'genSingKindInsts', but only generates 'DemoteX', 'PromoteX', and
-- 'SingKindC' instances. Ignores non–data type declarations.
genSingKindInsts2 :: OptionsMonad q => [Name] -> q [Dec]
genSingKindInsts2 = fmap snd . genSingKindInsts'

genSingKindInsts' :: OptionsMonad q => [Name] -> q ([Dec], [Dec])
genSingKindInsts' names = do
  (decs1, decs2) <- concatMapM (singInfo <=< dsInfo <=< reifyWithLocals) names
  pure (decsToTH decs1, decsToTH decs2)

-----
-- Internals
-----

singDecs :: OptionsMonad q => [Dec] -> q ([DDec], [DDec])
singDecs decs = do
  ddecs <- withLocalDeclarations decs $ dsDecs decs
  (ddecss1, ddecss2) <- mapAndUnzipM singDec ddecs
  pure (concat ddecss1, concat ddecss2)

singInfo :: OptionsMonad q => DInfo -> q ([DDec], [DDec])
singInfo (DTyConI dec _) = singDec dec
singInfo _               = pure ([], [])

singDec :: OptionsMonad q => DDec -> q ([DDec], [DDec])
singDec (DDataD _nd _cxt name tvbs mk cons _derivings)
  = do all_tvbs <- buildDataDTvbs tvbs mk
       singDataD name all_tvbs cons
singDec _ = pure ([], [])

-- Generates all SingKind-related declarations for a given data type,
-- separating the declarations into two groups (as described above) to
-- avoid Template Haskell staging restrictions.
--
-- This is largely cargo-culted from singDataD in the singletons library.
singDataD :: forall q. OptionsMonad q
          => Name -> [DTyVarBndr] -> [DCon]
          -> q ([DDec], [DDec])
singDataD name tvbs ctors = do
  let tvbNames = map extractTvbName tvbs
      tvbTys   = map DVarT tvbNames
      dataType = foldType (DConT name) tvbTys

  -- Demote, Promote instances
  let mkPromoteDemoteInstance :: Name -> Name -> DDec
      mkPromoteDemoteInstance cls clsX =
        DTySynInstD $ DTySynEqn Nothing (DConT cls `DAppT` dataType)
                    $ foldType (DConT name)
                    $ map (DConT clsX `DAppT`) tvbTys

      demoteInstance  = mkPromoteDemoteInstance ''Demote  ''DemoteX
      promoteInstance = mkPromoteDemoteInstance ''Promote ''PromoteX

  -- SingKind instance
  fromSingClauses     <- mapM mkFromSingClause ctors
  emptyFromSingClause <- mkEmptyFromSingClause
  toSingClauses       <- mapM mkToSingClause ctors
  emptyToSingClause   <- mkEmptyToSingClause

  let singKindInstance =
        DInstanceD Nothing Nothing
                   (map (DAppT (DConT ''SingKindX)) tvbTys)
                   (DAppT (DConT ''SingKind) dataType)
                   [ DLetDec $ DFunD 'fromSing
                             $ fromSingClauses `orIfEmpty` [emptyFromSingClause]
                   , DLetDec $ DFunD 'toSing
                             $ toSingClauses   `orIfEmpty` [emptyToSingClause]
                   ]

  -- DemoteX, PromoteX, SingKindC instances
  -- TODO RGS: Instances for TyConN
  let mkPromoteXDemoteXInstances :: Name -> q [DDec]
      mkPromoteXDemoteXInstances clsX = mkConInstances clsX foldType

      mkSingKindCInstance :: q [DDec]
      mkSingKindCInstance = mkConInstances ''SingKindC (const mkTupleDType)

      mkConInstances :: Name
                     -> (DType -> [DType] -> DType)
                     -> q [DDec]
      mkConInstances cls foldArgs = traverse mkInst ctors
        where
          mkInst :: DCon -> q DDec
          mkInst c = do
            let (cname, numArgs) = extractNameArgs c
            varNames <- replicateM numArgs (qNewName "x")
            let varTys = map DVarT varNames
            pure $ DTySynInstD
                 $ DTySynEqn Nothing (DConT cls `DAppT` foldType (DConT cname) varTys)
                 $ foldArgs (DConT cname)
                 $ map (DConT cls `DAppT`) varTys

  demoteXInstances   <- mkPromoteXDemoteXInstances ''DemoteX
  promoteXInstances  <- mkPromoteXDemoteXInstances ''PromoteX
  singKindCInstances <- mkSingKindCInstance

  pure ( [demoteInstance, promoteInstance, singKindInstance]
       , demoteXInstances ++ promoteXInstances ++ singKindCInstances
       )
  where
    mkFromSingClause :: DCon -> q DClause
    mkFromSingClause c = do
      opts <- getOptions
      let (cname, numArgs) = extractNameArgs c
      varNames <- replicateM numArgs (qNewName "b")
      return $ DClause [DConP (singledDataConName opts cname) (map DVarP varNames)]
                       (foldExp
                          (DConE cname)
                          (map (DAppE (DVarE 'fromSing) . DVarE) varNames))

    mkToSingClause :: DCon -> q DClause
    mkToSingClause (DCon _tvbs _cxt cname fields _rty) = do
      opts <- getOptions
      let types = tysOfConFields fields
      varNames  <- mapM (const $ qNewName "b") types
      svarNames <- mapM (const $ qNewName "c") types
      -- promoted  <- mapM promoteType types
      let promoted       = types
          varPats        = zipWith mkToSingVarPat varNames promoted
          recursiveCalls = zipWith mkRecursiveCall varNames promoted
      return $
        DClause [DConP cname varPats]
                (multiCase recursiveCalls
                           (map (DConP 'SomeSing . pure . DVarP)
                                svarNames)
                           (DAppE (DConE 'SomeSing)
                                     (foldExp (DConE (singledDataConName opts cname))
                                              (map DVarE svarNames))))

    mkToSingVarPat :: Name -> DKind -> DPat
    mkToSingVarPat varName _ki =
      -- DSigP (DVarP varName) (DAppT (DConT ''Demote) ki)
      DVarP varName

    mkRecursiveCall :: Name -> DKind -> DExp
    mkRecursiveCall var_name _ki =
      {-
      DSigE (DAppE (DVarE 'toSing) (DVarE var_name))
            (DAppT (DConT ''SomeSing) ki)
      -}
      DAppE (DVarE 'toSing) (DVarE var_name)

    mkEmptyFromSingClause :: q DClause
    mkEmptyFromSingClause = do
      x <- qNewName "x"
      pure $ DClause [DVarP x]
           $ DCaseE (DVarE x) []

    mkEmptyToSingClause :: q DClause
    mkEmptyToSingClause = do
      x <- qNewName "x"
      pure $ DClause [DVarP x]
           $ DConE 'SomeSing `DAppE` DCaseE (DVarE x) []

-- Discard any type class instances for SingKind (from the singletons library).
filterOutOldSingKind :: [Dec] -> [Dec]
filterOutOldSingKind = filter (not . isSingKind)
  where
    -- If it's stupid but it works, then it ain't stupid.
    isSingKind :: Dec -> Bool
    isSingKind (InstanceD _ _ instTy _)
      | AppT (ConT n) _ <- instTy
      = n == ''Old.SingKind
    isSingKind _ = False

-- | Make a constraint tuple 'DType' from a list of 'DType's. Avoids using a 1-tuple.
mkTupleDType :: [DType] -> DType
mkTupleDType [ty] = ty
mkTupleDType tys  = foldl DAppT (DConT $ tupleTypeName (length tys)) tys

----------------------------------------
-- Taken from singletons' source code --
----------------------------------------

-- build a pattern match over several expressions, each with only one pattern
multiCase :: [DExp] -> [DPat] -> DExp -> DExp
multiCase [] [] body = body
multiCase scruts pats body =
  DCaseE (mkTupleDExp scruts) [DMatch (mkTupleDPat pats) body]

tysOfConFields :: DConFields -> [DType]
tysOfConFields (DNormalC _ stys) = map snd stys
tysOfConFields (DRecC vstys) = map (\(_,_,ty) -> ty) vstys

-- extract the name and number of arguments to a constructor
extractNameArgs :: DCon -> (Name, Int)
extractNameArgs = fmap length . extractNameTypes

-- extract the name and types of constructor arguments
extractNameTypes :: DCon -> (Name, [DType])
extractNameTypes (DCon _ _ n fields _) = (n, tysOfConFields fields)

-- Construct a data type's variable binders, possibly using fresh variables
-- from the data type's kind signature.
buildDataDTvbs :: DsMonad q => [DTyVarBndr] -> Maybe DKind -> q [DTyVarBndr]
buildDataDTvbs tvbs mk = do
  extra_tvbs <- mkExtraDKindBinders $ fromMaybe (DConT typeKindName) mk
  pure $ tvbs ++ extra_tvbs

-- extract the name from a TyVarBndr.
extractTvbName :: DTyVarBndr -> Name
extractTvbName (DPlainTV n) = n
extractTvbName (DKindedTV n _) = n

-- apply an expression to a list of expressions
foldExp :: DExp -> [DExp] -> DExp
foldExp = foldl DAppE

-- apply a type to a list of types
foldType :: DType -> [DType] -> DType
foldType = foldl DAppT

-- choose the first non-empty list
orIfEmpty :: [a] -> [a] -> [a]
orIfEmpty [] x = x
orIfEmpty x _ = x

-- lift concatMap into a monad
-- could this be more efficient?
concatMapM :: (Monad monad, Monoid monoid, Traversable t)
           => (a -> monad monoid) -> t a -> monad monoid
concatMapM fn list = do
  bss <- mapM fn list
  return $ fold bss
