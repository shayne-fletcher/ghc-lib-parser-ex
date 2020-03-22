-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#include "ghclib_api.h"

module Language.Haskell.GhclibParserEx.GHC.Driver.Session(
      readExtension
    , extensionImplications
    -- Copied from DynFlags (see
    -- https://gitlab.haskell.org/ghc/ghc/merge_requests/2654).
    , TurnOnFlag, turnOn, turnOff, impliedGFlags, impliedOffGFlags, impliedXFlags
    --
    , parsePragmasIntoDynFlags
  ) where

import qualified GHC.LanguageExtensions as LangExt
import Panic
import HeaderInfo
import StringBuffer
#if defined(GHCLIB_API_811)
import GHC.Driver.Session
import GHC.Driver.Types
#else
import DynFlags
import HscTypes
#endif
import GHC.LanguageExtensions.Type
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as Map

-- Oprhan instance until
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/2707 lands.
instance Ord Extension where
  compare = compare `on` fromEnum

-- | Parse a GHC extension.
readExtension :: String -> Maybe Extension
readExtension s = flagSpecFlag <$> find (\(FlagSpec n _ _ _) -> n == s) xFlags

-- | Implicitly enabled/disabled extensions.
extensionImplications :: [(Extension, ([Extension], [Extension]))]
extensionImplications = map f $ Map.toList implicationsMap
  where
    f (e, ps) = (fromJust (readExtension e), ps)
    implicationsMap :: Map.Map String ([Extension], [Extension])
    implicationsMap = Map.fromListWith (<>)
      [(show a, ([c | b], [c | not b]))
        | (a, flag, c) <- impliedXFlags, let b = flag == turnOn]

-- Copied from 'ghc/compiler/main/DynFlags.hs'.

type TurnOnFlag = Bool   -- True  <=> we are turning the flag on
                         -- False <=> we are turning the flag off
turnOn  :: TurnOnFlag; turnOn  = True
turnOff :: TurnOnFlag; turnOff = False

-- General flags that are switched on/off when other general flags are switched
-- on
impliedGFlags :: [(GeneralFlag, TurnOnFlag, GeneralFlag)]
impliedGFlags = [(Opt_DeferTypeErrors, turnOn, Opt_DeferTypedHoles)
                ,(Opt_DeferTypeErrors, turnOn, Opt_DeferOutOfScopeVariables)
                ,(Opt_Strictness, turnOn, Opt_WorkerWrapper)
                ,(Opt_UnclutterValidHoleFits, turnOff, Opt_ShowTypeAppOfHoleFits)
                ,(Opt_UnclutterValidHoleFits, turnOff, Opt_ShowTypeAppVarsOfHoleFits)
                ,(Opt_UnclutterValidHoleFits, turnOff, Opt_ShowDocsOfHoleFits)
                ,(Opt_ShowTypeAppVarsOfHoleFits, turnOff, Opt_ShowTypeAppOfHoleFits)
                ,(Opt_UnclutterValidHoleFits, turnOff, Opt_ShowProvOfHoleFits)]

-- General flags that are switched on/off when other general flags are switched
-- off
impliedOffGFlags :: [(GeneralFlag, TurnOnFlag, GeneralFlag)]
impliedOffGFlags = [(Opt_Strictness, turnOff, Opt_WorkerWrapper)]

impliedXFlags :: [(LangExt.Extension, TurnOnFlag, LangExt.Extension)]
impliedXFlags
-- See Note [Updating flag description in the User's Guide]
  = [ (LangExt.RankNTypes,                turnOn, LangExt.ExplicitForAll)
    , (LangExt.QuantifiedConstraints,     turnOn, LangExt.ExplicitForAll)
    , (LangExt.ScopedTypeVariables,       turnOn, LangExt.ExplicitForAll)
    , (LangExt.LiberalTypeSynonyms,       turnOn, LangExt.ExplicitForAll)
    , (LangExt.ExistentialQuantification, turnOn, LangExt.ExplicitForAll)
    , (LangExt.FlexibleInstances,         turnOn, LangExt.TypeSynonymInstances)
    , (LangExt.FunctionalDependencies,    turnOn, LangExt.MultiParamTypeClasses)
    , (LangExt.MultiParamTypeClasses,     turnOn, LangExt.ConstrainedClassMethods)  -- c.f. #7854
    , (LangExt.TypeFamilyDependencies,    turnOn, LangExt.TypeFamilies)

    , (LangExt.RebindableSyntax, turnOff, LangExt.ImplicitPrelude)      -- NB: turn off!

    , (LangExt.DerivingVia, turnOn, LangExt.DerivingStrategies)

    , (LangExt.GADTs,            turnOn, LangExt.GADTSyntax)
    , (LangExt.GADTs,            turnOn, LangExt.MonoLocalBinds)
    , (LangExt.TypeFamilies,     turnOn, LangExt.MonoLocalBinds)

    , (LangExt.TypeFamilies,     turnOn, LangExt.KindSignatures)  -- Type families use kind signatures
    , (LangExt.PolyKinds,        turnOn, LangExt.KindSignatures)  -- Ditto polymorphic kinds

    -- TypeInType is now just a synonym for a couple of other extensions.
    , (LangExt.TypeInType,       turnOn, LangExt.DataKinds)
    , (LangExt.TypeInType,       turnOn, LangExt.PolyKinds)
    , (LangExt.TypeInType,       turnOn, LangExt.KindSignatures)

#if defined(GHCLIB_API_811) || defined(GHCLIB_API_810)
    -- Standalone kind signatures are a replacement for CUSKs.
    , (LangExt.StandaloneKindSignatures, turnOff, LangExt.CUSKs)
#endif

    -- AutoDeriveTypeable is not very useful without DeriveDataTypeable
    , (LangExt.AutoDeriveTypeable, turnOn, LangExt.DeriveDataTypeable)

    -- We turn this on so that we can export associated type
    -- type synonyms in subordinates (e.g. MyClass(type AssocType))
    , (LangExt.TypeFamilies,     turnOn, LangExt.ExplicitNamespaces)
    , (LangExt.TypeOperators, turnOn, LangExt.ExplicitNamespaces)

    , (LangExt.ImpredicativeTypes,  turnOn, LangExt.RankNTypes)

        -- Record wild-cards implies field disambiguation
        -- Otherwise if you write (C {..}) you may well get
        -- stuff like " 'a' not in scope ", which is a bit silly
        -- if the compiler has just filled in field 'a' of constructor 'C'
    , (LangExt.RecordWildCards,     turnOn, LangExt.DisambiguateRecordFields)

    , (LangExt.ParallelArrays, turnOn, LangExt.ParallelListComp)

    , (LangExt.JavaScriptFFI, turnOn, LangExt.InterruptibleFFI)

    , (LangExt.DeriveTraversable, turnOn, LangExt.DeriveFunctor)
    , (LangExt.DeriveTraversable, turnOn, LangExt.DeriveFoldable)

    -- Duplicate record fields require field disambiguation
    , (LangExt.DuplicateRecordFields, turnOn, LangExt.DisambiguateRecordFields)

    , (LangExt.TemplateHaskell, turnOn, LangExt.TemplateHaskellQuotes)
    , (LangExt.Strict, turnOn, LangExt.StrictData)
  ]

--

parsePragmasIntoDynFlags :: DynFlags
                         -> ([Extension], [Extension])
                         -> FilePath
                         -> String
                         -> IO (Either String DynFlags)
parsePragmasIntoDynFlags flags (enable, disable) file str =
  catchErrors $ do
    let opts = getOptions flags (stringToStringBuffer str) file
    (flags, _, _) <- parseDynamicFilePragma flags opts
    let flags' =  foldl' xopt_set flags enable
    let flags'' = foldl' xopt_unset flags' disable
    return $ Right (flags'' `gopt_set` Opt_KeepRawTokenStream)
  where
    catchErrors :: IO (Either String DynFlags) -> IO (Either String DynFlags)
    catchErrors act = handleGhcException reportErr
                        (handleSourceError reportErr act)
    reportErr e = return $ Left (show e)
