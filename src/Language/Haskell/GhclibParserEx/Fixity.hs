-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.
--
-- Adapted from (1) https://github.com/mpickering/apply-refact.git and
-- (2) https://gitlab.haskell.org/ghc/ghc ('compiler/renamer/RnTypes.hs').

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
#include "ghclib_api.h"

module Language.Haskell.GhclibParserEx.Fixity(
    applyFixities
  , fixitiesFromModule
  , preludeFixities, baseFixities
  , infixr_, infixl_, infix_, fixity
  ) where

#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902) || defined (GHCLIB_API_900)
import GHC.Hs
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
import GHC.Types.Fixity
import GHC.Types.SourceText
#else
import GHC.Types.Basic
#endif
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.SrcLoc
#elif defined (GHCLIB_API_810)
import GHC.Hs
import BasicTypes
import RdrName
import OccName
import SrcLoc
#else
import HsSyn
import BasicTypes
import RdrName
import OccName
import SrcLoc
#endif
import Data.Maybe
import Data.Data hiding (Fixity)
import Data.Generics.Uniplate.Data

#if defined (GHCLIB_API_900) || defined (GHCLIB_API_810)
noExt :: NoExtField
noExt = noExtField
#endif

-- | Rearrange a parse tree to account for fixities.
applyFixities :: Data a => [(String, Fixity)] -> a -> a
applyFixities fixities m =
  let m'  = transformBi (expFix fixities) m
      m'' = transformBi (patFix fixities) m'
  in m''

expFix :: [(String, Fixity)] -> LHsExpr GhcPs -> LHsExpr GhcPs
expFix fixities (L loc (OpApp _ l op r)) =
  mkOpApp (getFixities fixities) loc l op (findFixity (getFixities fixities) op) r
expFix _ e = e

-- LPat and Pat have gone through a lot of churn. See
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/1925 for details.
patFix :: [(String, Fixity)] -> LPat GhcPs -> LPat GhcPs
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902) || defined (GHCLIB_API_900)
patFix fixities (L loc (ConPat _ op (InfixCon pat1 pat2))) =
  L loc (mkConOpPat (getFixities fixities) op (findFixity' (getFixities fixities) op) pat1 pat2)
#elif defined (GHCLIB_API_810)
patFix fixities (L loc (ConPatIn op (InfixCon pat1 pat2))) =
  L loc (mkConOpPat (getFixities fixities) op (findFixity' (getFixities fixities) op) pat1 pat2)
#else
patFix fixities (dL -> L _ (ConPatIn op (InfixCon pat1 pat2))) =
  mkConOpPat (getFixities fixities) op (findFixity' (getFixities fixities) op) pat1 pat2
#endif
patFix _ p = p

mkConOpPat ::
  [(String, Fixity)]
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
  -> LocatedN RdrName
#else
  -> Located RdrName
#endif
  -> Fixity
  -> LPat GhcPs -> LPat GhcPs
  -> Pat GhcPs
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902) || defined (GHCLIB_API_900)
mkConOpPat fs op2 fix2 p1@(L loc (ConPat _ op1 (InfixCon p11 p12))) p2
#elif defined (GHCLIB_API_810)
mkConOpPat fs op2 fix2 p1@(L loc (ConPatIn op1 (InfixCon p11 p12))) p2
#else
mkConOpPat fs op2 fix2 p1@(dL->L loc (ConPatIn op1 (InfixCon p11 p12))) p2
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
  | nofix_error = ConPat noAnn op2 (InfixCon p1 p2)
#elif defined (GHCLIB_API_900)
  | nofix_error = ConPat noExtField op2 (InfixCon p1 p2)
#else
  | nofix_error = ConPatIn op2 (InfixCon p1 p2)
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
  | associate_right = ConPat noAnn op1 (InfixCon p11 (L loc (mkConOpPat fs op2 fix2 p12 p2)))
#elif defined (GHCLIB_API_900)
  | associate_right = ConPat noExtField op1 (InfixCon p11 (L loc (mkConOpPat fs op2 fix2 p12 p2)))
#elif defined (GHCLIB_API_810)
  | associate_right = ConPatIn op1 (InfixCon p11 (L loc (mkConOpPat fs op2 fix2 p12 p2)))
#else
  | associate_right = ConPatIn op1 (InfixCon p11 (cL loc (mkConOpPat fs op2 fix2 p12 p2)))
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
  | otherwise = ConPat noAnn op2 (InfixCon p1 p2)
#elif defined (GHCLIB_API_900)
  | otherwise = ConPat noExtField op2 (InfixCon p1 p2)
#else
  | otherwise = ConPatIn op2 (InfixCon p1 p2)
#endif
  where
    fix1 = findFixity' fs op1
    (nofix_error, associate_right) = compareFixity fix1 fix2
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
mkConOpPat _ op _ p1 p2 = ConPat noAnn op (InfixCon p1 p2)
#elif defined (GHCLIB_API_900)
mkConOpPat _ op _ p1 p2 = ConPat noExtField op (InfixCon p1 p2)
#else
mkConOpPat _ op _ p1 p2 = ConPatIn op (InfixCon p1 p2)
#endif

mkOpApp ::
   [(String, Fixity)]
#if defined(GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
   -> SrcSpanAnnA
#else
   -> SrcSpan
#endif
   -> LHsExpr GhcPs -- Left operand; already rearrange.
   -> LHsExpr GhcPs -> Fixity -- Operator and fixity.
   -> LHsExpr GhcPs -- Right operand (not an OpApp, but might be a NegApp).
   -> LHsExpr GhcPs
--      (e11 `op1` e12) `op2` e2
mkOpApp fs loc e1@(L _ (OpApp x1 e11 op1 e12)) op2 fix2 e2
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
  | nofix_error = L loc (OpApp noAnn e1 op2 e2)
#else
  | nofix_error = L loc (OpApp noExt e1 op2 e2)
#endif
  | associate_right = L loc (OpApp x1 e11 op1 (mkOpApp fs loc' e12 op2 fix2 e2 ))
  where
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
    loc'= combineLocsA e12 e2
#else
    loc'= combineLocs e12 e2
#endif
    fix1 = findFixity fs op1
    (nofix_error, associate_right) = compareFixity fix1 fix2
--      (- neg_arg) `op` e2
mkOpApp fs loc e1@(L _ (NegApp _ neg_arg neg_name)) op2 fix2 e2
#if defined(GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
  | nofix_error = L loc (OpApp noAnn e1 op2 e2)
#else
  | nofix_error = L loc (OpApp noExt e1 op2 e2)
#endif
#if defined(GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
  | associate_right = L loc (NegApp noAnn (mkOpApp fs loc' neg_arg op2 fix2 e2) neg_name)
#else
  | associate_right = L loc (NegApp noExt (mkOpApp fs loc' neg_arg op2 fix2 e2) neg_name)
#endif
  where
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
    loc' = combineLocsA neg_arg e2
#else
    loc' = combineLocs neg_arg e2
#endif
    (nofix_error, associate_right) = compareFixity negateFixity fix2
--      e1 `op` - neg_arg
mkOpApp _ loc e1 op1 fix1 e2@(L _ NegApp {}) -- NegApp can occur on the right.
#if defined(GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
  | not associate_right  = L loc (OpApp noAnn e1 op1 e2)-- We *want* right association.
#else
  | not associate_right  = L loc (OpApp noExt e1 op1 e2)-- We *want* right association.
#endif
  where
    (_, associate_right) = compareFixity fix1 negateFixity
 --     Default case, no rearrangment.
#if defined(GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
mkOpApp _ loc e1 op _fix e2 = L loc (OpApp noAnn e1 op e2)
#else
mkOpApp _ loc e1 op _fix e2 = L loc (OpApp noExt e1 op e2)
#endif

getIdent :: LHsExpr GhcPs -> String
getIdent (unLoc -> HsVar _ (L _ n)) = occNameString . rdrNameOcc $ n
getIdent _ = error "Must be HsVar"

-- If there are no fixities, give 'baseFixities'.
getFixities :: [(String, Fixity)] -> [(String, Fixity)]
getFixities fixities = if null fixities then baseFixities else fixities

findFixity :: [(String, Fixity)] -> LHsExpr GhcPs -> Fixity
findFixity fs r = askFix fs (getIdent r) -- Expressions.

#if defined(GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
findFixity' :: [(String, Fixity)] -> LocatedN RdrName -> Fixity
#else
findFixity' :: [(String, Fixity)] -> Located RdrName -> Fixity
#endif
findFixity' fs r = askFix fs (occNameString . rdrNameOcc . unLoc $ r) -- Patterns.

askFix :: [(String, Fixity)] -> String -> Fixity
askFix xs = \k -> lookupWithDefault defaultFixity k xs
  where lookupWithDefault def_v k mp1 = fromMaybe def_v $ lookup k mp1

-- All fixities defined in the Prelude.
preludeFixities :: [(String, Fixity)]
preludeFixities = concat
    [ infixr_ 9  ["."]
    , infixl_ 9  ["!!"]
    , infixr_ 8  ["^","^^","**"]
    , infixl_ 7  ["*","/","quot","rem","div","mod",":%","%"]
    , infixl_ 6  ["+","-"]
    , infixr_ 5  [":","++"]
    , infix_  4  ["==","/=","<","<=",">=",">","elem","notElem"]
    , infixr_ 3  ["&&"]
    , infixr_ 2  ["||"]
    , infixl_ 1  [">>",">>="]
    , infixr_ 1  ["=<<"]
    , infixr_ 0  ["$","$!","seq"]
    ]

-- All fixities defined in the base package. Note that the @+++@
-- operator appears in both Control.Arrows and
-- Text.ParserCombinators.ReadP. The listed precedence for @+++@ in
-- this list is that of Control.Arrows.
baseFixities :: [(String, Fixity)]
baseFixities = preludeFixities ++ concat
    [ infixr_ 9 ["Compose"]
    , infixl_ 9 ["!","//","!:"]
    , infixl_ 8 ["shift","rotate","shiftL","shiftR","rotateL","rotateR"]
    , infixl_ 7 [".&."]
    , infixl_ 6 ["xor"]
    , infix_  6 [":+"]
    , infixr_ 6 ["<>"]
    , infixl_ 5 [".|."]
    , infixr_ 5 ["+:+","<++","<+>","<|"] -- Fixity conflict for +++ between ReadP and Arrow.
    , infix_  5 ["\\\\"]
    , infixl_ 4 ["<$>","<$","$>","<*>","<*","*>","<**>","<$!>"]
    , infix_  4 ["elemP","notElemP",":~:", ":~~:"]
    , infixl_ 3 ["<|>"]
    , infixr_ 3 ["&&&","***"]
    , infixr_ 2 ["+++","|||"]
    , infixr_ 1 ["<=<",">=>",">>>","<<<","^<<","<<^","^>>",">>^"]
    , infixl_ 0 ["on"]
    , infixr_ 0 ["par","pseq"]
    ]

infixr_, infixl_, infix_ :: Int -> [String] -> [(String,Fixity)]
infixr_ = fixity InfixR
infixl_ = fixity InfixL
infix_  = fixity InfixN

fixity :: FixityDirection -> Int -> [String] -> [(String, Fixity)]
fixity a p = map (,Fixity (SourceText "") p a)

#if defined (GHCLIB_API_904) || defined(GHCLIB_API_902) || defined (GHCLIB_API_900)
fixitiesFromModule :: Located HsModule -> [(String, Fixity)]
#else
fixitiesFromModule :: Located (HsModule GhcPs) -> [(String, Fixity)]
#endif
#if defined (GHCLIB_API_HEAD)
fixitiesFromModule (L _ (HsModule _ _ _ _ decls)) = concatMap f decls
#elif defined (GHCLIB_API_904) || defined(GHCLIB_API_902)
fixitiesFromModule (L _ (HsModule _ _ _ _ _ decls _ _)) = concatMap f decls
#elif defined (GHCLIB_API_900)
fixitiesFromModule (L _ (HsModule _ _ _ _ decls _ _)) = concatMap f decls
#else
fixitiesFromModule (L _ (HsModule _ _ _ decls _ _)) = concatMap f decls
#endif
  where
    f :: LHsDecl GhcPs -> [(String, Fixity)]
    f (L _ (SigD _ (FixSig _ (FixitySig _ ops (Fixity _ p dir))))) =
          fixity dir p (map (occNameString. rdrNameOcc . unLoc) ops)
    f _ = []
