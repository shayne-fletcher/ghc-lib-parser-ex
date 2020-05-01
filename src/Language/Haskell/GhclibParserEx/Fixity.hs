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
  , preludeFixities, baseFixities
  , infixr_, infixl_, infix_, fixity
  ) where

#if defined (GHCLIB_API_811)
import GHC.Types.Basic
#else
import BasicTypes
#endif
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
import GHC.Hs
#else
import HsSyn
#endif
#if defined (GHCLIB_API_811)
import GHC.Types.Name.Reader
#else
import RdrName
#endif
#if defined (GHCLIB_API_811)
import GHC.Types.Name
#else
import OccName
#endif
#if defined (GHCLIB_API_811)
import GHC.Types.SrcLoc
#else
import SrcLoc
#endif
import Data.Maybe
import Data.Data hiding (Fixity)
import Data.Generics.Uniplate.Data

#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
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
#if defined (GHCLIB_API_811)
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
  -> Located RdrName -> Fixity
  -> LPat GhcPs -> LPat GhcPs
  -> Pat GhcPs
#if defined (GHCLIB_API_811)
mkConOpPat fs op2 fix2 p1@(L loc (ConPat _ op1 (InfixCon p11 p12))) p2
#elif defined (GHCLIB_API_810)
mkConOpPat fs op2 fix2 p1@(L loc (ConPatIn op1 (InfixCon p11 p12))) p2
#else
mkConOpPat fs op2 fix2 p1@(dL->L loc (ConPatIn op1 (InfixCon p11 p12))) p2
#endif
#if defined (GHCLIB_API_811)
  | nofix_error = ConPat noExtField op2 (InfixCon p1 p2)
#else
  | nofix_error = ConPatIn op2 (InfixCon p1 p2)
#endif
#if defined (GHCLIB_API_811)
  | associate_right = ConPat noExtField op1 (InfixCon p11 (L loc (mkConOpPat fs op2 fix2 p12 p2)))
#elif defined (GHCLIB_API_810)
  | associate_right = ConPatIn op1 (InfixCon p11 (L loc (mkConOpPat fs op2 fix2 p12 p2)))
#else
  | associate_right = ConPatIn op1 (InfixCon p11 (cL loc (mkConOpPat fs op2 fix2 p12 p2)))
#endif
#if defined (GHCLIB_API_811)
  | otherwise = ConPat noExtField op2 (InfixCon p1 p2)
#else
  | otherwise = ConPatIn op2 (InfixCon p1 p2)
#endif
  where
    fix1 = findFixity' fs op1
    (nofix_error, associate_right) = compareFixity fix1 fix2
#if defined (GHCLIB_API_811)
mkConOpPat _ op _ p1 p2 = ConPat noExtField op (InfixCon p1 p2)
#else
mkConOpPat _ op _ p1 p2 = ConPatIn op (InfixCon p1 p2)
#endif

mkOpApp ::
  [(String, Fixity)]
  -> SrcSpan
  -> LHsExpr GhcPs -- Left operand; already rearrange.
  -> LHsExpr GhcPs -> Fixity -- Operator and fixity.
  -> LHsExpr GhcPs -- Right operand (not an OpApp, but might be a NegApp).
  -> LHsExpr GhcPs
--      (e11 `op1` e12) `op2` e2
mkOpApp fs loc e1@(L _ (OpApp x1 e11 op1 e12)) op2 fix2 e2
  | nofix_error = L loc (OpApp noExt e1 op2 e2)
  | associate_right = L loc (OpApp x1 e11 op1 (mkOpApp fs loc' e12 op2 fix2 e2 ))
  where
    loc'= combineLocs e12 e2
    fix1 = findFixity fs op1
    (nofix_error, associate_right) = compareFixity fix1 fix2
--      (- neg_arg) `op` e2
mkOpApp fs loc e1@(L _ (NegApp _ neg_arg neg_name)) op2 fix2 e2
  | nofix_error = L loc (OpApp noExt e1 op2 e2)
  | associate_right = L loc (NegApp noExt (mkOpApp fs loc' neg_arg op2 fix2 e2) neg_name)
  where
    loc' = combineLocs neg_arg e2
    (nofix_error, associate_right) = compareFixity negateFixity fix2
--      e1 `op` - neg_arg
mkOpApp _ loc e1 op1 fix1 e2@(L _ NegApp {}) -- NegApp can occur on the right.
  | not associate_right  = L loc (OpApp noExt e1 op1 e2)-- We *want* right association.
  where
    (_, associate_right) = compareFixity fix1 negateFixity
 --     Default case, no rearrangment.
mkOpApp _ loc e1 op _fix e2 = L loc (OpApp noExt e1 op e2)

getIdent :: LHsExpr GhcPs -> String
getIdent (unLoc -> HsVar _ (L _ n)) = occNameString . rdrNameOcc $ n
getIdent _ = error "Must be HsVar"

-- If there are no fixities, give 'baseFixities'.
getFixities :: [(String, Fixity)] -> [(String, Fixity)]
getFixities fixities = if null fixities then baseFixities else fixities

findFixity :: [(String, Fixity)] -> LHsExpr GhcPs -> Fixity
findFixity fs r = askFix fs (getIdent r) -- Expressions.

findFixity' :: [(String, Fixity)] -> Located RdrName -> Fixity
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
