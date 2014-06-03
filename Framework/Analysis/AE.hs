-- |(C) Wout Elsinghorst 2013

{-# LANGUAGE FlexibleInstances #-}

module Framework.Analysis.AE (
  driver
  ) where

import Framework
import Framework.Analysis

import Framework.Information
import Framework.Information.Summary
import Framework.Information.FreeVariables
import Framework.Information.ArithmeticExpression
import Framework.Information.Block

import Framework.Analysis.Generic

import Framework.While.Language

import qualified Data.Map
import qualified Data.Set

import Prelude hiding ( init )
import Data.Set hiding ( map, foldr )
  

driver :: Package (Set AExp)
driver = Package {
  createFramework = \p ->
    let bottom = f_bottom fw
        kill l =
          case lookupBlock fw l of
            BlockAssign _ x _ -> unionMap (\t -> if x `member` fv t then singleton t else empty) $ aexp fw
            BlockSkip   _     -> empty
            BlockCond   _ b   -> empty
            BlockProc   _     -> empty
            BlockCall edge@(l_c, l_r) _ decls -> 
              let declMapping = map (\(Decl ty dst, src) -> (ty, dst, src) ) $ getIntercall fw edge 
                  transfer | l == l_c = \(t, x, a) -> union $
                              case t of
                                VarTy    -> unionMap (\t -> if x `member` fv t then singleton t else empty) $ aexp a
                                ReturnTy -> Data.Set.empty
                            | l == l_r = \(t, a, x) -> union $
                              case (t, x) of
                                (ReturnTy, Var r) -> unionMap (\t -> if r `member` fv t then singleton t else empty) $ aexp (Var a)
                                (VarTy   , _    ) -> Data.Set.empty
              in foldr transfer Data.Set.empty declMapping
        gen l =
          case lookupBlock fw l of
            BlockAssign _ x a -> unionMap (\t -> if not (x `member` fv t) then singleton t else empty) $ aexp a
            BlockSkip   _     -> empty
            BlockCond   _ b   -> aexp b
            BlockProc   _     -> empty
            BlockCall edge@(l_c, l_r) _ decls -> 
              let declMapping = map (\(Decl ty dst, src) -> (ty, dst, src) ) $ getIntercall fw edge 
                  transfer | l == l_c = \(t, x, a) -> union $
                              case t of
                                VarTy    -> unionMap (\t -> if not (x `member` fv t) then singleton t else empty) $ aexp a
                                ReturnTy -> Data.Set.empty
                            | l == l_r = \(t, a, x) -> union $
                              case (t, x) of
                                (ReturnTy, Var r) -> unionMap (\t -> if not (r `member` fv t) then singleton t else empty) $ aexp (Var a)
                                (VarTy   , _    ) -> Data.Set.empty
              in foldr transfer Data.Set.empty declMapping
        fw = Framework {
          f_join     = intersection,
          f_bottom   = aexp fw,
          f_iota     = empty,
          f_extreme  = singleton (init fw),
          f_flow     = flow fw,
          f_interflow = interflow fw,
          f_transfer = transfer_gk fw gen kill,
          
          f_summary  = createSummary p
         } 
    in fw,      
  showAnalysis = \m -> "Available Expressions: " ++ show m
}

--TODO: share with CP

-- |For (l_c, l_r) function call, determine how call arguments are mapped to the
-- |corresponding function parameters
getIntercall :: Framework Summary k -> (Lab, Lab) -> [(Decl, AExp)]
getIntercall fw edge@(l_c, l_r) = 
  let reduced = Data.Set.filter (\(c, _, _, r, _) -> l_c == c && l_r == r) $ interflow' fw
  in case toList reduced of
          []                  -> error $ "getIntercall: no interflow available for call " ++ show edge 
          (_, _, _, _, r) : _ -> r
          
