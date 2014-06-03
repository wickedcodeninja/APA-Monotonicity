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
    let kill l =
          case lookupBlock fw l of
            BlockAssign _ x _ -> unionMap (\t -> if x `member` fv t then singleton t else empty) $ aexp fw
            BlockSkip   _     -> empty
            BlockCond   _ b   -> empty
            BlockProc   _     -> empty
            BlockCall   _ _ decls -> error "AE: procedure calls not handled yet"
        gen l =
          case lookupBlock fw l of
            BlockAssign _ x a -> unionMap (\t -> if not (x `member` fv t) then singleton t else empty) $ aexp a
            BlockSkip   _     -> empty
            BlockCond   _ b   -> aexp b
            BlockProc   _     -> empty
            BlockCall   _ _ decls -> error "AE: procedure calls not handled yet"
            
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
