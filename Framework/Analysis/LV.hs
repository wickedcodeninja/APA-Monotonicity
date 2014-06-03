-- |(C) Wout Elsinghorst 2013

module Framework.Analysis.LV (
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
    
driver :: Package (Set Var)
driver = Package {
  createFramework = \p ->
    let kill l =
          case lookupBlock fw l of
            BlockAssign _ x _ -> singleton x
            BlockSkip   _     -> empty
            BlockCond   _ b   -> empty
            BlockProc   _     -> empty
            BlockCall   _ _ decls -> error "LV: procedure calls not handled yet"
        gen l =
          case lookupBlock fw l of
            BlockAssign _ _ a -> fv a
            BlockSkip   _     -> empty
            BlockCond   _ b   -> fv b
            BlockProc   _     -> empty
            BlockCall   _ _ decls -> error "LV: procedure calls not handled yet"
          
        fw = Framework {
          f_join     = union,
          f_bottom   = empty,
          f_iota     = empty,
          f_extreme  = final fw,
          f_flow     = flipFlow (flow fw),
          f_interflow = interflow fw,
          f_transfer = transfer_gk fw gen kill,

          f_summary  = createSummary p
        } 
    in fw,
  showResult = show
}
flipFlow :: Set (Lab, Lab) -> Set (Lab, Lab)
flipFlow = Data.Set.map (\(a, b) -> (b, a))    
