-- |(C) Wout Elsinghorst 2013

module Framework.Analysis.RD (
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
   
driver :: Package ( Set (Var, Maybe Lab) )
driver = Package {
  createFramework = \p ->
    let kill l =
          case lookupBlock fw l of
            BlockAssign l x _ -> singleton (x, Nothing) `union` (unionMap matcher . collectAssigns $ fw) where
              matcher (BlockAssign l' y _) = if x == y then singleton (x, Just l') else empty
              matcher _                    = empty
            BlockSkip   _     -> empty
            BlockCond   _ b   -> empty
            BlockProc   _     -> empty
            BlockCall   _ _ decls -> error "RD: procedure calls not handled yet"
            
        gen l =
          case lookupBlock fw l of
            BlockAssign _ x a -> singleton (x, Just l)
            BlockSkip   _     -> empty
            BlockCond   _ b   -> empty
            BlockProc   _     -> empty
            BlockCall   _ _ decls -> error "RD: procedure calls not handled yet"
            
        fw = Framework {
          f_join     = union,
          f_bottom   = empty,
          f_iota     = Data.Set.map (\v -> (v, Nothing)) $ fv p,
          f_extreme  = singleton (init fw),
          f_flow     = flow fw,
          f_interflow = interflow fw,
          f_transfer = transfer_gk fw gen kill,
          
          f_summary  = createSummary p
        } 
    in fw,       
  showAnalysis = \m -> "Reaching Definitions: " ++ show m
}
collectAssigns :: (Eq r) => Framework Summary r -> Set (Block Lab)
collectAssigns fw = unionMap matcher $ blocks fw where
  matcher r@(BlockAssign _ _ _) = singleton r
  matcher _                     = empty
