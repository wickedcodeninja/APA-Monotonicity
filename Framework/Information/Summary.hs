-- |(C) Wout Elsinghorst 2013

{-# LANGUAGE FlexibleInstances #-}

module Framework.Information.Summary (
    Summary (..),
    createSummary  -- :: Program Lab -> Summary
  ) where

import Framework.Information
import Framework.Information.ArithmeticExpression
import qualified Framework.Information.Block as Block

import Framework.While.Language


import Prelude hiding ( init )
import Data.Set hiding ( map )


-- |Datatype used to store calculated information about the program. Used
-- |to parameterize Framework
data Summary = Summary {
    g_init      :: Lab,
    g_final     :: Set Lab,
      
    g_vertices  :: Set Lab,         -- Labels
    g_edges     :: Set (Lab, Lab),  -- Normal forward flow, Reverse flow calculated when needed. 
      
    g_blocks    :: Set (Block.Block Lab),
    g_aexp      :: Set AExp,
    
    g_calls     :: Set (DelayedCall Lab),
    g_interflow :: Set (Lab, Lab, Lab, Lab, [(Decl, AExp)]),
    
    g_program   :: Program Lab
  }  
  
instance Information Summary where
  init     = g_init
  final    = g_final
  blocks   = g_blocks
  edges    = g_edges
  vertices = g_vertices
  calls    = g_calls

instance ArithmeticExpression Summary where
  aexp     = g_aexp


createSummary :: Program Lab -> Summary
createSummary p@(Program fs _) =
  let connectedCalls = unionMap (\(l_c, l_n, l_x, l_r, _) -> fromList [(l_c, l_n), (l_x, l_r)]) (g_interflow s) 
      
      s = Summary { 
        g_program  = p,
        
        g_init     = init p,
        g_final    = final p,
      
        g_edges    = connectedCalls `union` edges p,      
        g_vertices = unionMap Block.getLabels $ g_blocks s,
    
        g_blocks   = blocks p,
        g_aexp     = aexp p,
    
        g_calls    = calls p,
      
        g_interflow =
          let matchProc (l_c, l_r) callName vars = concatMap match fs where
                match (Function (l_n, l_x) procName decls _) = if callName == procName
                                                                  then let vdecls = if length decls == length vars
                                                                                            then zip decls vars
                                                                                            else error "g_interflow: unmatched parameters"
                                                                       in [(l_c, l_n, l_x, l_r, vdecls)]
                                                              
                                                              else []

          in unionMap (\(DelayedCall edge nm vars) -> fromList $ matchProc edge nm vars) $ calls s
  } in s
