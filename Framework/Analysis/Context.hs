-- |(C) Wout Elsinghorst 2013

{-# LANGUAGE TypeOperators #-}

module Framework.Analysis.Context (
  addContext
  ) where

import Framework
import Framework.Analysis

import Framework.Information
import Framework.Information.Summary
import Framework.Information.FreeVariables
import Framework.Information.Block

import Framework.While.Language

import qualified Data.Map
import qualified Data.Set

import Prelude hiding ( init )
import Data.Set hiding ( map, foldr )
  
-- |Add callstring context to a given framework. The parameter k is used to 
-- |indicate callstring length.
addContext :: Int -> Package r -> Package ([Lab] ~> r)
addContext k lp =
  let pkg = Package {
        createFramework = \p ->    
          let legacy = createFramework lp p
              
              bottom = f_bottom legacy 
              iota = f_iota legacy
              join = f_join legacy
              
              -- |At non-proc/call-points, the transfer function is just the 
              -- |pointwise application of the inherited transfer function
              transfer_legacy l = Data.Map.unionWith (f_transfer legacy l)
              
              -- |Calculate all traces of length k for the given program
              traces = collectTraces fw k
              
              
              -- |Used to interpret `Map k a` as a function k -> a
              infix 0 $*
              f $* a = case Data.Map.lookup a f of
                            Just b  -> b
                            Nothing -> bottom
                
              -- |Find all traces with a given prefix
              findTracesWithPrefix :: [Lab] -> Set [Lab]
              findTracesWithPrefix pfix =
                let r = length pfix
                in Data.Set.filter (\tr -> take r tr == pfix) traces
              
              transfer l =
                case lookupBlock fw l of
                    -- Intra-procedural fragment
                    BlockAssign _ _ _ -> transfer_legacy l -- |Lift the original transfer functions to
                    BlockSkip _       -> transfer_legacy l -- |to the context world
                    BlockCond _ _     -> transfer_legacy l
                    
                    -- Inter-procedural fragment
                    BlockProc _       -> const id
                    BlockCall edge@(l_c, l_r) nm _ -> \p q -> flip Data.Map.fromSet traces $
                      if l == l_c                          -- f_l^(L)[d] = union { f_{l_c}(L[d']) | d = l_c : d' }
                          then \d' -> case d' of           -- 
                                        []    -> bottom
                                        t : d -> if t == l_c 
                                                    then if length d < k 
                                                            then f_transfer legacy l_c bottom (q $* d) -- not truncated
                                                            else -- |Context trunctation is not injective,
                                                                 -- |Join over all context that might have
                                                                 -- |truncated to the current context
                                                                 let trans pfix = f_transfer legacy l_c bottom (q $* pfix) -- take sup over 
                                                                     pfixList = toList $ findTracesWithPrefix d            -- all contexts
                                                                 in foldr join bottom . map trans $ pfixList               -- with this prefix
                                                    else bottom  -- NOTE: I'm not totally sure this case can/should occur.
                          else
                      if l == l_r                         -- f_l^(L, L')[d] =  f_l(L d, L' (l_c : d) )
                          then \d -> f_transfer legacy l_r (p $* d) (q $* take k (l_c : d) )      
                      
                          else error "contextCallStack: inconsistent transfer"
                        
        
              fw = Framework {
                  f_join     = Data.Map.unionWith join,                               -- Lift join pointwise 
                  f_bottom   = Data.Map.fromSet (const bottom) traces,                -- \_ -> bottom
                  f_iota     = Data.Map.update (Just . const iota) [] $ f_bottom fw,  -- \t -> bottom  if t /= []
                                                                                      -- \t -> iota    if t == []
                  f_extreme  = f_extreme legacy,
                  f_flow     = f_flow legacy,
                  f_transfer = transfer, 
                
                  f_summary  = f_summary legacy
                } 
          in fw,
            
        showAnalysis = \m -> 
          foldr (\(k, a) xs -> "trace = " ++ show k ++ "\n" ++ showAnalysis lp a ++ xs) [] . Data.Map.toList $ m
       }
  in pkg
  
  
-- |Calculate all traces up to length n for the program associated to fw
collectTraces :: Framework Summary r -> Int -> Set [Lab]
collectTraces fw n  = 
  let Program fs s = program fw
      
      iflow = interflow fw
      
      clist = calls fw
      
      -- |For a given function, calculate all function calls (directly) called
      -- |made that function
      collectTraceF :: Function Lab -> Set (DelayedCall Lab)
      collectTraceF (Function edge nm decls s) = collectTraceS s
      
      -- |Idem, for statements
      collectTraceS :: Stmt         -> Set (DelayedCall Lab)
      collectTraceS stmt = 
                case stmt of
                  Declaration _ _      -> empty
                  Call edge nm vars    -> singleton $ DelayedCall edge nm vars
                  Seq xs               -> unions $ map collectTraceS xs 
                  Assign _ _ _         -> empty
                  IfThenElse _ _ x y   -> collectTraceS x `union` collectTraceS y
                  While _ _ x          -> collectTraceS x
                  Skip _               -> empty
      
      -- |For a given function call, find the callee 
      matchProc :: DelayedCall Lab -> Function Lab
      matchProc (DelayedCall (l_c, l_r) _ _) = 
        let reduced = unionMap (\(c, e, x, r, _) -> if c == l_c && r == l_r then singleton (e, x) else empty) $ iflow
        in case elems reduced of
             edge:_ -> case Prelude.filter (\(Function t _ _ _) -> t == edge) fs of
                             f:_ -> f
                             []  -> error "traces: no edge"
             []     -> error "traces: no flow"
             
      
      -- |Expand the program as a (necessarally finite) forrest and for each tree
      -- |and for each path in that tree, collect all fragments of length <= k
      solution :: Int -> DelayedCall Lab -> Set ([Lab])
      solution k dc | k == 0    = empty
                    | otherwise = 
        let DelayedCall (l_c, l_r) _ _ = dc
            collector r =
              let subsol = solution (k - 1) r 
              in Data.Set.map (\t ->  l_c : t) subsol `union` subsol
        in singleton [l_c] `union` unionMap collector (collectTraceF $ matchProc dc)  
        
      
  in singleton [] `union` (unionMap (solution n) $ calls fw)

  