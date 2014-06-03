-- |(C) Wout Elsinghorst 2013

module Framework.Analysis.CP (
  driver,
  StateCP (..),
  ZBounded (..)
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

import Data.Map ( Map (..) )

data ZBounded 
  = ZTop
  | ZBottom
  | ZFinite Integer
  deriving (Eq, Ord, Show)
  
type StateCP = Data.Map.Map Var ZBounded
  
driver :: Package StateCP
driver = Package {
  createFramework = \s ->
    let z_top = ZTop
        z_bottom = ZBottom
        z_join a b = case (a, b) of
                          (ZFinite p, ZFinite q) -> if p == q then ZFinite p else z_top
                          (ZBottom  ,         r) -> r
                          (        r,   ZBottom) -> r
                          (ZTop     ,         r) -> ZTop
                          (        r,      ZTop) -> ZTop
        
        
        var_star = fv s  --Assuming fv(S*) == Var*, the book doesn't define fv
        
        join = f_join fw
        bottom = f_bottom fw
        
        -- |Given an AExp and a assignment of variables, decide if the expression is constant.
        -- |Just like in the book.                                                                  
        acp :: AExp -> (StateCP -> ZBounded)
        acp r w = case r of
                  Var x -> if w == bottom
                              then z_bottom
                              else case Data.Map.lookup x w of
                                    Just h  -> h
                                    Nothing -> error "acp: bah partial functions" 
                  Num n -> if w == bottom
                              then z_bottom
                              else ZFinite n
                  AOperator op x y -> acp x w `lop` acp y w where
                    lop a b = case (a, b) of
                                  (ZFinite p, ZFinite q) -> ZFinite $ case op of 
                                                                            Add -> p + q
                                                                            Sub -> p - q
                                                                            Mul -> p * q
                                  _                      -> z_top

        transfer_cp l = case lookupBlock fw l of
                          BlockAssign _ x a -> \_ q -> if q == bottom 
                                                          then bottom
                                                          else Data.Map.insert x (acp a q) q
                          BlockSkip _       -> const id
                          BlockCond _ _     -> const id
                          
                          BlockProc _       -> const id
                          BlockCall edge@(l_c, l_r) _ decls -> \p q -> 
                            let declMapping = map (\(Decl ty dst, src) -> (ty, dst, src) ) $ getIntercall fw edge 
                                transfer | l == l_c = \(t, x, a) q ->
                                            case t of
                                              VarTy    -> Data.Map.insert x (acp a q) q
                                              ReturnTy -> q
                                         | l == l_r = \(t, a, x) q -> 
                                            case (t, x) of
                                              (ReturnTy, Var r) -> Data.Map.insert r (acp (Var a) q) q
                                              (VarTy   , _    ) -> q
                            in if q == bottom
                                  then bottom
                                  else foldr transfer q declMapping
                              
          
        fw = Framework {
          f_join     = Data.Map.unionWith z_join,
          f_bottom   = Data.Map.fromSet (const z_bottom) var_star,
          f_iota     = Data.Map.fromSet (const z_top)    var_star,
          f_extreme  = singleton (init fw),
          f_flow     = flow fw,
          f_interflow = interflow fw,
          f_transfer = transfer_cp,
    
          f_summary  = createSummary s
        } 
    in fw,  
  showAnalysis = \m -> "Constant Propagation " ++ showFunctionMap m
}
  
-- |For (l_r, l_c) function call, determine how call arguments are mapped to the
-- |corresponding function parameters
getIntercall :: Framework Summary k -> (Lab, Lab) -> [(Decl, AExp)]
getIntercall fw edge@(l_c, l_r) = 
  let reduced = Data.Set.filter (\(c, _, _, r, _) -> l_c == c && l_r == r) $ interflow' fw
  in case toList reduced of
          []                  -> error $ "getIntercall: no interflow available for call " ++ show edge 
          (_, _, _, _, r) : _ -> r
          
