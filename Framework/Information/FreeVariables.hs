-- |(C) Wout Elsinghorst 2013

{-# LANGUAGE FlexibleInstances #-}

module Framework.Information.FreeVariables (
    FreeVariables (..)
  ) where


import Data.Set hiding ( map )
  
import Framework.While.Language
import Framework.Information


class FreeVariables s where
  fv :: s -> Set Var

instance FreeVariables (Program Lab) where
  fv r = case r of
           Program fs s    -> unions (map fv fs) `union` fv s

instance FreeVariables (Function Lab) where
  fv r = case r of
           Function  _ _ ds s  -> fv s `union` fromList ( map (\(Decl _ x) -> x) ds ) 
           
  
instance FreeVariables (Statement Lab) where
  fv r = case r of
           Declaration _ (Decl _ v) -> singleton v
           Seq xs              -> unions $ map fv xs
           Assign _ x a        -> singleton x `union` fv a
           Skip   _            -> empty
           IfThenElse _ c x y  -> fv c `union` fv x `union` fv y
           While      _ c x    -> fv c `union` fv x
           Call       _ _ xs   -> unions $ map fv xs
--           Continue   _        -> empty
--           Break   _           -> empty
  
  
instance FreeVariables AExp where
  fv r = case r of 
           Var x           -> singleton x
           Num _           -> empty
           AOperator _ a b -> fv a `union` fv b 

instance FreeVariables BExp where
  fv r = case r of
           Atom _          -> empty
           Not b           -> fv b
           BOperator _ x y -> fv x `union` fv y
           ROperator _ x y -> fv x `union` fv y

