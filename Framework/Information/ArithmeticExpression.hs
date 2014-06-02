-- |(C) Wout Elsinghorst 2013

{-# LANGUAGE FlexibleInstances #-}

module Framework.Information.ArithmeticExpression (
    ArithmeticExpression (..)
  ) where
  
import Framework.While.Language
import Framework.Information

  
import Data.Set hiding ( map )

-- |Collect all arithmetic expressions found in a given program construct
class ArithmeticExpression s where
  aexp :: s -> Set AExp

instance ArithmeticExpression (Program Lab) where
  aexp (Program fs s) = (unions $ map aexp fs) `union` aexp s
  
instance ArithmeticExpression (Function Lab) where
  aexp (Function _ _ _ s)     = aexp s

  
instance ArithmeticExpression (Statement a) where
  aexp s = case s of 
    Seq xs             -> unions $ map aexp xs
    Skip _             -> empty
    Declaration _ _    -> empty
    Assign _ _ a       -> aexp a
    IfThenElse _ c x y -> aexp c `union` aexp x `union` aexp y
    While _ c x        -> aexp c `union` aexp x
    Call  _ _ _        -> empty
--    Continue _         -> empty
--    Break _            -> empty

    
instance ArithmeticExpression BExp where
  aexp = subexp where
    subexp (Atom _) = empty
    subexp (Not  _) = empty
    subexp (BOperator _ x y) = subexp x `union` subexp y
    subexp (ROperator _ x y) = aexp x `union` aexp y
  
instance ArithmeticExpression AExp where
  aexp a = singleton a `union` subexp a where
    subexp (Num _) = empty
    subexp (Var _) = empty
    subexp (AOperator _ x y) = aexp x `union` aexp y
    
    
