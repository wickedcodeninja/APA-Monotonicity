-- |(C) Wout Elsinghorst 2013

module Framework (
  Framework (..),
  E (..),
  F (..),
  interflow,
  program,
  lookupBlock,
  showFunctionMap
  ) where
    
    
import Framework.Information
import Framework.Information.ArithmeticExpression
import Framework.Information.Summary
import Framework.While.Language

import qualified Framework.Information.Block as Block

import Prelude hiding ( init )

import Data.Set hiding ( foldr )
import qualified Data.Map

type E = Set Lab
type F = Set (Lab, Lab)

-- |This datatype describes the framework. The transfer function is
-- |a binary function. The first parameter is used when transfering the
-- |r part of a (l_c, l_r) call, but it is otherwise ignored. 

data Framework s l = Framework {
    f_join     :: l -> l -> l,
    f_bottom   :: l,
    f_iota     :: l,
    f_extreme  :: E,                    
    f_flow     :: F,                    
    f_transfer :: Lab -> (l -> l -> l), 
    f_summary  :: s
  }

 
   
-- |Forward Information methods to work on Framework
instance (Information s) => Information (Framework s r) where 
  init = init . f_summary
  final = final . f_summary
  
  edges = edges . f_summary
  vertices = vertices . f_summary

  blocks = blocks . f_summary
  
  calls = calls . f_summary
  
-- |Forward..
instance (ArithmeticExpression s) => ArithmeticExpression (Framework s r) where
  aexp = aexp . f_summary

-- |Forward..
program :: Framework Summary k -> Program Lab
program = g_program . f_summary

-- | Interflows consisting of (l_c, l_e, l_x, l_r) together with
-- | an argument -> parameter mapping.
interflow :: Framework Summary k -> Set (Lab, Lab, Lab, Lab, [(Decl, AExp)])
interflow = g_interflow . f_summary


-- |Find the block matching the label. In the case of Proc or Call, 
-- |both the initial and the final labels will connect to the same block.
lookupBlock :: Framework Summary r -> Lab -> Block.Block Lab
lookupBlock fw l =
  case (toList . Data.Set.filter (Block.hasLabel l) . blocks $ fw) of
    []   -> error $ "lookupBlock: no block found labeled \'" ++ show l ++ "\'"
    r:_  -> r

-- |Interpret a `Map k a` as a function `k -> a` on a finite domain. Show.
showFunctionMap :: (Show a, Show k, Ord k) => Data.Map.Map k a -> String
showFunctionMap m = "{\n" ++ (foldr (\(k, a) xs -> "  " ++ show k ++ "\t-> " ++ show a ++ "\n" ++ xs) [] . Data.Map.toList $ m) ++ "}\n"
    