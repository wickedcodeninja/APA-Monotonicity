-- |(C) Wout Elsinghorst 2013

{-# LANGUAGE FlexibleInstances #-}

module Framework.Information.Block (
    Block (..),
    hasLabel,
    getLabels
  ) where

import Framework.While.Language

  
import Data.Set
  

data Block lab
    = BlockAssign lab Var AExp
    | BlockSkip   lab 
    | BlockCond   lab BExp
    | BlockCall   (lab, lab) String [AExp]
    | BlockProc   (lab, lab) -- [Decl] - should probably also be saved here
  deriving (Show, Ord, Eq)

getLabels :: (Eq a, Ord a) => Block a -> Set a
getLabels b = 
  case b of
    BlockAssign l _ _     -> singleton l
    BlockSkip l           -> singleton l
    BlockCond l _         -> singleton l
    BlockProc (l_e, l_x)     -> fromList [l_e, l_x]
    BlockCall (l_c, l_r) _ _ -> fromList [l_c, l_r]
  
hasLabel :: Eq a => a -> Block a -> Bool
hasLabel l b = 
  case b of
    BlockAssign l_t _ _      -> l == l_t
    BlockSkip l_t            -> l == l_t
    BlockCond l_t _          -> l == l_t
    BlockProc (l_e, l_x)     -> l == l_e || l_x == l
    BlockCall (l_c, l_r) _ _ -> l == l_c || l_r == l
