-- |(C) Wout Elsinghorst 2013

{-# LANGUAGE FlexibleInstances #-}

module Framework.Analysis (
  Package (..),    -- Package { createFramework, showAnalysis }
  Analysis (..),   -- type Analysis r = Map Lab r
  runFramework,    -- :: Eq r => Framework Summary r -> (Analysis r, Analysis r)
  showResults      -- :: Package r -> Analysis r -> String
  ) where

import Framework.While.Language
  
import Framework
import Framework.Information
import Framework.Information.Block
import Framework.Information.Summary
  
import qualified Data.Map
import qualified Data.Set

import Data.Set hiding ( map, foldr )
import Data.Map ( Map (..) )

import qualified Framework.Information.Block as Block

type Analysis r = Map Lab r

data Package r = Package {
    createFramework :: Program Lab -> Framework Summary r,
    showResult :: r -> String
  }


lfp :: Eq r => Framework Summary r 
            -> [(Lab, Lab)]   -- Worklist 
            -> Analysis r     -- Initial (unsound) estimate
            -> Analysis r     -- Final analysis result
lfp fw = go where
  bottom = f_bottom fw
  join = f_join fw
  a <= b = a `join` b == b

  -- while W not empty
  go []              = id
  go ( (l, l') : w ) = \m ->  
    let analysis l = case Data.Map.lookup l m of
                       Just p  -> p
                       Nothing -> error "lfp: label not available"
      
        pickOther (l_a, l_b) l = if l == l_a then l_b
                                    else
                                 if l == l_b then l_a
                                    else error "lfp: label lookup inconsistent"
        
        f_l  = case lookupBlock fw l of
                 BlockCall edge _ _  -> f_transfer fw l (analysis $ pickOther edge l) -- f_{l_c, l_r}
                 BlockProc edge      -> f_transfer fw l (analysis $ pickOther edge l) -- f_{l_e, l_x}
                 _                   -> f_transfer fw l bottom                        -- f_l
        p = f_l (analysis l)
        
    in if not (p <= analysis l')
        then let m' = Data.Map.update (\q -> Just $ q `join` p) l' m
                 w' = toList (Data.Set.filter (\(t, _) -> l' == t) $ f_flow fw) ++ w
             in go w' m'
        else go w m
  
  
runFramework :: Eq r => Framework Summary r -> (Analysis r, Analysis r)
runFramework fw =  
  let bottom = f_bottom fw
      iota   = f_iota   fw
      
      setToMap :: Ord k => Set (k, l) -> Map k l
      setToMap = Data.Map.fromList . Data.Set.toList 
      
      -- Prepare worklist W
      w = toList $ f_flow fw
      
      -- All labels occurring in the flow F are initially bottom
      f = concatMap (\(l, l') -> [(l, bottom), (l', bottom)]) $ Data.Set.toList (f_flow fw)
      
      -- All extreme labels are initialy iota
      e = map (\l -> (l, iota  )) $ Data.Set.toList (f_extreme fw) 

      -- Setup initial Analysis array. Prefer defaults from E over F.
      -- In a program with isolated entries, these should be disjoint.
      m = Data.Map.unionWith (curry snd) (Data.Map.fromList f) (Data.Map.fromList e)
      
      -- Calculate least fixed point
      r = lfp fw w m
      
  in (r, Data.Map.mapWithKey (\l -> f_transfer fw l bottom) r) 

showResults :: Package r -> Analysis r -> String
showResults pkg = foldr (\(k, a) xs -> "label = " ++ show k ++ "\n\n" ++ showResult pkg a ++ "\n *** \n" ++ xs) [] . Data.Map.toList 

  