-- |(C) Wout Elsinghorst 2013

{-# LANGUAGE FlexibleInstances, TypeOperators #-}

module Framework.Information (
    Information (..),
    DelayedCall (..),
    Lab (..),
    Stmt (..),          -- type Stmt = Statement Lab
    (~>) (..),          -- type (~>) k a = Map k a
    
    unionMap -- TODO: move around
  ) where

import Framework.While
import Framework.Information.Block as Block
  
import Prelude hiding ( init )
import Data.Set hiding ( map )
  
import qualified Data.Set
  
import Data.Map ( Map (..) )



unionMap :: (Ord a, Ord b) => (a -> Set b) -> (Set a -> Set b)
unionMap f = fromList . concatMap (toList . f) . toList 

data DelayedCall lab = DelayedCall (lab, lab) String [AExp]
  deriving (Eq, Ord, Show)

type (~>) d a = Map d a

type Lab = Int

type Stmt = Statement Lab

class Information s where
  init :: s -> Lab
  final :: s -> Set Lab
  
  vertices :: s -> Set Lab
  edges :: s -> Set (Lab, Lab)
  
  blocks :: s -> Set (Block Lab)
  
  calls :: s -> Set (DelayedCall Lab)

instance Information (Function Lab) where
  init (Function (l_n, l_x) _ _ _)     = l_n
  final (Function (l_n, l_x) _ _ _)    = singleton l_x
  
  edges (Function (l_n, l_x) _ _ s)    = singleton (l_n, init s) `union` edges s `union` Data.Set.map (\l -> (l, l_x)) (final s)
  vertices (Function (l_n, l_x) _ _ s) = fromList [l_n, l_x] `union` vertices s
  
  blocks (Function edge _ _ s)    = singleton (BlockProc edge) `union` blocks s 

  calls (Function _ _ _ s)        = calls s
    
  
instance Information (Program Lab) where
  init (Program _ s)      = init s
  final (Program _ s)     = final s
  
  edges (Program fs s)    = (unions $ map edges    fs) `union` edges s
  vertices (Program fs s) = (unions $ map vertices fs) `union` vertices s  
  
  blocks (Program fs s)   = (unions $ map blocks   fs) `union` blocks s

  calls (Program fs s)    = (unions $ map calls    fs) `union` calls s
                  
instance Information (Statement Lab) where
  init s = case s of 
    Declaration l _    -> l
    Seq xs             -> if Prelude.null xs
                             then error "init: empty Seq"
                             else init (head xs)
    Assign l _ _       -> l
    Skip   l           -> l
    IfThenElse l _ _ _ -> l
    While      l _ _   -> l
    Call (l_c, l_r) _ _   -> l_c

  final s = case s of 
    Declaration l _    -> singleton l
    Seq xs             -> if Prelude.null xs
                             then error "final: empty Seq"
                             else final $ last xs
    Assign l _ _       -> singleton l
    Skip   l           -> singleton l
    IfThenElse l c a b -> final a `union` final b
    While      l _ _   -> singleton l
    Call (l_c, l_r) _ _   -> singleton l_r
    
  blocks s = case s of
    Declaration l _    -> singleton $ BlockSkip l
    Assign l v e       -> singleton $ BlockAssign l v e
    Skip   l           -> singleton $ BlockSkip l   
    Seq xs             -> unions $ map blocks xs
    IfThenElse l c a b -> singleton (BlockCond l c) `union` blocks a `union` blocks b 
    While      l c a   -> singleton (BlockCond l c) `union` blocks a
    Call (l_c, l_r) nm xs -> singleton (BlockCall (l_c, l_r) nm xs)
    
  edges s = case s of
    Declaration _ _    -> empty
    Seq xs             -> (unions $ map edges xs) `union` connection xs where
      connection []       = empty
      connection (x:[])   = edges x
      connection (x:y:zs) = edges x `union` link x y `union` connection (y:zs) where
        link x y = Data.Set.map (\l -> (l, init y)) $ final x
    Assign _ _ _       -> empty
    Skip   _           -> empty
    IfThenElse l c a b -> edges a `union` edges b `union` fromList [(l, init a), (l, init b)]
    While      l c a   -> edges a `union` singleton (l, init a)
    Call (l_c, l_r) nm xs -> empty -- The interflow edges (l_c; l_e) and (l_x, l_r )
                                   -- are added in `createSummary` where the needed
                                   -- information is available.
  
  calls s = case s of
    Declaration _ _      -> empty
    Call (l, l') nm vars -> singleton $ DelayedCall (l, l') nm vars
    Seq xs               -> unions $ map calls xs 
    Assign _ _ _         -> empty
    IfThenElse _ _ x y   -> calls x `union` calls y
    While _ _ x          -> calls x
    Skip _               -> empty
    
  vertices = unionMap Block.getLabels . blocks
