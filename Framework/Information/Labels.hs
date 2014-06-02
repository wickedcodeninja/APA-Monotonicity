-- |(C) Wout Elsinghorst 2013

{-# LANGUAGE FlexibleInstances #-}

module Framework.Information.Labels (
    decorate -- 
  ) where


import Framework.While.Language
import Framework.Information
  
import Data.Set hiding ( map, foldr )
  
-- |Replace all placeholder ()-values in a structure
-- |by unique labels. `labels` is recursively defined
-- |over the substructures.
decorate :: Labels f => f () -> f Lab
decorate p = case labels 1 p of
                  (_, p') -> p'
  
class Labels f where
  -- |First parameter passed is assume to be free for use.
  -- |The second parameter is the structure to be labeled
  -- |The result tuple contains a labeled structure at the right
  -- |and a new starting index at the left, which is again free
  -- |for use.
  labels :: Lab -> f () -> (Lab, f Lab)

instance Labels Statement where
  labels k s = 
    case s of 
      Declaration () t        -> (k + 1, Declaration k t)
      Call ( (), () ) nm vars -> (k + 2, Call (k, k+1) nm vars)
      Seq xs                  -> let folder g gs = \n -> let (n_g,  s_g ) = labels n g
                                                             (n_gs, s_gs) = gs n_g
                                                         in (n_gs, s_g : s_gs)
                                     (n_xs, s_xs) = foldr folder (\n -> (n, [])) xs k 
                                 in (n_xs, Seq s_xs)
      Assign () x a           -> (k + 1, Assign k x a) 
      IfThenElse () b x y     -> let (n_x, s_x) = labels (k + 1) x
                                     (n_y, s_y) = labels n_x     y
                                 in (n_y, IfThenElse k b s_x s_y)
      While () b x            -> let (n_x, s_x) = labels (k + 1) x
                                 in (n_x, While k b s_x)
      Skip ()                 -> (k + 1, Skip k)
  
    
instance Labels Function where
  labels k (Function ( (), () ) nm decls s) =
    let (r, s') = labels (k + 1) s
    in (r + 1, Function (k, r) nm decls s')

instance Labels Program where
  labels k (Program fs p) =
    let folder g gs = \n -> let (n_g,  s_g ) = labels n g 
                                (n_gs, s_gs) = gs n_g
                            in (n_gs, s_g : s_gs)

        (n_fs, fs') = foldr folder (\n -> (n, [])) fs k
        (n_p,  p' ) = labels n_fs p
    in (n_p, Program fs' p')
