-- |(C) Wout Elsinghorst 2013

module Framework.Analysis.Generic (
  transfer_gk
  ) where
  
import Framework
import Framework.Information

import Data.Set
  
-- |Generic transfer function parameterised by gen and kill sets.
-- |Used in AE, LV, RD and VB analysis'.

transfer_gk :: Ord k => Framework s (Set k)
                  -> (Lab -> Set k) -- gen
                  -> (Lab -> Set k) -- kill
                  -> Lab -> (Set k -> Set k -> Set k)
transfer_gk fw gen kill = \lab _ q ->  
  let join = f_join fw
  in (q \\ kill lab) `join` gen lab 
