-- |(C) Wout Elsinghorst 2013

import Framework.While

import Framework

import Framework.Information
import Framework.Information.ArithmeticExpression
import Framework.Information.FreeVariables
import Framework.Information.Labels
import Framework.Information.Summary
import Framework.Information.Block ( Block (..) )

import Framework.Analysis

import qualified Framework.Information.Block as Block


import qualified Examples


import Data.Set hiding ( foldr, map, null )
import Data.Map ( Map (..) )
import Data.Maybe

import Control.Monad

import qualified Prelude
import qualified Data.Set
import qualified Data.Map

import Prelude hiding ( break, init, floor, ceil )

import qualified Framework.Analysis as Analysis
import qualified Framework.Analysis.AE as AE
import qualified Framework.Analysis.LV as LV
import qualified Framework.Analysis.RD as RD
import qualified Framework.Analysis.VB as VB

import qualified Framework.Analysis.CP as CP

import qualified Framework.Analysis.Context as Context
 
-- |Overview:
-- |  Framework.hs: This defines the main interface that analysis instances should 
-- |    implement.
-- |
-- |  Framework.While.Language: contains the abstract syntax for the while language
-- |  Framework.While.Parser: contains the parser
-- |
-- |  Framework.Information: this module and various submodules contain
-- |    various classes and datastructures to facilitate making calculated
-- |    information for a specific program easily available.
-- |  Framework/Information.hs defines the main class making {init, final, vertices, 
-- |    edges, blocks, call} methods available to work on various structures
-- |  
-- |
-- |  Framework/Analysis.hs: contains an implementation of the work list algorithm
-- |  Framework.Analysis.{CP, AE, LV, RD, VB}: these modules contain various predefined
-- |    analysis'. Only CP is currently fully implemented.
-- |  Framework.Analysis.Context: the addContext method enhances an existing framework
-- |    with bounded callstring history.
-- |
-- |  Examples.hs: contains the pre-written programs `propagation1` and `propagation2` 
-- |               which show constant propagation.

 
 
 
 
-- |Main entry point to the framework. 
-- |
-- |Change CP.driver to {AE, CP, LV, RD, VB}.driver to test other analyses.
-- |Unfortunately, no other analysis besides CP are currently fully 
-- |implemented.

    
main =
  let k                 = 1                     -- Length of Call Strings
      driver            = CP.driver             -- Change CP to one \in {AE, CP, LV, RD, VB} 
      program           = Examples.cp2          -- Choose test programs \in {cp1, cp2, ae1}             
                                                --   -> propagation1 has a call to a function with a while loop which always exits with u == 30 
                                                --   -> propagation2 has a function which is called twice. Enabling call string context prevents poisoning between the calls. 
      liftedDriver      = Context.addContext k driver
      framework         = createFramework liftedDriver program  
      (open, closed)    = Analysis.runFramework $ framework     
      
  in putStrLn $ showResults liftedDriver closed

