{-# LANGUAGE RankNTypes, GADTs #-}

-- |(C) Wout Elsinghorst 2014

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

import Text.Read (readMaybe)

import Data.Set hiding ( foldr, map, null )
import Data.Map ( Map (..) )
import Data.Maybe
import Data.List

import Control.Monad
import Control.Applicative
import Control.Monad.State

import System.IO ( stdout, hFlush )

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

-- ----------------------------------------------------------------------------------------
-- |What's there, what isn't:
-- | - Lattice Embelishment by lifting a given lattice to one having call stack context
-- | - Fully Arithmetic Expression Analysis
-- | - Fully Implemented Constant Propagation Analysis
-- | - User interface to facilitate debugging
-- |    - Change call stack depth, selected analysis and test program on the fly
-- |    - Query analysis results per labeled program point 
-- | - Third analysis is missing, sorry
-- | - Everything else should be fully implemented and, to my knowledge, mostly bug-free
-- |
-- |
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
-- |    analysis'. Only AE and CP are currently fully implemented.
-- |  Framework.Analysis.Context: the addContext method enhances an existing framework
-- |    with bounded callstring history.
-- |
-- |  Examples.hs: contains the pre-written programs `propagation1` and `propagation2` 
-- |               which show constant propagation.
-- |
-- |  Main.hs (this file): Main entry point. Start the program via main and use the menu
-- |                       to do one of the following:
-- |                         1). Display settings  -- Show current configuration.
-- |                         2). Set program       -- Select which program to load.
-- |                         3). Set stack depth   -- Change call stack depth (default 0).
-- |                         4). Set analysis      -- Change analysis (only CP and AE are functional, default CP).
-- |                         5). Show program      -- Pretty print the currently selected program (includes labels).
-- |                         6). Show results      -- Show the results of the analysis. Results can be queried per labeled program point. 
-- |  The example "Constant Propagation Example 1" can be used to test how constants are propagated
-- |  through loops and function calls.
-- |  The example "Constant Propagation Example 2" can be used to test call stack contexts by
-- |  having (for example) the function 'testA' called twice with different parameters. Increasing
-- |  the depth to something > 0 each call to the function it's own context.
 
exampleList :: [(Program Lab, String, String)]
exampleList =
  [ (Examples.cp1, "cp1", "Constant Propagation Example 1")
  , (Examples.cp2, "cp2", "Constant Propagation Example 2")
  , (Examples.ae1, "ae1", "Arithmetic Expression Example 1")
  ]
 
data Config = Config { stackDepth :: Int, currentProgram :: Maybe (Program Lab, String), currentAnalysis :: Maybe (Closed Package, String), currentResult :: Maybe Result }
 
type Interface a = StateT Config IO a
 
chooseProgram:: Interface (Program Lab)
chooseProgram =
  do lift $ putStrLn $ "Please select one of the following programs: "
     let overview = zipWith (\k (p, name, desc) -> (k, p, desc)) [1..] $ exampleList
     
     forM overview $ \(k, _, desc) ->
       lift $ putStrLn $ "  " ++ show k ++ "). " ++ desc

     
     
     let validator = do lift $ putStr $ ">> "
                        lift $ hFlush stdout
                        input <- lift getLine
                        case readMaybe input :: Maybe Int of
                             Just n  -> case find (\(k, p, desc) -> k == n) overview of
                                             Just (_, p, desc) -> do modify $ \config -> config { currentProgram = Just (p, desc) }
                                                                     lift $ putStrLn $ "\nOK! Program set to " ++ desc ++ "\n" 
                                                                     return p
                                             Nothing -> do lift $ putStrLn $ "Error: entry " ++ show n ++ " not in available."
                                                           validator
                             Nothing -> do lift $ putStrLn $ "Error: input not a number"
                                           validator
                        
     
     validator

data Closed m = forall r . (Eq r, Show r) => Closed { open :: m r }
     
data Result = forall r . (Eq r, Show r) => Result { result :: (Package r, Analysis r) } 
     
analysisList :: [(Closed Package, String, String)]
analysisList =
  [ (Closed CP.driver, "cp", "Constant Propagation")
  , (Closed AE.driver, "ae", "Arithmetic Expressions")
  ]
     
chooseAnalysis :: Interface (Closed Package)
chooseAnalysis = 
  do lift $ putStrLn $ "Please select one of the following analyses: "
     let overview = zipWith (\k (driver, name, desc) -> (k, driver, desc)) [1..] $ analysisList
     
     forM overview $ \(k, _, desc) ->
       lift $ putStrLn $ "  " ++ show k ++ "). " ++ desc

     
     
     let validator = do lift $ putStr $ ">> "
                        lift $ hFlush stdout
                        input <- lift getLine
                        case readMaybe input :: Maybe Int of
                             Just n  -> case find (\(k, _, _) -> k == n) overview of
                                             Just (_, driver, desc) -> do modify $ \config -> config { currentAnalysis = Just (driver, desc) }
                                                                          lift $ putStrLn $ "\nOK! Analysis set to " ++ desc ++ "\n"
                                                                          return driver
                                             Nothing -> do lift $ putStrLn $ "Error: entry " ++ show n ++ " not in available."
                                                           validator
                             Nothing -> do lift $ putStrLn $ "Error: input not a number."
                                           validator
     validator
chooseStackDepth :: Interface Int
chooseStackDepth =
  do lift $ putStrLn $ "Enter stack depth:\n"
     lift $ hFlush stdout
     
     let validator = do lift $ putStr $ ">> "
                        lift $ hFlush stdout
                        input <- lift getLine
                        case readMaybe input :: Maybe Int of
                             Just n  -> do modify $ \config -> config { stackDepth = n }
                                           lift $ putStrLn $ "\nOK! Stack depth set to " ++ show n ++ "\n"
                                           return $ n
                             Nothing -> do lift $ putStrLn $ "Error: input not a number."
                                           validator
     validator       
     
displaySettings :: Interface ()
displaySettings =
  do config <- get
  
     lift $ putStrLn $ "Current settings: "
     lift $ putStr   $ "  Stack depth: "
     case stackDepth config of
          n              -> lift $ putStrLn $ show n
     lift $ putStr   $ "  Analysis: "
     case currentAnalysis config of
          Just (_, desc) -> lift $ putStrLn $ desc
          Nothing        -> lift $ putStrLn $ "<no analysis set>"
     lift $ putStr   $ "  Program: "
     case currentProgram config of
          Just (_, desc) -> lift $ putStrLn $ desc
          Nothing        -> lift $ putStrLn $ "<no program set>"
     lift $ putStrLn $ ""
     
     
runAnalysis :: Interface (Maybe Result)
runAnalysis = 
  do config <- get
     
     let depth = stackDepth config
         analysis = currentAnalysis config
         program = currentProgram config
  
         runner = do (Closed anal, _) <- analysis
                     (prog,        _) <- program

                     
                     let liftedDriver      = Context.addContext depth anal
                         framework         = createFramework liftedDriver prog 
                         (open, closed)    = Analysis.runFramework $ framework
                         
                     return $ Result (liftedDriver, closed)
     case runner of
          Just result@(Result (driver, stuff))  -> do modify $ \config -> config { currentResult = Just result }
                                                      return $ Just result
          Nothing -> do lift $ putStrLn $ "Error: one or more settings not set."
                        return $ Nothing

displayProgram :: Interface () -> Interface ()
displayProgram callback =
  do config <- get
     let prog = currentProgram config
     case prog of
          Just (program, desc) -> do lift $ putStrLn $ "Current program AST:\n"
                                     lift $ putStrLn $ show program ++ "\n"
          Nothing -> lift $ putStrLn $ "Error: no program loaded."
     callback
  
displayResults :: Interface ()
displayResults =
  do let validator :: Result -> Interface ()
         validator result@(Result (driver, stuff)) = 
           do lift $ putStrLn $ "Which label do you want to show? (press r to return to main menu or s to show current program)"
              lift $ putStr $ ">> "
              lift $ hFlush stdout
              input <- lift getLine
              case readMaybe input :: Maybe Int of
                    Just n  -> case Data.Map.lookup n stuff of
                                    Just v  -> do lift $ putStrLn $ "Analysis result for label " ++ show n ++ ":\n"
                                                  lift $ putStrLn $ showResult driver v
                                                  validator result
                                    Nothing -> do lift $ putStrLn $ "Error: invalid label."
                                                  validator result
                    Nothing -> case input of
                                    "r"  -> userInterface 
                                    "s"  -> displayProgram (validator result)
                                    _    -> do lift $ putStrLn $ "Error: invalid option."
                                               validator result
  
     config <- get
     case currentResult config of
          Nothing -> do stuff <- runAnalysis 
                        case stuff of
                          Nothing -> userInterface
                          Just r  -> validator r
          Just r -> validator r
     
     
invalidator :: Interface a -> Interface ()
invalidator m = do _ <- m
                   modify $ \config -> config { currentResult = Nothing }
                   
mainMenu :: [(Interface (), String)]
mainMenu =
  [ (do () <- displaySettings;              return (), "Display settings")
  , (do () <- invalidator chooseProgram;    return (), "Set program")
  , (do () <- invalidator chooseStackDepth; return (), "Set stack depth")
  , (do () <- invalidator chooseAnalysis;   return (), "Set analysis")
  , (do () <- displayProgram userInterface; return (), "Show program")
  , (do () <- displayResults;               return (), "Show results")
  ]
     
userInterface :: Interface ()
userInterface =
  do lift $ putStrLn $ "Please select one of the following options: "
     let overview = zipWith (\k (menu, desc) -> (k, menu, desc)) [1..] $ mainMenu
     
     forM overview $ \(k, _, desc) ->
       lift $ putStrLn $ "  " ++ show k ++ "). " ++ desc

     
     
     let validator = do lift $ putStr $ ">> "
                        lift $ hFlush stdout
                        input <- lift getLine
                        case (readMaybe input :: Maybe Int) of
                             Just n  -> case find (\(k, _, _) -> k == n) overview of
                                             Just (_, submenu, _) -> do () <- submenu
                                                                        userInterface
                                             Nothing -> do lift $ putStrLn $ "Error: entry " ++ show n ++ " not in available."
                                                           validator
                             Nothing -> do lift $ putStrLn $ "Error: input not a number."
                                           validator
     validator

safeIndex :: Int -> [a] -> Maybe a
safeIndex 0 [] = Nothing
safeIndex 0 (x:_) = Just x
safeIndex k (x:xs) = safeIndex (k-1) xs
     
defaultConfig :: Config
defaultConfig = 
  Config { 
    stackDepth = 0, 
    currentProgram = safeIndex 1 . map (\(p, _, desc) -> (p, desc)) $ exampleList,
    currentAnalysis = safeIndex 0 . map (\(p, _, desc) -> (p, desc)) $ analysisList,
    currentResult = Nothing
  }
     
main :: IO ()
main =
  do let config = defaultConfig
     _ <- runStateT userInterface config
     return ()
 
