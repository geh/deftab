{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Deftab.CommandLine (
   Params(..), defaultParams, configureStats, checkParams
 , strategy1, strategy2
) where

import System.Console.CmdArgs
import Data.List ( sort )
import Deftab.Statistics( StatisticsState, setPrintOutInterval )

data Params = Params {
           filename        :: FilePath
         , timeout         :: Int
         , stats           :: Int
         , strategy        :: String
         , backjumping     :: Bool
         , cache           :: Bool
         , no_defaults     :: Bool
         , no_facts        :: Bool
         , reflexive       :: Bool
         , transitive      :: Bool
         , bench           :: Bool
         , sceptical       :: Bool
         } deriving (Show, Data, Typeable)

defaultParams :: Annotate Ann
defaultParams
 = record Params{}
     [ filename      := "" += name "f" += typFile += help "input file"
     , timeout       := 0       += name "t" += help "timeout (in seconds, default=none)"
     , stats         := 0       += help "display statistics every n steps (default=none)"
     , strategy      := strategy1 += help "specify rules strategy"
     , backjumping   := True    += help "enable backjumping (enabled by default)"
     , cache         := True    += help "enable subtableaux caching (enabled by default)"
     , no_defaults   := False   += help "ignore defaults of input file"
     , no_facts      := False   += help "ignore facts of input file"
     , reflexive     := False   += help "for classical input, set models as reflexive"
     , transitive    := False   += help "for classical input, set models as transitive"
     , bench         := False   += help "run different strategies on input and report times"
     , sceptical     := True    += help "check for sceptical consequence; set --sceptical=False for credulous"
     ] += verbosity

strategy1, strategy2 :: String
strategy1 = "n@E<|bd"
strategy2 = "n@E|<bd"

checkParams :: Params -> IO Bool
checkParams p
 | strategy p `notPermutationOf` strategy1 =
    do putStrLn
        $ unlines ["ERROR"
                  , "strategy should contain all of the following characters: "
                  , "  n = nominals"
                  , "  @ = satisfaction operator"
                  , "  E = existential modality "
                  , "  < = diamond (also (-)-implication, (-)-neg) "
                  , "  | = or (also (-)-conjunction, (+)-implication)"
                  , "  b = down-arrow binder"
                  , "  d = defaults"
                  , ""
                  , "The default is \"" ++ strategy1 ++ "\""
                  , "The rules conjunction, (-)-disjunction,"
                  , "box, (+)-negation, universal modality"
                  , "are applied immediately, thus do not belong to the strategy."]
       return False
 | null (filename p) =
    do putStrLn $ unlines ["ERROR: No input specified.","Run with --help for usage options"]
       return False
 | otherwise = return True
  where notPermutationOf l1 l2 = sort l1 /= sort l2

configureStats :: Params -> StatisticsState ()
configureStats p = setPrintOutInterval $ stats p
