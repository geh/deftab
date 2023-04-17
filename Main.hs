{-# LANGUAGE CPP #-}
module Main
   ( main, runWithParams )

where

import Control.Monad ( when, unless, forM_ )
import Control.Monad.State( runStateT )
import Data.Maybe ( isNothing )
import System.CPUTime( getCPUTime )
import qualified System.Timeout as T
import System.IO.Strict ( readFile )
import Prelude hiding ( readFile )
import System.Console.CmdArgs
   ( whenNormal, whenLoud, details, summary
   , (+=), cmdArgs_ )
import Deftab.CommandLine
   ( defaultParams, checkParams, filename, timeout
   , Params(strategy, cache), reflexive, transitive, no_facts, no_defaults, bench
   , strategy1, strategy2
   )
import Deftab.Branch
   ( BranchInfo(..), initialBranch)
import Deftab.Statistics
   ( Statistics, initialStatisticsStateFor, printOutMetricsFinal )
import Deftab.Tableau
   ( OpenFlag(..), tableauStart )
import Deftab.Formula
   ( LanguageInfo(..), Formula, conj, Language(..) )
import qualified Deftab.Formula as F

import Paths_deftab (version)
import Data.Version (showVersion)

main :: IO ()
main = do clp  <- cmdArgs_ $ defaultParams += summary (unlines header) += details license
          clpOK <- checkParams clp
          when ( clpOK ) $ runWithParams clp

header, license :: [String]
header  = ["Deftab " ++ showVersion version
          ,"Compiled on " ++ __DATE__ ++ ", at " ++ __TIME__
          , "http://tinyurl.com/deftab0"]
license = ["BSD3"]

runWithParams :: Params -> IO ()
runWithParams p = do
  i <- readFile (filename p)
  let (lang,fLang,fs,ds,c) = F.parseInput i
  let inTimeout 0 action = Just <$> action
      inTimeout t action = T.timeout (t * (10::Int)^(6::Int)) action
  if bench p
   then forM_ [ (p{strategy = s, cache=b}, "\nStrategy: " ++ show s ++ " cache: " ++ show b)
                      | s <- [strategy2,strategy1], b <- [True,False]  ] $
         \(par,msg) -> time $ do
           putStrLn msg
           result1 <- inTimeout (timeout p) (runTask lang fLang fs ds c par)
           when (isNothing result1) $ myPutStrLn "\nTimeout.\n"
  else time $ do
           result <- inTimeout (timeout p) (runTask lang fLang fs ds c p)
           when (isNothing result) $ myPutStrLn "\nTimeout.\n"

runTask :: Language -> LanguageInfo -> [Formula] -> [F.Rule] -> [Formula] -> Params -> IO OpenFlag
runTask lang fLang fs ds csq p =
 do unless (bench p) $
     myPutStrLn $ unlines $
       [ "Language: " ++ show lang
       , "Facts:"]
       ++ (if no_facts p
           then ["Facts disabled by commandline flag."]
           else if null fs
                  then ["No Facts in input file."]
                  else map (("  " ++) . show) fs )
       ++
       ["", "Defaults:"]
       ++ (if no_defaults p
           then ["Defaults disabled by commandline flag."]
           else if null ds
                  then ["No Defaults in input file."]
                  else map (("  " ++) . show) ds)
       ++
       ["","Consequence:"]
       ++  map (("  " ++) . show) csq
    --
    let p' = case lang of Classical -> p ; Intuitionistic -> p{reflexive=True, transitive=True}
    let ds' = if no_defaults p then [] else ds
    let fs' = if no_facts p    then [] else fs
    (result,stats) <- tableauInit p'
      $ initialBranch p' lang fLang (foldr conj F.taut fs') ds' (Just (foldr conj F.taut csq))
    --
    whenNormal $ printOutMetricsFinal stats
    --
    case result of
       OPEN ext  | null ds' -> myPutStrLn "Not a sceptical consequence."
                 | otherwise
                    -> do myPutStrLn "Not a sceptical consequence, found bad extension:"
                          myPutStrLn $ show ext
       CLOSED _ _ -> myPutStrLn "Indeed a sceptical consequence."
    return result

tableauInit :: Params -> BranchInfo -> IO (OpenFlag,Statistics)
tableauInit p bi =
        do whenLoud $ putStrLn ">> Starting tableau rules application"
           initStatsState $ tableauStart p bi
 where initStatsState  = initialStatisticsStateFor runStateT

time :: IO a -> IO a
time action =
  do start  <- getCPUTime
     result <- action
     end <- getCPUTime
     let elapsedTime = fromInteger (end - start) / 1000000000000.0
     myPutStrLn $ "Elapsed time: " ++ show (elapsedTime :: Double)
     return result

myPutStrLn :: String -> IO ()
myPutStrLn str = whenNormal $ putStrLn str

