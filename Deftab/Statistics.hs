module Deftab.Statistics
(   Statistics(..), StatisticsState, StatisticsStateIO,
    recordFiredRule, recordClosedBranch,

    printOutMetricsFinal, printOutMetrics,

    initialStatisticsStateFor,
    setPrintOutInterval,

    Metric, updateStep

) where

import Control.Monad.State(MonadState , MonadIO, modify, unless,
                           gets, when)

import qualified Control.Monad.State as State(liftIO)
import Control.DeepSeq ( NFData, rnf )

import Data.Map(Map)
import qualified Data.Map as Map(insertWith, toList, empty)
import Data.List ( intercalate )

import Deftab.RuleId(RuleId(..))

data Statistics = Stat{metrics::[Metric],
                       count::Int,
                       step::Int}

instance NFData Statistics where
 rnf (Stat sM sC sS) = rnf sM  `seq` rnf sC `seq`  rnf sS

type StatisticsState a   = forall m. (MonadState Statistics m) => m a
type StatisticsStateIO a = forall m. (MonadState Statistics m, MonadIO m) => m a

updateMetrics :: (Metric -> Metric) -> Statistics -> Statistics
updateMetrics f stat = let s = stat{metrics           = map (f $!) (metrics stat)}
                       in
                            rnf s `seq` s

updateStep :: Statistics -> Statistics
updateStep s@(Stat _  _ 0)   = s
updateStep stat                  = stat{count = count stat + 1}

needsToPrintOut :: Statistics -> Bool
needsToPrintOut (Stat _  _ 0)       = False
needsToPrintOut (Stat _  iter toi ) = iter > 0 && iter `mod` toi == 0

defaultStats :: Statistics
defaultStats = Stat{metrics=[closedBranches, ruleApplicationCount],
                    count=0, step=0}

---------- Monadic Statistics functions follow -------------


initialStatisticsStateFor :: (MonadState Statistics m) => (m a -> Statistics -> b)
                                                             -> m a -> b
initialStatisticsStateFor f = flip f defaultStats

setPrintOutInterval :: Int -> StatisticsState ()
setPrintOutInterval i = modify $ \s -> s{step = i}

recordFiredRule :: RuleId -> StatisticsState ()
recordFiredRule rule = modify (updateMetrics $ recordFiredRuleM rule)

recordClosedBranch :: StatisticsState ()
recordClosedBranch = modify (updateMetrics recordClosedBranchM)

printOutMetricsFinal :: Statistics -> IO ()
printOutMetricsFinal stats =
           liftIO $ printOutList (metrics stats)

printOutMetrics :: StatisticsStateIO ()
printOutMetrics = do  shouldPrint <- gets needsToPrintOut
                      when shouldPrint  $ do
                          liftIO $ putStr "(partial statistics: iteration "
                          iter <- gets count
                          liftIO . putStr . show $ iter
                          liftIO $ putStrLn ")"
                          ms <- gets metrics
                          liftIO $ printOutList ms


printOutList :: Show a => [a] -> IO ()
printOutList ms = unless ( null ms ) $ do
                          let separator = "\n----------------------------------\n"
                          let separate sep l = intercalate sep $ map show l
                          putStr separator
                          putStr (separate separator ms)
                          putStr separator

--------------------------------------------
-- Metrics
--------------------------------------------
data Metric = RC  (Map RuleId Int) -- Rule application count
             |CB  !Int             -- Number of closed branches

instance NFData Metric where
 rnf (CB b) = rnf b
 rnf (RC m) = rnf m

type MetricModificator = Metric -> Metric

instance Show Metric where
  show (CB  x)   = "Closed branches: " ++ show x
  show (RC  x)   = "Rule applications:" ++ concatMap p (Map.toList x)
      where p (i,c) = "\n  " ++ show i ++ " rule: " ++ show c


recordFiredRuleM :: RuleId -> MetricModificator
recordFiredRuleM rule (RC m) = RC (Map.insertWith (+) rule 1 m)
recordFiredRuleM _    m      = m


recordClosedBranchM :: MetricModificator
recordClosedBranchM (CB x) = CB (x+1)
recordClosedBranchM m      = m

ruleApplicationCount :: Metric
ruleApplicationCount = RC  Map.empty

closedBranches :: Metric
closedBranches = CB 0

--

liftIO :: (MonadIO m) => IO a -> m a
liftIO = State.liftIO
