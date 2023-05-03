module Deftab.Tableau
 (OpenFlag(..), tableauStart)
where

import System.Console.CmdArgs ( whenLoud )

import Control.Monad.State(StateT,lift,modify, modify)
import Deftab.Statistics(Statistics,updateStep,printOutMetrics,
                       recordClosedBranch,recordFiredRule)
import Deftab.Branch(BranchInfo(..), detachedDefaults, subTabCache
                    , SubTabCache, emptyCache, insertCache)
import Deftab.CommandLine(backjumping,Params,configureStats)
import Deftab.Rules(applyRule,applicableRule,ruleToId)
import Data.List ( nub )
import Deftab.Formula(Formula,Rule(conseq),DependencySet,dsEmpty,dsMember,dsUnion)

type Depth = Int
type TableauMonad a = StateT Statistics IO a
data OpenFlag = OPEN [Formula] -- TODO open branches could keep subtableaux cache
              | CLOSED DependencySet SubTabCache

tableauStart :: Params -> BranchInfo -> TableauMonad OpenFlag
tableauStart p bi = configureStats p >> tableauDown p 0 bi

tableauDown :: Params -> Depth -> BranchInfo -> TableauMonad OpenFlag
tableauDown p depth branchInfo =
      do let verbose = lift . whenLoud . putStrLn
         printOutMetrics
         modify updateStep
         verbose $ ">> Depth " ++ show depth
         case branchInfo of
            BranchClash br pr bprs f ->
             do verbose $ show br ++ "Clasher : " ++ show (pr,bprs,f)
                recordClosedBranch
                return $ CLOSED bprs (subTabCache br)
            BranchOK br ->
             do verbose (show br)
                case applicableRule br p (depth + 1) of -- updates subTabCache
                  Nothing  ->
                      do verbose ">> Saturated open branch"
                         return $ OPEN (nub $ map (conseq.fst) $ detachedDefaults br)
                  Just (rule,newBranch)  ->
                      do verbose $ ">> Rule : " ++ show rule
                         recordFiredRule $ ruleToId rule
                         let bis = applyRule p rule newBranch
                         case bis of
                          [newBi] -> tableauDown  p (depth + 1) newBi
                          (_:_:_) -> tableauRight p (depth + 1) bis dsEmpty Nothing
                          []       -> undefined

tableauRight :: Params -> Depth -> [BranchInfo] -> DependencySet -> Maybe SubTabCache
             -> TableauMonad OpenFlag
tableauRight p depth (hd:tl) currentDepSet mstc =
 do res <- case mstc of
             Nothing  -> tableauDown p depth hd
             Just stc -> tableauDown p depth (insertCache stc hd)
    case res of
     o@(OPEN _)    -> return o
     CLOSED depSet stc ->
         if backjumping p && not (dsMember depth depSet)
          then return $ CLOSED depSet stc
          else tableauRight p depth tl (dsUnion currentDepSet depSet) (Just stc)

tableauRight _ _ [] currentDepSet mstc =
 return $ CLOSED currentDepSet $ case mstc of Nothing -> emptyCache ; Just c -> c

-- credulous tableau
-- if an open saturated branch is found, continue until a closed branch is found
-- if all 

tableauDownCred :: Params -> Depth -> BranchInfo -> TableauMonad OpenFlag
tableauDownCred p depth branchInfo =
      do let verbose = lift . whenLoud . putStrLn
         printOutMetrics
         modify updateStep
         verbose $ ">> Depth " ++ show depth
         case branchInfo of
            BranchClash br pr bprs f ->
             do verbose $ show br ++ "Clasher : " ++ show (pr,bprs,f)
                recordClosedBranch
                return $ CLOSED bprs (subTabCache br)
            BranchOK br ->
             do verbose (show br)
                case applicableRule br p (depth + 1) of -- updates subTabCache
                  Nothing  ->
                      do verbose ">> Saturated open branch"
                         return $ OPEN (nub $ map (conseq.fst) $ detachedDefaults br)
                  Just (rule,newBranch)  ->
                      do verbose $ ">> Rule : " ++ show rule
                         recordFiredRule $ ruleToId rule
                         let bis = applyRule p rule newBranch
                         case bis of
                          [newBi] -> tableauDownCred  p (depth + 1) newBi
                          (_:_:_) -> tableauRightCred p (depth + 1) bis dsEmpty Nothing
                          []       -> undefined


-- TODO actually we don't need backjumping and dependency handling
tableauRightCred :: Params -> Depth -> [BranchInfo] -> DependencySet -> Maybe SubTabCache
                 -> TableauMonad OpenFlag
tableauRightCred p depth (hd:tl) currentDepSet mstc =
 do res <- case mstc of
             Nothing  -> tableauDownCred p depth hd
             Just stc -> tableauDownCred p depth (insertCache stc hd)
    case res of
     OPEN _         -> -- keep searching for a closed branch
          tableauRight p depth tl currentDepSet Nothing
     c@(CLOSED _ _) -> -- closed branch found == success
          return c

tableauRightCred _ _ [] _ _ = return $ OPEN []
