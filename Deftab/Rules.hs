module Deftab.Rules
(
Rule(..),
applicableRule, applyRule, ruleToId
) where

import qualified Data.Set as Set
import Data.Maybe ( mapMaybe )
import qualified Data.ByteString.Char8 as C

import Deftab.Formula( Formula(..), PrFormula(..), showLess,
                     Dependency, DependencySet, dsUnion, dsInsert,
                     prefix,
                     Prefix, Nom, Atom(..),
                     replaceVar)
import qualified Deftab.Formula as F
import Deftab.Branch( Branch(..), BranchInfo(..), TodoList(..),
                    -- for rules
                    createNewNode, createNewNom,
                    addFormulas, addAccFormula,
                    addDiaRuleCheck, addToBlockedDias,
                    addDownRuleCheck,
                    addImpRuleCheck, addNegRuleCheck,
                    getUrfatherAndDeps, merge,
                    positiveNomOf,
                    -- for choosing rule in todo list
                    patternBlocked, intPatternBlocked,
                    diaAlreadyDone, downAlreadyDone,
                    -- defaults
                    checkGrounded, checkBlocked, checkEnabled,
                    enableDefault
                    )
import Deftab.CommandLine(Params, strategy )
import Deftab.RuleId(RuleId(..))
import qualified Deftab.DisjSet as DS

-- rule constructors contain the data needed to modify a branch

data Rule =  DiaRule    PrFormula
           | ExistRule  PrFormula
           | DisjRule   PrFormula Dependency
           | AtRule     PrFormula
           | DownRule     PrFormula
           | DiscardDownRule PrFormula
           | DiscardDiaDoneRule PrFormula
           | DiscardDiaBlockedRule PrFormula
           | MergeRule Prefix Nom DependencySet
           | NegRule    PrFormula
           | TImpRule    PrFormula Dependency
           | FImpRule    PrFormula
           | TDimpRule   PrFormula Dependency
           | FDimpRule   PrFormula Dependency
           | DefaultRule [(F.Rule,DependencySet)] Dependency

instance Show Rule where
   show (MergeRule pr n _)                = "merge:              " ++ show (pr, show n)
   show (DiaRule   todelete )             = "diamond:            " ++ showLess todelete
   show (DisjRule  todelete _)            = "disjunction:        " ++ showLess todelete
   show (AtRule    todelete )             = "at:                 " ++ showLess todelete
   show (DownRule  todelete )             = "down:               " ++ showLess todelete
   show (ExistRule todelete )             = "E:                  " ++ showLess todelete
   show (DefaultRule rules _)             = "Defaults:           " ++ show (map fst rules)

   show (DiscardDownRule todelete)        = "Discard:            " ++ showLess todelete
   show (DiscardDiaDoneRule todelete)     = "Discard done:       " ++ showLess todelete
   show (DiscardDiaBlockedRule todelete)  = "Discard blocked:    " ++ showLess todelete
   show (NegRule   todelete )             = "False Neg rule:     " ++ showLess todelete
   show (TImpRule  todelete _)            = "True Impl. rule:    " ++ showLess todelete
   show (FImpRule  todelete )             = "False Impl. rule:   " ++ showLess todelete
   show (TDimpRule  todelete _)           = "True Dimpl. rule:   " ++ showLess todelete
   show (FDimpRule  todelete _)           = "False Dimpl. rule:  " ++ showLess todelete


ruleToId :: Rule -> RuleId
ruleToId r = case r of
              (MergeRule _ _ _)          -> R_Merge
              (DiaRule _)                -> R_Dia
              (DisjRule _ _)             -> R_Disj
              (AtRule _ )                -> R_At
              (DownRule _ )              -> R_Down
              (ExistRule _)              -> R_Exist
              (DefaultRule _ _)          -> R_Default
              (NegRule _)                -> R_Neg
              (TImpRule _ _)             -> R_TImp
              (FImpRule _)               -> R_FImp
              (TDimpRule _ _)            -> R_TDimp
              (FDimpRule _ _)            -> R_FDimp
              (DiscardDownRule _)        -> R_DiscardDown
              (DiscardDiaDoneRule _)     -> R_DiscardDiaDone
              (DiscardDiaBlockedRule _)  -> R_DiscardDiaBlocked

-- the rules application strategy is defined here:
-- the first rule is the one that will be applied at the next tableau step
applicableRule :: Branch -> Params -> Dependency -> Maybe (Rule,Branch)
applicableRule br p d =
 case mapMaybe (ruleByChar br p d) (strategy p) of
   [] -> Nothing
   ((rule,newBr):_) -> Just (rule, newBr)

ruleByChar :: Branch -> Params -> Dependency -> Char -> Maybe (Rule,Branch)
ruleByChar br p d char =
 case char of
  'n' -> applicableMergeRule
  '|' -> applicableDisjRule
  '<' -> applicableDiaRule
  '@' -> applicableAtRule
  'E' -> applicableExistRule
  'b' -> applicableDownRule
  'd' -> applicableDefaultRule
  _   -> error "ruleByChar"
 where
  todos  = todoList br

  applicableDiaRule
   = do (f@(PrFormula pr ds _ f') ,new) <- Set.minView $ diaTodo todos
        if diaAlreadyDone br f
          then return ( DiscardDiaDoneRule f, br{todoList = todos{diaTodo = new}})
          else
           case f' of
            (Dia _)
              | patternBlocked p br f -> return ( DiscardDiaBlockedRule f, br{todoList = todos{diaTodo = new}})
              | otherwise -> return ( DiaRule f, br{todoList = todos{diaTodo = new}})
            (Imp _ _)
              | intPatternBlocked br f -> return (DiscardDiaBlockedRule f,  br{todoList = todos{diaTodo = new}})
              | otherwise -> return (FImpRule f,  br{todoList = todos{diaTodo = new}})
            (Dimp f1 f2)
              |    intPatternBlocked br (PrFormula pr ds False (Imp f1 f2))
                || intPatternBlocked br (PrFormula pr ds False (Imp f2 f1))
                          -> return (DiscardDiaBlockedRule f,  br{todoList = todos{diaTodo = new}})
              | otherwise -> return (FDimpRule f d,  br{todoList = todos{diaTodo = new}})
            (Neg _)
              | intPatternBlocked br f -> return (DiscardDiaBlockedRule f,  br{todoList = todos{diaTodo = new}})
              | otherwise -> return (NegRule f,  br{todoList = todos{diaTodo = new}})
            _ -> error $ "Impossible: " ++ show f'
            
  applicableAtRule    = do (f,new) <- Set.minView $ atTodo todos
                           return (AtRule f, br{todoList = todos{atTodo = new}})

  applicableDownRule  = do (f,new) <- Set.minView $ downTodo todos
                           if downAlreadyDone br f
                            then return (DiscardDownRule f, br{todoList = todos{downTodo = new}})
                            else return (DownRule f, br{todoList = todos{downTodo = new}})

  applicableExistRule = do (f,new) <- Set.minView $ existTodo todos
                           return (ExistRule f, br{todoList = todos{existTodo = new}})

  applicableMergeRule  = do ((ds,pr,n),new) <- Set.minView $ mergeTodo todos
                            return (MergeRule pr n ds, br{todoList = todos{mergeTodo = new}})
  applicableDisjRule
   = do (f@(PrFormula _ _ b f') ,new) <- Set.minView $ disjTodo todos
        case (b,f') of
          (True, Imp _ _)   -> return (TImpRule f d, br{todoList = todos{disjTodo = new}})
          (True, Dis _)     -> return (DisjRule f d, br{todoList = todos{disjTodo = new}})
          (False, Con _)    -> return (DisjRule f d, br{todoList = todos{disjTodo = new}})
          (True, Dimp _ _)  -> return (TDimpRule f d, br{todoList = todos{disjTodo = new}})
          _               -> error $ "applicableDisjRule: " ++ show f

  applicableDefaultRule
   -- 1. for all available default, check if grounded
   --    if grounded, delete from available list, add to grounded list
   = let (newGrounded, newAvailable,newCache) = checkGrounded p br applicableRule applyRule
         br2 = br{availableDefaults = newAvailable
                 , groundedDefaults = newGrounded ++ (groundedDefaults br)
                 , subTabCache = newCache}
   -- 2. for all grounded default, check if blocked
   --    if blocked, delete from grounded list
         (newGrounded2,newCache2) = checkBlocked p br2 applicableRule applyRule
         br3 = br2{groundedDefaults = newGrounded2
                  , subTabCache = newCache2}
   -- 3. for all grounded rules, check the one whose enablement is possible
   --    branch on them applying the (Default) rule
         (enabledRules,newCache3) = checkEnabled p br3 applicableRule applyRule
     in
       if null enabledRules
        then Nothing
        else Just (DefaultRule enabledRules d, br3{subTabCache = newCache3})
       
(>>?) :: BranchInfo -> (Branch -> BranchInfo) -> BranchInfo
clash@(BranchClash _ _ _ _) >>? _ = clash
(BranchOK br) >>? f = f br

applyRule :: Params -> Rule -> Branch -> [BranchInfo]
applyRule p rule br
 = case rule of
    DiaRule (PrFormula pr ds True (Dia f))
     ->     [ createNewNode p br >>?
              addAccFormula p (dsUnion ds ds2, ur, newPr) >>?
              addFormulas p [PrFormula newPr ds True f] >>?
              addDiaRuleCheck p pr f newPr
            ]
      where newPr      = lastPref br + 1
            (ur,ds2,_) = getUrfatherAndDeps br (DS.Prefix pr)

    NegRule (PrFormula pr ds False (Neg f))
     ->     [ createNewNode p br >>?
              addAccFormula p (dsUnion ds ds2, ur, newPr) >>?
              addFormulas p [PrFormula newPr ds True f] >>?
              addNegRuleCheck pr f newPr
            ]
      where newPr      = lastPref br + 1
            (ur,ds2,_) = getUrfatherAndDeps br (DS.Prefix pr)

    FImpRule (PrFormula pr ds False (Imp f1 f2))
     ->     [ createNewNode p br >>?
              addAccFormula p (dsUnion ds ds2, ur, newPr) >>?
              addFormulas p [PrFormula newPr ds True f1, PrFormula newPr ds False f2] >>?
              addImpRuleCheck pr (f1,f2) newPr
            ]
      where newPr      = lastPref br + 1
            (ur,ds2,_) = getUrfatherAndDeps br (DS.Prefix pr)

    ExistRule (PrFormula _ ds _ (E f2))
     -> [createNewNode p br >>? addFormulas p [PrFormula newPr ds True f2]]
      where newPr = lastPref br + 1
    DisjRule (PrFormula pr ds True (Dis fs)) d
     -> [ addFormulas p [toadd] br | toadd <- prefix pr (dsInsert d ds) True fs]
    DisjRule (PrFormula pr ds False (Con fs)) d
     -> [ addFormulas p [toadd] br | toadd <- prefix pr (dsInsert d ds) False fs]
    TImpRule (PrFormula pr ds True (Imp f1 f2)) d
     -> [ addFormulas p [toadd] br
           | toadd <- [PrFormula pr ds' False f1, PrFormula pr ds' True f2] ]
        where ds' = dsInsert d ds
    TDimpRule (PrFormula pr ds True (Dimp f1 f2)) d
     -> [ addFormulas p toadds br
           | toadds <- [ [PrFormula pr ds' True f1 , PrFormula pr ds' True  f2]
                       , [PrFormula pr ds' False f1, PrFormula pr ds' False f2] ]
        ]
        where ds' = dsInsert d ds
    FDimpRule (PrFormula pr ds False (Dimp f1 f2)) d
     ->     [ createNewNode p br >>?
              addAccFormula p (dsUnion ds ds'', ur, newPr) >>?
              addFormulas p toadds >>?
              addImpRuleCheck pr (rc1,rc2) newPr -- according to choice made, remember we satisfied -(f1 -> f2) or -(f2 -> f1)
            | (toadds,rc1,rc2) <- [ ([PrFormula newPr ds'' True f1 , PrFormula newPr ds'' False f2],f1,f2)
                                  , ([PrFormula newPr ds'' False f1, PrFormula newPr ds'' True  f2],f2,f1)]
            ]
      where newPr      = lastPref br + 1
            (ur,ds',_) = getUrfatherAndDeps br (DS.Prefix pr)
            ds'' = dsInsert d ds'
    AtRule  (PrFormula _ ds _ (At n f)) ->
            [ addFormulas p [toadd] br{ nomPrefClasses = equiv }]
            where (ur,ds2,equiv) = getUrfatherAndDeps br (DS.Nominal n)
                  toadd = PrFormula ur (dsUnion ds ds2) True f
    DownRule (PrFormula pr ds _ f@(Down v f2)) ->
                 case positiveNomOf br pr of -- reuse positive nominal if we can
                  Just n' ->
                     [ addFormulas p [PrFormula pr ds True (replaceVar v n' f2)] br >>?
                       addDownRuleCheck pr f ]
                  Nothing ->   [ createNewNom br >>?
                                 addFormulas p [toadd1, toadd2] >>?
                                 addDownRuleCheck pr f ]
                    where toadd1 = PrFormula pr ds True (replaceVar v newNom f2)
                          toadd2 = PrFormula pr ds True $ Atm $ N newNom
                          newNom = C.pack ('_':(show $ nextNom br))
    DiscardDownRule _         -> [BranchOK br]
    DiscardDiaDoneRule _      -> [BranchOK br]
    DiscardDiaBlockedRule f   -> [addToBlockedDias f br]
    MergeRule pr n ds -> [merge p pr ds n br]
    DefaultRule rules d -> [enableDefault p d (r,ds) br | (r,ds) <- rules]

    _ -> error $ "applyRule with bad argument: " ++ show rule

