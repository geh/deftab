module Deftab.Branch
(
Branch(..), BranchInfo(..), TodoList(..),
createNewNode, createNewNom,
addFormulas, addAccFormula,
addToBlockedDias,
addDiaRuleCheck, addNegRuleCheck, addImpRuleCheck,
addDownRuleCheck,
initialBranch,
merge,
getUrfather, getUrfatherAndDeps,
patternBlocked, intPatternBlocked,
diaAlreadyDone,
downAlreadyDone,
patternOf, findByPattern,
prefixes, positiveNomOf,
checkGrounded, checkBlocked,
enableDefault, SubTabCache, emptyCache,
insertCache
) where

import Data.Maybe( mapMaybe, isJust )

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.IntMap.Strict ( IntMap)
import qualified Data.IntMap.Strict as I
import qualified Data.IntSet as IntSet
import Data.List ( delete )
import Data.Either ( partitionEithers )
import qualified Data.ByteString.Char8 as C

import qualified Deftab.DMap as D
import qualified Deftab.DisjSet as DS
import Deftab.CommandLine(Params(..))
import Deftab.Formula hiding ( list )
import Deftab.Relations ( OutRels, emptyRels, insertRelation, mergePrefixes,
                   successors, showRels )
import Deftab.Literals ( UpdateResult(..), Literals,
                  SlotUpdateResult(..),
                  updateMap, lsUnions, lsAddDeps,
                  positiveNom )

data BranchInfo = BranchOK Branch |
                  BranchClash Branch Prefix DependencySet Formula

type BoxConstraints = IntMap {- Prefix -} (Map Formula DependencySet)
type EquivClasses = DS.DisjSet DS.Pointer
type SubTabCache = Map (Formula, Maybe Formula) Bool
                        -- True: tableau known to be open
                        -- False: known to be closed

data Branch =
              Branch {
                 -- premodel
                      literals :: Literals,
                        accStr :: OutRels,
                 -- local constraints
                     -- Classical box
                        boxFwd :: BoxConstraints,
                     -- Intuitionistic positive negation
                        intNeg :: BoxConstraints, 
                     -- Intuitionistic positive implication
                        intImp :: BoxConstraints,
                 -- global constraints
                      univCons :: [(DependencySet,Formula)],
                 -- pending formulas / todo lists
                      todoList :: TodoList,
                 -- rules saturation
                       diaRlCh :: IntMap (Set Formula),
                      downRlCh :: IntMap (Set Formula),
                        atRlCh :: Set (Nom,Formula),
                     existRlCh :: Set Formula,
                       impRlCh :: IntMap (Set (Formula,Formula)),
                       negRlCh :: IntMap (Set Formula),
                 -- pattern blocking
                      patterns :: IntMap (Set Formula),
                   intPatterns :: IntMap (Set (Bool,Formula)),
                 -- backjumping data attached to equivalence classes
                    prToDepSet :: IntMap DependencySet,
                 -- prefix/nominal equivalence classes
                nomPrefClasses :: EquivClasses,
                 -- book keeping
                      lastPref :: Prefix,
                       nextNom :: Int,
                 -- information about language of input formula and blocking
                 inputLanguage :: LanguageInfo,
                   blockedDias :: IntMap [PrFormula],
                 -- facts
                         facts :: Formula,
                 -- defaults
             availableDefaults :: [Rule],
              groundedDefaults :: [(Rule,DependencySet)], -- whose prereq is proved
              detachedDefaults :: [(Rule,DependencySet)], -- consequent is added to branch and justification shall not be blocked
                 -- cache
                   subTabCache :: SubTabCache,
                 -- language
                      language :: Language
}

--

instance Show Branch where
 show br = concat $
  [  "\nLiterals:", showIMap (\v -> "(" ++ showMap_lits v ++ ")") "\n " (literals br),
     "\nRelations:", showRels (accStr br),
     "\nBoxes:", showIMap show "\n " (boxFwd br)] ++
   (if (language br == Intuitionistic)
    then
   [ "\nIntuit. Negations: ", showIMap show "\n " (intNeg br),
     "\nIntuit. Implications: ", showIMap show "\n " (intImp br),
     "\nIntuit. Neg rule chart: ", show (negRlCh br),
     "\nIntuit. Imp rule chart: ", show (impRlCh br),
     "\nIntuit. patterns: ", show (intPatterns br)
   ]
    else
   ["\nDia rule chart: ", show (diaRlCh br),
    "\nBlocked Dias: ", show (blockedDias br),
    "\nPatterns: ", show (patterns br)
   ])
   ++
   [ "\n", (if (language br == Intuitionistic) then showTodoInt else showTodo) (todoList br)]
   ++
   (if (language br == Classical)
    then
   [ "\n@ rule chart: ", show (list $ atRlCh br),
     "\nExist rule chart: ", show (list $ existRlCh br),
     "\nDown rule chart: ", show (downRlCh br),
     "\nUniv constraints: ", show (univCons br),
     "\nPrefix to dependency set:", showIMap  dsShow "\n " (prToDepSet br),
     "\nPrefix-Nominal classes: ", showMap ", " (nomPrefClasses br)]
    else [])
   ++
   [ "\nLast prefix: ", show (lastPref br),
     " nextnom : ", show (nextNom br),
     "\navailable defaults:", show (availableDefaults br),
     "\ngrounded defaults:", show (groundedDefaults br),
     "\ndetached defaults:", show (detachedDefaults br),
     "\n"
  ]
   where
    showIMap :: (a -> String) -> String -> IntMap a -> String
    showIMap vShow sep im
     = I.foldrWithKey (\k v -> (++ sep ++ show k ++ " -> " ++ vShow v )) "" im
    showMap sep = Map.foldrWithKey (\k v -> (++ sep ++ show k ++ " -> " ++ show v )) ""
    showMap_lits ml = Map.foldrWithKey (\l d -> (++ showl l ++ " " ++ dsShow d  ++ ", ")) "" ml
    showl (True,a)  = '+':show a
    showl (False,a) = '-':show a

data TodoList
 = TodoList{disjTodo :: Set PrFormula, -- classical OR + intuitionistic implication
            diaTodo :: Set PrFormula, -- clas. <> + int. neg implication + int. neg negation
            existTodo :: Set PrFormula,
            atTodo :: Set PrFormula,
            downTodo :: Set PrFormula,
            mergeTodo :: Set (DependencySet, Prefix, Nom)}

showTodoInt :: TodoList -> String
showTodoInt t = unlines
 [ "DisjTodo  " ++ show (disjTodo t)
 , "DiaTodo   " ++ show (diaTodo t)
 ]


showTodo :: TodoList -> String
showTodo t = unlines
 [ "DisjTodo  " ++ show (disjTodo t)
 , "DiaTodo   " ++ show (diaTodo t)
 , "ExistTodo " ++ show (existTodo t)
 , "AtTodo    " ++ show (atTodo t)
 , "DownTodo  " ++ show (downTodo t)
 , "MergeTodo " ++ show (mergeTodo t)
 ]

emptyTodoList :: TodoList
emptyTodoList =
      TodoList {  disjTodo = Set.empty
               ,   diaTodo = Set.empty
               , existTodo = Set.empty
               ,    atTodo = Set.empty
               ,  downTodo = Set.empty
               , mergeTodo = Set.empty
               }

addFormulas :: Params -> [PrFormula] -> Branch -> BranchInfo
addFormulas p fs br =
 foldr (\f bi ->
          case bi of
           BranchOK br2 -> addFormula p br2 f
           clash -> clash
       )
       (BranchOK br)
       fs

addFormula :: Params -> Branch -> PrFormula -> BranchInfo
addFormula p br pf@(PrFormula pr _ _ _)
 =   putAwayFormula  p pf
   $ rescheduleBlockedDias ur br -- TODO should only happen if adding Box, +Imp or +Neg
  where
   ur = getUrfather br (DS.Prefix pr)

putAwayFormula :: Params -> PrFormula -> Branch -> BranchInfo
putAwayFormula p pf@(PrFormula pr ds b f2) br =
 case (b,f2) of
   -- classical only cases
   (True,Box f)       -> addBoxConstraint      pr f ds p br
   (True,A f)         -> addUnivConstraint        f ds p br
   (True,Dia _)       -> BranchOK $ addToTodo pf br
   (True,E _)         -> BranchOK $ addToTodo pf br
   (True,At _ _)      -> BranchOK $ addToTodo pf br
   (True,Down _ _)    -> BranchOK $ addToTodo pf br
   -- classical or intuitionistic
   (True, Con fs)     -> addFormulas p (prefix pr ds True  fs) br
   (False,Dis fs)     -> addFormulas p (prefix pr ds False fs) br
   (True, Dis _)      -> BranchOK $ addToTodo pf br
   (False, Con _)     -> BranchOK $ addToTodo pf br
   -- intuitionistic cases
   (True,  Imp _ _)   -> addImpConstraint pr f2 ds p br -- similar to box: add to intImp constraints
   (False, Imp _ _)   -> BranchOK $ addToTodo pf br -- similar to diamond: add to diamond to-do
   (True,  Dimp _ _)  -> addImpConstraint pr f2 ds p br -- similar to box: add to intImp constraints
   (False, Dimp _ _)  -> BranchOK $ addToTodo pf br -- similar to diamond: add to diamond to-do
   -- intuitionistic negation: we need to check input language
   (False, Neg (Atm Taut))
                  | language br == Intuitionistic -> BranchOK br  -- trivially true, drop it
   (True, Neg f)  | language br == Intuitionistic
                      -> addNegConstraint pr f ds p br -- similar to box rule: add to true neg constraint
   (False, Neg _) | language br == Intuitionistic
                      -> BranchOK $ addToTodo pf br -- similar to diamond rule: add to diamond to-do
   -- atoms
   (True,Neg (Atm a)) | language br == Classical
                       -> addToLiterals pr ds (False,a) br
   (True, Atm a@(N _)) -> addToLiterals pr ds (True,a) $ addToTodo pf br
   (True, Atm Taut) -> BranchOK br
   (True, Atm a) | language br == Classical -> addToLiterals pr ds (True,a) br
   (True, Atm a) -> case addToLiterals pr ds (True,a) br of
                      c@(BranchClash _ _ _ _) -> c
                      BranchOK br2            -> addBoxConstraint pr (Atm a) ds p br2 -- heredity
   (False,Atm a)       -> addToLiterals pr ds (False,a) br
   -- error
   b_f         -> error $ "should not occur: " ++ show (b_f,language br)

{- todo list functions -}

addToTodo :: PrFormula -> Branch -> Branch
addToTodo pf@(PrFormula p ds b f2) br =
  if alreadyDone
   then br
   else brWithSaturation{todoList = newTodoList}
  where
   utodo = todoList br
   newTodoList =
       case (b,f2) of
         -- disjTodo: true disjunctions and false conjunctions, and true (intuit.) implication
         (True,Dis _)          -> utodo{ disjTodo = Set.insert pf ( disjTodo utodo)}
         (False,Con _)         -> utodo{ disjTodo = Set.insert pf ( disjTodo utodo)}
         (True,Imp _ _)        -> utodo{ disjTodo = Set.insert pf ( disjTodo utodo)}
         (True,Dimp _ _)       -> utodo{ disjTodo = Set.insert pf ( disjTodo utodo)}
             -- ^ invented formula p_i:Imp f1 f2  branches on prefix p_i
         -- diaTodo: (true) diamond, false (intuit.) negation, false (intuit.) implication
         (True, Dia _)         -> utodo{  diaTodo = Set.insert pf (  diaTodo utodo)}
         (False, Neg _)        -> utodo{  diaTodo = Set.insert pf (  diaTodo utodo)}
         (False, Imp _ _)      -> utodo{  diaTodo = Set.insert pf (  diaTodo utodo)}
         (False, Dimp _ _)     -> utodo{  diaTodo = Set.insert pf (  diaTodo utodo)}
         -- existTodo
         (True,E _)            -> utodo{existTodo = Set.insert pf (existTodo utodo)}
         -- atTodo
         (True,At _ _)         -> utodo{   atTodo = Set.insert pf (   atTodo utodo)}
         -- downTodo
         (True,Down _ _)       -> utodo{ downTodo = Set.insert pf ( downTodo utodo)}
         -- mergeTodo: positive nominal
         (True,Atm (N s))      -> utodo{mergeTodo = Set.insert (ds,p,s)
                                                          (mergeTodo utodo)}
         _                  -> error $ "addToTodo: " ++ show (b,f2)
   alreadyDone = case f2 of
     E  f3           -> Set.member f3 (existRlCh br)
     At n f3         -> Set.member (n,f3) (atRlCh br)
     Down _ _        -> downAlreadyDone br pf
     Atm (N s)       -> inSameClass br p s
     Dia _           -> False -- test happens when the todo list is processed
     Dis _           -> False -- no test 
     Con _           -> False
     Imp _ _         -> False
     Dimp _ _         -> False
     Neg _         -> False
     _               -> error $ "alreadyDone: " ++ show f2
   brWithSaturation = case f2 of
     E f3        -> br{existRlCh = Set.insert f3 (existRlCh br)}
     At n f3     -> br{atRlCh    = Set.insert (n,f3) (atRlCh br)}
     _           -> br

rescheduleBlockedDias :: Prefix -> Branch -> Branch
rescheduleBlockedDias pr br
 = foldr addToTodo br2 toAdd
  where toAdd = iget [] pr (blockedDias br)
        br2 = br{blockedDias = I.delete pr $ blockedDias br}

addToBlockedDias :: PrFormula -> Branch -> BranchInfo
addToBlockedDias f@(PrFormula pr _ _ _) br
 = BranchOK br{blockedDias = I.insertWith (++) ur [f] (blockedDias br)}
   where ur = getUrfather br (DS.Prefix pr)

{-    helper functions for equivalence class merge     -}

merge :: Params -> Prefix -> DependencySet -> Nom -> Branch -> BranchInfo
merge p pr fDs n br
 = let
       (DS.Prefix ur1,classes1) = DS.find  (DS.Prefix pr) (nomPrefClasses br)
       (poAncestor   ,classes2) = DS.find  (DS.Nominal n) classes1
       classes3                 = DS.union (DS.Prefix pr) (DS.Nominal n) classes2
   in
    case poAncestor of
     DS.Nominal _   -> BranchOK $ addClassDeps ur1 fDs $ br { nomPrefClasses = classes3 }
                         -- new nominal from down-arrow rule
     DS.Prefix ur2
      | ur1 == ur2 -> BranchOK $ addClassDeps ur1 fDs br
      | otherwise
         ->
          let
           oldUr           = max ur1 ur2
           newUr           = min ur1 ur2
           literalSlots    = mapMaybe (\ur -> I.lookup ur (literals br)) [ur1,ur2]
           currentDeps     = dsUnions $ fDs:(map (findDeps br) [ur1,ur2])
           newPrToDepSet   = I.insert newUr currentDeps (prToDepSet br)
           newUrfatherSlot = lsAddDeps currentDeps $ lsUnions literalSlots
          in
           case newUrfatherSlot of
            SlotUpdateFailure clashingDeps ->
             let newBr = br{nomPrefClasses = classes3} in
             BranchClash newBr pr (dsUnion clashingDeps currentDeps) (neg taut)

            SlotUpdateSuccess slot ->
             let
              newLiterals = I.delete oldUr $ I.insert newUr slot $ literals br

              -- structures that merge
              newBoxFwd       = D.moveInnerPlusDeps fDs (boxFwd br) oldUr newUr
              newAccStr       = mergePrefixes (accStr br) oldUr newUr fDs
              newDiaRlCh      = moveInMap (diaRlCh br)  oldUr newUr Set.union
              newDownRlCh     = moveInMap (downRlCh br) oldUr newUr Set.union
              newNegRlCh      = moveInMap (negRlCh br)  oldUr newUr Set.union
              newImpRlCh      = moveInMap (impRlCh br)  oldUr newUr Set.union
              newBlockedDias  = moveInMap (blockedDias br) oldUr newUr (++)

              -- structures that combine
              mapBoxFwd = map (\idx -> iget Map.empty idx (boxFwd br) ) [ur1,ur2]
              mapAccFwd = map (successors (accStr br)) [ur1,ur2]
              forms1    = concatMap (boxRule currentDeps) $ combine mapBoxFwd mapAccFwd

              formulasToAdd = nubAndMergeDeps forms1

              newBr           = br{nomPrefClasses = classes3,
                                   boxFwd         = newBoxFwd,
                                   accStr         = newAccStr,
                                   prToDepSet     = newPrToDepSet,
                                   diaRlCh        = newDiaRlCh,
                                   downRlCh       = newDownRlCh,
                                   negRlCh        = newNegRlCh,
                                   impRlCh        = newImpRlCh,
                                   blockedDias    = newBlockedDias,
                                   literals       = newLiterals}
             in
              addFormulas p formulasToAdd $ rescheduleBlockedDias newUr newBr

nubAndMergeDeps :: [PrFormula] -> [PrFormula]
-- Because of the equivalence classes, a same formula can be added to a branch
-- several times with different branching dependencies.
-- This function takes a list of prefixed formulas, looks which inner formulas
-- are the same and merges their branching dependencies.
nubAndMergeDeps prfs =  namd prfs (Map.empty::Map (Prefix,Bool,Formula) DependencySet)

namd :: [PrFormula] -> Map (Prefix,Bool,Formula) DependencySet -> [PrFormula]
namd ((PrFormula p ds b f):prfs) theMap =
  namd prfs (Map.insertWith dsUnion (p,b,f) ds theMap)

namd [] theMap = map (\((p,b,f),ds) -> PrFormula p ds b f) (Map.assocs theMap)

{-     handling nominal urfathers, equivalence classes and dependencies     -}

isNominalUrfather :: Branch -> Prefix -> Bool
isNominalUrfather br p = DS.isRoot (DS.Prefix p) classes
                         where classes = nomPrefClasses br

-- May look redundant with getUrfatherAndDeps, but it is important not
-- to make the test isRoot for performance
getUrfather :: Branch -> DS.Pointer -> Prefix
getUrfather br p =
    ur
  where
        (DS.Prefix ur) = DS.onlyFind p (nomPrefClasses br)

getUrfatherAndDeps :: Branch -> DS.Pointer -> (Prefix,DependencySet,EquivClasses)
getUrfatherAndDeps br p =
   if DS.isRoot p classes then defaultAnswer
                          else (ur,deps,newClasses)
  where classes = nomPrefClasses br
        (urfather, newClasses) = DS.find p classes
        (DS.Prefix ur) = urfather
        DS.Prefix unboxedP = p
        defaultAnswer = (unboxedP,dsEmpty,classes)
        deps = findDeps br ur

findDeps :: Branch -> Prefix -> DependencySet
findDeps br pr = iget dsEmpty pr (prToDepSet br)

addClassDeps :: Prefix -> DependencySet -> Branch -> Branch
addClassDeps pr ds br = br { prToDepSet = I.insertWith dsUnion pr ds (prToDepSet br) }

inSameClass :: Branch -> Prefix -> C.ByteString -> Bool
inSameClass br p n
 = case fst $ DS.find (DS.Nominal n) (nomPrefClasses br) of
    DS.Nominal _ -> False
    DS.Prefix p2 -> getUrfather br (DS.Prefix p) == p2

{-     box-related constraints     -}

boxRule :: DependencySet
            -> (Map Formula DependencySet, [(Prefix,DependencySet)])
            -> [PrFormula]
boxRule deps (mapBox, listAcc)
 = [PrFormula p (dsUnions [deps,ds1,ds2]) True f | -- True since rule [] is classical
                      (f,ds1) <- Map.toList mapBox,
                      (p,ds2) <- listAcc     ]

addBoxConstraint :: Prefix -> Formula -> DependencySet -> Params -> Branch
                 -> BranchInfo
addBoxConstraint pr_ f ds p br
 | boxAlreadyDone br pr f = BranchOK br
 | otherwise
    = let newBr = br{boxFwd =  I.insertWith Map.union pr (Map.singleton f ds) (boxFwd br)}
          succs  = successors (accStr br) pr
          toAdd = fromTrans ++ fromBox
          fromBox = map (\(pr2,ds2) -> PrFormula pr2 (dsUnion ds ds2) True f) succs
          fromTrans
           = if transitive p
              then map (\(pr2,ds2) -> PrFormula pr2 (dsUnion ds ds2) True (Box f)) succs
              else []
    -- todo check again with new pattern, create successor if new pattern not realized
      in
         addFormulas p toAdd newBr
 where pr = getUrfather br (DS.Prefix pr_)

boxAlreadyDone :: Branch -> Prefix -> Formula -> Bool
boxAlreadyDone br ur f = isJust (I.lookup ur (boxFwd br) >>= Map.lookup f)

negAlreadyDone :: Branch -> Prefix -> Formula -> Bool
negAlreadyDone br ur f = isJust (I.lookup ur (intNeg br) >>= Map.lookup f)

-- called with f such that we added True (Neg f) to the branch
addNegConstraint :: Prefix -> Formula -> DependencySet -> Params -> Branch
                 -> BranchInfo
addNegConstraint pr_ f ds p br
 | negAlreadyDone br pr f = BranchOK br
 | otherwise
    = let newBr = br{intNeg =  I.insertWith Map.union pr (Map.singleton f ds) (intNeg br)}
          succs  = successors (accStr br) pr
          negRule = map (\(pr2,ds2) -> PrFormula pr2 (dsUnion ds ds2) False f) succs
                                     -- False as tableau rule indicates ^
          negPropagation
           = map (\(pr2,ds2) -> PrFormula pr2 (dsUnion ds ds2) True (Neg f)) succs -- emulate transitivity
    -- todo check again with new pattern, create successor if new pattern not realized
          toAdd = negRule ++ negPropagation
      in
         addFormulas p toAdd newBr
 where pr = getUrfather br (DS.Prefix pr_)

addImpConstraint :: Prefix -> Formula -> DependencySet -> Params -> Branch
                 -> BranchInfo
addImpConstraint pr_ f ds p br
 | isImp f && impAlreadyDone br pr f = BranchOK br
 | isImp f
    = let newBr = br{intImp =  I.insertWith Map.union pr (Map.singleton f ds) (intImp br)}
          succs  = successors (accStr br) pr
          impRuleSchedule
           =
            map (\(pr2,ds2) -> PrFormula pr2 (dsUnion ds ds2) True f) succs -- add to to-do for branching
          newBr2 = foldr addToTodo newBr impRuleSchedule
          impPropagation = map (\(pr2,ds2) -> PrFormula pr2 (dsUnion ds ds2) True f) succs
    -- todo check again with new pattern, create successor if new pattern not realized
      in
         addFormulas p impPropagation newBr2
 | otherwise = error "should be called with Imp or Dimp formula"
    where pr = getUrfather br (DS.Prefix pr_)
          isImp (Imp _ _)  = True
          isImp (Dimp _ _) = True
          isImp _          = False

impAlreadyDone :: Branch -> Prefix -> Formula -> Bool
impAlreadyDone br ur f = isJust (I.lookup ur (intImp br) >>= Map.lookup f)

-- accessibility Formulas

addAccFormula :: Params -> (DependencySet,Prefix,Prefix) -> Branch -> BranchInfo
addAccFormula p (ds, p1_, p2_) br
   = addFormulas p toAdd newBr
     where
      p1 = getUrfather br (DS.Prefix p1_)
      p2 = getUrfather br (DS.Prefix p2_)
      toSendBox = Map.toList $ iget Map.empty p1 (boxFwd br)
      boxApplications = map (\(f,ds2) -> PrFormula p2 (dsUnion ds ds2) True  f) toSendBox
      toAdd = boxApplications ++ propagations ++ heredity
      -- Neg and Imp applications to new prefix are done after we propagate formulas to new prefix
      -- since it is reflexive

      toSendNeg = Map.toList $ iget Map.empty p1 (intNeg br)
      toSendImp = Map.toList $ iget Map.empty p1 (intImp br)

      propagations =
       if transitive p
        then    map (\(f,ds2) -> PrFormula p2 (dsUnion ds ds2) True (Box f)) toSendBox
             ++ map (\(f,ds2) -> PrFormula p2 (dsUnion ds ds2) True (Neg f)) toSendNeg
             ++ map (\(f,ds2) -> PrFormula p2 (dsUnion ds ds2) True f)       toSendImp
        else []

      positiveAtoms = map (\((_,a),d) -> (a,d)) $ filter (fst . fst) $ Map.assocs $ iget Map.empty p1 (literals br)
      heredity
        | language br == Intuitionistic = map (\(a,ds2) -> PrFormula p2 (dsUnion ds ds2) True (Atm a)) positiveAtoms
        | otherwise = []

      newBr = insertRelationBranch br p1 p2 ds

insertRelationBranch :: Branch -> Prefix -> Prefix -> DependencySet -> Branch
insertRelationBranch br p1 p2 ds
 = br{accStr = insertRelation (accStr br) p1 p2 ds}

-- <>f is pattern blocked if its pattern is a subset
-- of one pattern of the branch's pattern store
patternBlocked :: Params -> Branch -> PrFormula -> Bool
patternBlocked p br f = not $ I.null $ I.filter lookForSuperset (patterns br)
 where lookForSuperset = Set.isSubsetOf (patternOf p br f)

-- !f is pattern blocked if its pattern is a subset
-- of one pattern of the branch's pattern store
intPatternBlocked :: Branch -> PrFormula -> Bool
intPatternBlocked br f = not $ I.null $ I.filter lookForSuperset (intPatterns br)
 where lookForSuperset = Set.isSubsetOf (intPatternOf br f)

-- given a p:-(f->g) formula, return the pattern:
-- { +f,-g } U {-f',+!f'|p:+!f' in branch } U { +(f'-> g') | p:+(f'->g') in branch }
--           U {+p | p:+p in branch  }
intPatternOf :: Branch -> PrFormula -> Set (Bool,Formula)
intPatternOf br (PrFormula pr _ _ (Imp f g))
 = foldr Set.insert otherFs [(True,f),(False,g)]
   where
    ur = getUrfather br (DS.Prefix pr)
    otherFs = fsOf ur
    fsOf p = negFsOf p `Set.union` impFsOf p `Set.union` heredity p
    negFsOf p = set $ concatMap (\e -> [(True,Neg e),(False,e)]) $ Map.keys $ iget Map.empty p (intNeg br)
    impFsOf p = Set.map (\e -> (True,e)) $ Map.keysSet $ iget Map.empty p (intImp br)
    heredity p = set $ concatMap posProp $ Map.keys $ iget Map.empty p (literals br)
    posProp (True, p'@(P _)) = [(True,Atm p')] ; posProp _ = []

-- given a p:-!f formula, return the pattern:
-- { +f } U {-f',+!f'|p:+!f' in branch } U { +(f'-> g') | p:+(f'->g') in branch }
--        U {+p | p:+p in branch  }
intPatternOf br (PrFormula pr _ _ (Neg f))
 = Set.insert (True,f) otherFs
   where
    ur = getUrfather br (DS.Prefix pr)
    otherFs = fsOf ur
    fsOf p = negFsOf p `Set.union` impFsOf p `Set.union` heredity p
    negFsOf p = set $ concatMap (\e -> [(True,Neg e),(False,e)]) $ Map.keys $ iget Map.empty p (intNeg br)
    impFsOf p = Set.map (\e -> (True,e)) $ Map.keysSet $ iget Map.empty p (intImp br)
    heredity p = set $ concatMap posProp $ Map.keys $ iget Map.empty p (literals br)
    posProp (True, p'@(P _)) = [(True,Atm p')] ; posProp _ = []

intPatternOf _ _ = error "intPatternOf called with a non-impl. nor neg. formula"


-- given a p:<>f formula, return the pattern:
-- { f } U { f' | p:[]f' in branch }
patternOf :: Params -> Branch -> PrFormula -> Set Formula
patternOf params br (PrFormula pr _ _ (Dia f))
 = Set.insert f boxes
    where ur = getUrfather br (DS.Prefix pr)
          boxes = if transitive params
                   then boxesOf ur
                          `Set.union` (Set.map Box $ boxesOf ur)
                   else boxesOf ur
          boxesOf p = Map.keysSet $ iget Map.empty p (boxFwd br)
patternOf _ _ _ = error "patternOf called with a non diamond formula"

findByPattern :: Branch -> Set Formula -> Prefix
findByPattern br pattern =
       head $ [ pr | (pr,pat2)  <- I.toList $ patterns br,
                     pattern `Set.isSubsetOf` pat2 ]

{-     modifications done by rule application     -}

-- add checks for
--  1. pattern blocking
--  2. prefix-level rule saturation
addDiaRuleCheck :: Params -> Prefix -> Formula -> Prefix -> Branch -> BranchInfo
addDiaRuleCheck p pr f newPr br =
  BranchOK br2
   where pattern = patternOf p br (PrFormula ur dsEmpty True (Dia f))
         br1 = br{patterns = I.insert newPr pattern (patterns br)}
         br2 = br1{diaRlCh=I.insertWith Set.union ur (Set.singleton f) (diaRlCh br1)}
         ur = getUrfather br (DS.Prefix pr)

-- add checks for
--  1. pattern blocking
--  2. prefix-level rule saturation
addImpRuleCheck :: Prefix -> (Formula,Formula) -> Prefix -> Branch -> BranchInfo
addImpRuleCheck pr (f,g) newPr br =
  BranchOK br2
   where pattern = intPatternOf br (PrFormula ur dsEmpty True (Imp f g))
         br1 = br{intPatterns = I.insert newPr pattern (intPatterns br)}
         br2 = br1{impRlCh=I.insertWith Set.union ur (Set.singleton (f,g)) (impRlCh br1)}
         ur = getUrfather br (DS.Prefix pr)

addNegRuleCheck :: Prefix -> Formula -> Prefix -> Branch -> BranchInfo
addNegRuleCheck pr f newPr br =
  BranchOK br2
   where pattern = intPatternOf br (PrFormula ur dsEmpty True (Neg f))
         br1 = br{intPatterns = I.insert newPr pattern (intPatterns br)}
         br2 = br1{negRlCh=I.insertWith Set.union ur (Set.singleton f) (negRlCh br1)}
         ur = getUrfather br (DS.Prefix pr)

diaAlreadyDone :: Branch -> PrFormula -> Bool
diaAlreadyDone b (PrFormula p _ _ f) =  -- f can be (Dia _), (Neg _) or (Imp _ _)
    case I.lookup ur (diaRlCh b) of
      Nothing  -> False
      Just fset -> Set.member f fset
 where ur = getUrfather b (DS.Prefix p)


addDownRuleCheck :: Prefix -> Formula -> Branch -> BranchInfo
addDownRuleCheck pr f br =
  BranchOK br{downRlCh=I.insertWith Set.union ur (Set.singleton f) (downRlCh br)}
   where ur = getUrfather br (DS.Prefix pr)

downAlreadyDone :: Branch -> PrFormula -> Bool
downAlreadyDone b (PrFormula p _ _ f@(Down _ _)) =
  case I.lookup ur (downRlCh b) of
     Nothing  -> False
     Just fset -> Set.member f fset
 where ur = getUrfather b (DS.Prefix p)

downAlreadyDone _ _ = error "down already done : wrong formula kind"

-- | return some nominal that holds at a given prefix
positiveNomOf :: Branch -> Prefix -> Maybe C.ByteString
positiveNomOf b p = positiveNom (literals b) ur
 where ur = getUrfather b (DS.Prefix p)

-- TODO negAlreadyDone
-- TODO impAlreadyDone

addUnivConstraint :: Formula -> DependencySet -> Params -> Branch -> BranchInfo
addUnivConstraint f ds p br
 = addFormulas p [PrFormula pr ds True f -- True since this is classical rule
                    | pr <- urfathers] newBr
   where newBr = br{univCons = (ds,f):(univCons br)}
         prefs = [0..(lastPref br)]
         urfathers = filter (isNominalUrfather br) prefs

createNewNode :: Params -> Branch -> BranchInfo
createNewNode p br
 = addFormulas p
               ( map (\(ds,f) -> PrFormula newPr ds True f) univConstraints ) -- see above
               newBrWithRefl
   where newPr = lastPref br + 1
         newBr = br{lastPref = newPr}
         univConstraints = univCons br
         newBrWithRefl | reflexive p = addReflexiveLink newPr newBr
                       | otherwise   = newBr

createNewNom :: Branch -> BranchInfo
createNewNom br
 = BranchOK br{nextNom = nextNom br + 1}


addReflexiveLink :: Prefix -> Branch -> Branch
addReflexiveLink pr br 
 = insertRelationBranch br pr pr dsEmpty

-- preparation of the branch at the beginning of the calculus:
--  - add the input formula at prefix 0
--  - add a nominal formula at a fresh prefix for each nominal of the input language
--    (even if the nominal was filtered out during lexical normalisation)
--  - add reflexive links for prefixes 0 and nominal witnesses
initialBranch :: Params -> Language -> LanguageInfo -> Formula -> [Rule] -> Maybe Formula
                  -> BranchInfo
initialBranch p lang fLang f defaults_ conseq_
 = addFormulas p pfs br
    where
          pfs = case conseq_ of
                 Nothing -> [first True f]
                 Just c  -> case lang of
                              Classical      -> [ first True  f
                                                , first True $ neg c]
                              Intuitionistic -> [ first True f
                                                , first False c]
          first = PrFormula 0 dsEmpty
          ns = languageNoms fLang
          nbNs = length ns
          initPrefixes = 0:[1..nbNs]
          br | reflexive p = foldr addReflexiveLink emptyBr initPrefixes
             | otherwise   = emptyBr
          initClasses = foldr (\(pr,n) -> DS.union (DS.Prefix pr) (DS.Nominal n))
                              DS.mkDSet
                              (zip [1..] ns)
          initLiterals = foldr (\(pr,n) -> D.insert pr (True, (N n)) dsEmpty)
                               I.empty
                               (zip [1..] ns)
          emptyBr =
           Branch{ language          = lang,
                   literals          = initLiterals,
                   accStr            = emptyRels,
                   todoList          = emptyTodoList,
                   boxFwd            = I.empty,
                   intNeg            = I.empty,
                   intImp            = I.empty,
                   diaRlCh           = I.empty,
                   downRlCh          = I.empty,
                   negRlCh           = I.empty,
                   impRlCh           = I.empty,
                   atRlCh            = Set.empty,
                   existRlCh         = Set.empty,
                   patterns          = I.empty,
                   intPatterns       = I.empty,
                   univCons          = [],
                   lastPref          = nbNs,
                   nextNom           = 0,
                   prToDepSet        = I.empty,
                   nomPrefClasses    = initClasses,
                   inputLanguage     = fLang,
                   blockedDias       = I.empty,
                   facts             = f,
                   availableDefaults = defaults_,
                   groundedDefaults  = [],
                   detachedDefaults  = [],
                   subTabCache       = emptyCache
                 }

emptyCache :: SubTabCache
emptyCache = Map.empty

insertCache :: SubTabCache -> BranchInfo -> BranchInfo
insertCache stc (BranchOK br)          = BranchOK    br{subTabCache = stc}
insertCache stc (BranchClash br p d c) = BranchClash br{subTabCache = stc} p d c

addToLiterals :: Prefix -> DependencySet -> (Bool,Atom) -> Branch -> BranchInfo
addToLiterals pr_ ds1 l br
  = case updateMap (literals br) pr ds l of
     UpdateSuccess ls  -> BranchOK br{literals = ls}
     UpdateFailure dsf -> BranchClash br pr dsf (lit l)
   where (pr,ds2,_) = getUrfatherAndDeps br (DS.Prefix pr_)
         ds = ds1 `dsUnion` ds2
         lit (True,a) = Atm a
         lit (_,a) = Neg $ Atm a


{-     other functions     -}
prefixes :: Branch -> [Prefix]
prefixes br = [0..(lastPref br)]

combine :: [a] -> [b] -> [(a,b)]
combine [] _  = error "combine: first list empty"
combine _  [] = error "combine: second list empty"
combine as bs = [(a,b) | (idxA,a) <- zip [(0::Int)..] as,
                         (idxB,b) <- zip [(0::Int)..] bs,
                         idxA /= idxB]

moveInMap :: IntMap b -> Int -> Int -> (b -> b -> b) -> IntMap b
moveInMap m origKey destKey mergeF
 = case I.lookup origKey m of
    Nothing -> m
    Just origValue
     -> I.insertWith mergeF destKey origValue $ I.delete origKey m

list :: Ord a => Set.Set a -> [a]
list = Set.toList

set :: Ord a => [a] -> Set.Set a
set = Set.fromList

iget :: a -> Int -> IntMap a -> a
iget = I.findWithDefault


-- defaults rules management
enableDefault :: Params -> Dependency -> (Rule,DependencySet) -> Branch -> BranchInfo
enableDefault p d rds@((Rule _ conseq'),ds) br
 = let br2 = br{ detachedDefaults = rds:(detachedDefaults br)
               , groundedDefaults = delete rds (groundedDefaults br)}
   in addFormula p br2 (PrFormula 0 (IntSet.insert d ds) True conseq') -- add consequent with dependencies

-- returns list of grounded rules and new list of
-- available (non-grounded) rules
checkGrounded :: Params -> Branch -> Applicable r -> ApplyRule r
              -> ([(Rule,DependencySet)], [Rule], SubTabCache)
checkGrounded p br applicableRules applyRule
 = let (l, newCache) = go (availableDefaults br) (subTabCache br)
       (left, right) = partitionEithers l
   in  (left, right, newCache)

 where go (r:rs) stc_ = let (l,stc') = go rs stc_ in
                        case checkGrounded1 p br r applicableRules applyRule stc' of
                           (Just ds, stc'') -> ((Left (r,ds)):l, stc'')
                           (Nothing, stc'') -> ((Right r):l,     stc'')
       go [] stc_     = ([] , stc_ )

checkGrounded1 :: Params -> Branch -> Rule -> Applicable r -> ApplyRule r -> SubTabCache
               -> (Maybe DependencySet, SubTabCache)
checkGrounded1 p br r ab ar stc =
 case runTableau (language br) p f mc ab ar stc of
   (True , stc') -> ( Nothing                                                                , stc')
   (False, stc') -> ( Just $ foldr IntSet.union IntSet.empty $ map snd (detachedDefaults br) , stc')
  where conseqs = map (conseq . fst) (detachedDefaults br)
        (f,mc)  = case language br of
                   Classical      -> (foldr conj (facts br `conj` (neg $ prereq r)) conseqs, Nothing)
                   Intuitionistic -> (foldr conj (facts br) conseqs, Just (prereq r)) -- so (prereq r) is added as False
-- ^ if tableau closed (return False), prerequisite is consequence of facts & enabled consequents

-- for all grounded default, check if blocked by facts and consequents of enabled rules.
-- if blocked, delete from grounded list returns that new list
checkBlocked :: Params -> Branch -> Applicable r -> ApplyRule r
             -> ([(Rule,DependencySet)], SubTabCache)
checkBlocked p br ab ar
 = go (groundedDefaults br, subTabCache br)
  where go (def@(r,_):ds, stc_ ) = let (l,stc') = go (ds,stc_) in
                                   case checkBlocked1 p br r ab ar stc' of
                                    (True,stc'') -> (l,stc'')
                                    (False,stc'')-> (def:l,stc'')
        go ([], stc_) = ([], stc_)

checkBlocked1 :: Params -> Branch -> Rule -> Applicable r -> ApplyRule r -> SubTabCache
              -> (Bool, SubTabCache)
checkBlocked1 p br r ab ar stc =
 case runTableau (language br) p f Nothing ab ar stc of
   (True, stc')  -> (False, stc')
   (False, stc') -> (True,  stc')
 where conseqs = map (conseq . fst) (detachedDefaults br)
       f       = foldr conj (conseq r `conj` facts br) conseqs

-- simplified tableau calculus to check default rules conditions
type Depth = Int
data OpenFlag = OPEN | CLOSED DependencySet

runTableau :: Language -> Params -> Formula -> Maybe Formula -> Applicable r -> ApplyRule r -> SubTabCache
           -> (Bool, SubTabCache)
runTableau l p f mc ab ar stc
 | cache p =
      case Map.lookup (f,mc) stc of
        Nothing -> case tableauDown p 0 (initialBranch p l (langInfo [f]) f [] mc) ab ar of
                     OPEN     -> (True,  Map.insert (f,mc) True stc)
                     CLOSED _ -> (False, Map.insert (f,mc) False stc)
        Just b  -> (b, stc)
 | otherwise =
    case tableauDown p 0 (initialBranch p l (langInfo [f]) f [] mc) ab ar of
       OPEN -> (True, stc)
       CLOSED _ -> (False, stc)


tableauDown :: Params -> Depth -> BranchInfo -> Applicable r -> ApplyRule r -> OpenFlag
tableauDown p depth branchInfo applicableRules applyRule =
      case branchInfo of
         BranchClash _ _ bprs _ -> CLOSED bprs
         BranchOK br ->
            case applicableRules br p (depth + 1) of
               Nothing  -> OPEN         -- saturated open branch
               Just (rule,newBranch)  ->
                  case applyRule p rule newBranch of
                     [newBi] -> tableauDown  p (depth + 1) newBi applicableRules applyRule
                     bis     -> tableauRight p (depth + 1) bis   dsEmpty applicableRules applyRule

tableauRight :: Params -> Depth -> [BranchInfo] -> DependencySet -> Applicable r -> ApplyRule r -> OpenFlag
tableauRight _ _ []  depset _ _ = CLOSED depset
tableauRight p depth (hd:tl) currentDepset ab ar =
 case tableauDown p depth hd ab ar of
   OPEN -> OPEN
   CLOSED depset -> if backjumping p && not (dsMember depth depset)
                     then CLOSED depset
                     else tableauRight p depth tl (dsUnion depset currentDepset) ab ar

type Applicable r = Branch -> Params -> Dependency -> Maybe (r,Branch)
type ApplyRule r = Params -> r -> Branch -> [BranchInfo]
