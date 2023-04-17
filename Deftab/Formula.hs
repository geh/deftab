module Deftab.Formula

(Nom, Prop, Prefix, Formula(..), Atom(..),
Rule(..),
DependencySet, Dependency,
dsUnion, dsUnions, dsInsert, dsMember,
dsEmpty, dsMin, dsShow, addDeps,
PrFormula(..),showLess,
neg, prefix, firstPrefixedFormula,
replaceVar,
conj, disj, taut,
parseInput, 
list, imp,
convert , LanguageInfo(..), langInfo, Language(..)
)

 where

import qualified Data.Set as Set
import Data.Set ( Set )
import qualified Data.IntSet as IntSet
import Data.List ( nub, intercalate )
import qualified Data.ByteString.Char8 as C

import Deftab.ParserTypes (Form(..), Atom(..), Nom, Prop
                     , Rule'(..), Language(..))
import qualified Deftab.Lexer  as L
import qualified Deftab.Parser as P

data Formula
     = Con   (Set Formula)
     | Dis   (Set Formula)
     | At     Nom Formula
     | Down   Nom Formula
     | Box    Formula
     | Dia    Formula
     | A      Formula
     | E      Formula
     | Atm    Atom
     -- Intuitionistic operators:
     | Imp    Formula Formula
     | Dimp    Formula Formula
     | Neg    Formula
  deriving (Eq, Ord)

instance Show Formula where
 show (Con fs)   = "^" ++ show (list fs)
 show (Dis fs)   = "v" ++ show (list fs)
 show (At n f)   = "@N" ++ C.unpack n ++ " " ++ show f
 show (Box f)    = "[]" ++ show f
 show (Dia f)    = "<>" ++ show f
 show (A f)      = "A " ++ show f
 show (E f)      = "E " ++ show f
 show (Down n f) = "down N" ++ C.unpack n ++ ". (" ++ show f ++ ")"
 show (Atm a)    = show a
 show (Imp f1 f2) = showPar f1 ++ " -> " ++ showPar f2
 show (Dimp f1 f2) = showPar f1 ++ " <-> " ++ showPar f2
 show (Neg f)    = '!':show f

showPar :: Formula -> String
showPar f@(Atm _) = show f
showPar f@(Neg (Atm _)) = show f
showPar f = "( " ++ show f ++ " )"

data Rule = Rule { prereq :: Formula
                 , justif :: Formula
                 , conseq :: Formula }   deriving (Eq,Ord,Show)


type InputFile = (Language,[Form],[Rule'],[Form])

parse :: String -> InputFile
parse = P.parse . L.alexScanTokens

parseInput :: String ->  (Language,LanguageInfo,[Formula],[Rule],[Formula])
parseInput s = (lang,fLang, fs, ds, csq)
    where (lang,fs',ds',csq') = parse s
          (fs,ds,csq)
            = case lang of
               Classical      -> (map convert fs', map convertRule ds', map convert csq')
               Intuitionistic -> (map convint fs', map convintRule ds', map convint csq')
          ruleFs         = concatMap (\(Rule f1 f2 f3) -> [f1,f2,f3]) ds
          fLang          = langInfo $ fs++ruleFs++csq

convertRule :: Rule' -> Rule
convertRule (Rule' f1 f2 f3) = Rule (convert f1) (convert f2) (convert f3)

convintRule :: Rule' -> Rule
convintRule (Rule' f1 f2 f3) = Rule (convint f1) (convint f2) (convint f3)

-- convert parsed formula to formula usable by tableau calculus
-- classical logic
-- push negations inside, flatten and make non-redundant conjuncts and disjuncts
convert :: Form -> Formula
convert (At' n f) = At n (convert f)
convert (Down' n f) = Down n (convert f)
convert (Box' f) = Box (convert f)
convert (Dia' f) = Dia (convert f)
convert (A' f) = A (convert f)
convert (E' f) = E (convert f)
convert (Con' f1 f2) = convert f1 `conj` convert f2
convert (Dis' f1 f2) = convert f1 `disj` convert f2
convert (Imp' f1 f2)  = (neg $ convert f1) `disj` convert f2
convert (Dimp' f1 f2) = convert f1 `dimp` convert f2
convert (Lit' a) = Atm a
convert (Neg' f) = neg (convert f)

-- convert parsed formula to formula usable by tableau calculus
-- intuitionistic logic
-- push negations inside, flatten and make non-redundant conjuncts and disjuncts
convint :: Form -> Formula
convint (At' _ _)    = error "Parsing as intuitionistic formula, Satisfaction operator not supported."
convint (Down' _ _)  = error "Parsing as intuitionistic formula, Down-arrow operator not supported."
convint (Box' _)     = error "Parsing as intuitionistic formula, Box operator not supported."
convint (Dia' _)     = error "Parsing as intuitionistic formula, Diamond operator not supported."
convint (A' _)       = error "Parsing as intuitionistic formula, Universal operator not supported."
convint (E' _)       = error "Parsing as intuitionistic formula, Existential operator not supported."
convint (Con' f1 f2) = convint f1 `conj` convint f2
convint (Dis' f1 f2) = convint f1 `disj` convint f2
convint (Imp' f (Neg' (Lit' Taut))) = Neg (convint f)
convint (Imp' f1 f2) = Imp (convint f1) (convint f2)
convint (Dimp' f1 f2) = Dimp (convint f1) (convint f2)
convint (Lit' (N _)) = error "Parsing as intuitionistic formula, Nominals not supported."
convint (Lit' a) = Atm a
convint (Neg' f) = Neg (convint f) -- do not push negs inside

-- CONSTRUCTORS

taut :: Formula
taut = Atm Taut

{- Conjunction and disjunction -}

conj, disj :: Formula -> Formula -> Formula

conj    (Con xs) (Con ys) = Con (Set.union xs ys)
conj     f     c@(Con  _) = conj c f
conj c@(Con xs)   f
    | isTrue f            = c
    | isFalse f           = neg taut
    | otherwise           = Con (Set.insert f xs)
conj     f        f'
    | isTrue f            = f'
    | isFalse f           = neg taut
    | isTrue f'           = f
    | isFalse f'          = neg taut
    | otherwise           = skipSingleton Con (set [f,f'])

disj   (Dis xs)   (Dis ys) = Dis (Set.union xs ys)
disj    f       c@(Dis  _) = disj c f
disj c@(Dis xs)    f
    | isTrue f             = taut
    | isFalse f            = c
    | otherwise            = Dis (Set.insert f xs)
disj    f          f'
    | isTrue f             = taut
    | isFalse f            = f'
    | isTrue f'            = taut
    | isFalse f'           = f
    | otherwise            = skipSingleton Dis (set [f,f'])

dimp :: Formula -> Formula -> Formula
dimp f1 f2 = (neg f1 `disj` f2) `conj` (f1 `disj` neg f2)
-- this form is preferred so as to enhance lazy branching

-- TODO
-- when there is no literal in the double implication,
-- use the following form:
--dimp f1 f2 = (f1 `conj` f2) `disj` (neg f1 `conj` neg f2)

imp :: Formula -> Formula -> Formula
imp f1 f2 = neg f1 `disj` f2

skipSingleton :: (Set Formula -> Formula) -> Set Formula -> Formula
skipSingleton c xs
 | Set.size xs == 1 = Set.findMin xs
 | otherwise       = c xs

isTrue, isFalse :: Formula -> Bool
isTrue  (Atm Taut) = True
isTrue   _         = False
isFalse (Neg (Atm Taut)) = True
isFalse  _         = False

-- invariant: neg is only applied before atoms during
-- the execution of the tableau algorithm
neg :: Formula -> Formula
neg (Con l)          = Dis (Set.map neg l)
neg (Dis l)          = Con (Set.map neg l)
neg (At n f)         = At   n (neg f)
neg (Down n f)       = Down n (neg f)
neg (Box f)          = Dia  (neg f)
neg (Dia f)          = Box  (neg f)
neg (A f)            = E (neg f)
neg (E f)            = A (neg f)
neg (Atm a)          = Neg (Atm a)
neg (Neg a)          = a
neg (Imp _ _)        = error "should not happen" -- only called on classical formulas
neg (Dimp _ _)       = error "should not happen" --

-- prefixed formula

type Prefix  = Int

data PrFormula =
  PrFormula Prefix        -- ^ World where formula holds
            DependencySet -- ^ Backjumping data
            Bool          -- ^ Always true for classical, may be false for intuitionistic
            Formula       -- ^ The formula itself
 deriving Eq

instance Show PrFormula where
 show (PrFormula pr ds b f)
   = show pr ++ ":" ++ dsShow ds ++ ":" ++ (if b then '+' else '-'):show f

showLess :: PrFormula -> String
showLess (PrFormula pr _ _ f) = show pr ++ ":" ++ show f

prefix :: Prefix -> DependencySet -> Bool -> Set Formula -> [PrFormula]
prefix p bps b fs = [PrFormula p bps b formula|formula <- list fs]

firstPrefixedFormula :: Formula -> PrFormula
firstPrefixedFormula = PrFormula 0 dsEmpty True

-- formula language

data LanguageInfo = LanguageInfo { languageNoms :: [C.ByteString] } -- ascending

instance Show LanguageInfo where
 show li = "Input Language:\n|" ++ yesnol "Noms " ( languageNoms li )
  where yesnol s l | null l = "no " ++ s
        yesnol s l = s ++ intercalate ", " (map C.unpack l)

langInfo :: [Formula] -> LanguageInfo
langInfo fs
 = LanguageInfo { languageNoms = noms_ }
    where noms_ = nub $ concatMap noms fs

noms :: Formula -> [C.ByteString]
noms (Con fs) = concat $ Set.map noms fs
noms (Dis fs) = concat $ Set.map noms fs
noms (Dia f) = noms f
noms (Box f) = noms f
noms (A f) = noms f
noms (E f) = noms f
noms (At n f) = n:(noms f)
noms (Neg f) = noms f
noms (Atm (N n)) = [n]
noms _ = [] 

-- composeXX functions follow the idea from
-- "A pattern for almost compositional functions", Bringert and Ranta.
composeMap :: (Formula -> Formula)
           -> (Formula -> Formula)
           -> (Formula -> Formula)
composeMap baseCase g e = case e of
    Con fs     -> Con $ Set.map g fs
    Dis fs     -> Dis $ Set.map g fs
    Dia f      -> Dia (g f)
    Box f      -> Box (g f)
    At   i f   -> At  i (g f)
    A f        -> A (g f)
    E f        -> E (g f)
    Down x f   -> Down x (g f)
    f          -> baseCase f

replaceVar :: C.ByteString -> C.ByteString -> Formula -> Formula
replaceVar v n a@(Atm (N v2)) = if v == v2 then Atm (N n) else a
replaceVar v n a@(Down v2 f) = if v == v2 then a   -- variable capture
                                          else Down v2 (replaceVar v n f)
replaceVar v n (At v2 f)   = if v == v2 then At n (replaceVar v n f)
                                        else At v2 (replaceVar v n f)
replaceVar v n f = composeMap id (replaceVar v n) f

-- backjumping

type Dependency = Int
type DependencySet = IntSet.IntSet

-- the ordering of prformula's is used in selecting the next formula in the todo list
-- here we select the one that's most promising for backjumping
instance Ord PrFormula where
 compare (PrFormula pr1 ds1 _ f1) (PrFormula pr2 ds2 _ f2) =
-- This one seems more performant in many cases:

-- case dsMin ds1 `compare` dsMin ds2 of

-- But this one seems 'fairer' and helps termination on some complicated formulas
  case  IntSet.size ds1  `compare` IntSet.size ds2 of
   LT -> LT
   GT -> GT
   EQ -> compare (pr1,f1,ds1) (pr2,f2,ds2)

dsUnion :: DependencySet -> DependencySet -> DependencySet
dsUnion = IntSet.union

dsUnions :: [DependencySet] -> DependencySet
dsUnions = IntSet.unions

dsInsert :: Dependency -> DependencySet -> DependencySet
dsInsert = IntSet.insert

dsMember :: Dependency -> DependencySet -> Bool
dsMember = IntSet.member

dsEmpty :: DependencySet
dsEmpty = IntSet.empty

dsMin :: DependencySet -> Int
dsMin ds = maybe 0 fst $ IntSet.minView ds

dsShow :: DependencySet -> String
dsShow = show . IntSet.toList

addDeps :: DependencySet -> PrFormula -> PrFormula
addDeps ds1 (PrFormula p ds2 b f) = PrFormula p (dsUnion ds1 ds2) b f

list :: Ord a => Set.Set a -> [a]
list = Set.toList

set :: Ord a => [a] -> Set.Set a
set = Set.fromList
