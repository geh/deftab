module Deftab.DisjSet
( DisjSet, Pointer(..), mkDSet, find, union, isRoot, onlyFind )
where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as C
import Deftab.Formula ( Nom )

-- a disjoint-set forest
type DisjSet x = Map.Map x x
-- meaning: if an element is *not* in DisjSet,
-- then it is a root of a class

-- invariant: there is no N such that (N,N) is in DisjSet
-- (it would provoke a look in the find function)



-- find the root of the tree in which a given element belongs to
-- at each call to "find", we optimise the DisjSet to link to the root
find :: Ord x => x -> DisjSet x -> (x,DisjSet x)
find n s = case Map.lookup n s of
             Nothing -> (n,s)
             Just parent -> let (ancestor, modifiedDisjSet) = find parent s
                             in 
                            (ancestor, Map.insert n ancestor modifiedDisjSet)


onlyFind :: Ord x => x -> DisjSet x -> x
onlyFind n s
 = case Map.lookup n s of
     Nothing -> n
     Just parent -> onlyFind parent s

-- union the sets in which a and b belong to
-- ensure : root of the merged set is the smallest root (min a b)
union :: Ord x => x -> x ->  DisjSet x -> DisjSet x
union a b s = case compare rootA rootB of
                EQ -> modifiedDisjSet2
                GT -> Map.insert rootA rootB modifiedDisjSet2
                LT -> Map.insert rootB rootA modifiedDisjSet2
               where (rootA,modifiedDisjSet1) = find a s
                     (rootB,modifiedDisjSet2) = find b modifiedDisjSet1

isRoot :: Ord x => x -> DisjSet x -> Bool
isRoot n s = Map.notMember n s

-- constructor of an empty disjoint-set forest
mkDSet :: Ord x => DisjSet x
mkDSet = Map.empty::DisjSet x

-- this should be outside of the module
data Pointer = Prefix Int | Nominal Nom
 deriving (Eq)

instance Show Pointer where
 show (Prefix p)  = '#' : show p
 show (Nominal n) = C.unpack n

instance Ord Pointer where
 compare (Prefix i1)  (Prefix i2)  = compare i1 i2
 compare (Nominal i1) (Nominal i2) = compare i1 i2
 compare (Nominal _)  (Prefix _)   = GT
 compare (Prefix _)   (Nominal _)  = LT

-- We have this order (p: prefix , n: nominal):
-- p0 < p1 < ... < pn < n0 < n1 < ... < nm

-- The representative of a set is the smallest (by this order) element of the set.
-- Good news : a set always contains a prefix, so the reprensentative
-- is always the earliest prefix of the set.

