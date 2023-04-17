module Deftab.Relations

( OutRels, emptyRels, insertRelation, mergePrefixes,
  successors, null,
  showRels, allRels )

where

-- data structure for monomodal logic

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as I

import qualified Data.List as List

import qualified Deftab.DMap as D

import Deftab.Formula (Prefix, DependencySet, dsShow )
import Prelude hiding (id, pred, succ, null)

type OutRels = IntMap {- Prefix -} (Map Prefix DependencySet)

emptyRels :: OutRels
emptyRels = I.empty

null :: OutRels -> Bool
null = I.null

allRels :: OutRels -> [(Prefix,Prefix)]
allRels rels = [ (p1,p2) | (p1,p_dss) <-  I.assocs rels,
                           (p2,_) <- M.toList p_dss ]

successors :: OutRels -> Prefix -> [(Prefix,DependencySet)]
successors rels p = M.toList $ I.findWithDefault M.empty p rels

-- assumes you never add twice the same relation
insertRelation :: OutRels -> Prefix -> Prefix -> DependencySet -> OutRels
insertRelation rels p1 p2 ds =
  case I.lookup p1 rels of
      Nothing       -> I.insert p1 (M.singleton p2 ds) rels
      Just innerMap -> I.insert p1 (M.insert p2 ds innerMap) rels

mergePrefixes :: OutRels -> Prefix -> Prefix -> DependencySet -> OutRels
mergePrefixes r pr ur _ | pr == ur = r
mergePrefixes r pr ur ds = D.moveInnerPlusDeps ds r pr ur

showRels :: OutRels -> String
showRels r = prettyShowMap_ r (\v -> "(" ++ prettyShowMap_rel_bps_x (M.toList v) ++ ")") "\n "

prettyShowMap_ :: (Show y) => IntMap y -> (y -> String) -> String -> String
prettyShowMap_ m valueShow separator
 = List.intercalate separator $ map (\(k,v) -> show k ++ " -> " ++ valueShow v)
          $ I.toList m

prettyShowMap_rel_bps_x :: (Show a) => [(a,DependencySet)] -> String
prettyShowMap_rel_bps_x x_bp_s
 = List.intercalate ", "  $ map (\(x,bp) -> show x ++ " " ++ dsShow bp) x_bp_s
