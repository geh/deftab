module Deftab.Model ( buildModel )

where

import Deftab.Branch( Branch(..))

buildModel :: Branch -> String
buildModel br = show (literals br, accStr br)
 where
  -- todo transitivity
  -- todo add blocked diamonds
  {-
  pbBlocked =
         [ (pr, pr2) |
             pr <- prefixes br,
             isInTheModel br pr,
             blockedDia@(PrFormula _ _ (Dia _)) <- get [] pr (blockedDias br),
             let pat = patternOf br blockedDia,
             let pr2 = findByPattern br pat ]
  rels = (allRels $ accStr br) ++ pbBlocked
  rs = Set.fromList
        $ map toSimpSig
        $ map (\(p1,p2) -> ((p1 `inModel` br) + bias , (p2 `inModel` br) + bias)) rels


prefixAndProps :: Branch -> [(Prefix,S.PropSymbol)]
prefixAndProps br =
  [(pr, S.PropSymbol s) | (pr , p_) <- prPosLitProp ++ prefWitPositive,
                           let (PosLit (P s)) = p_ ]
 where
  litsRelevant = I.filterWithKey (\k _ -> isInTheModel br k) (literals br)
  prPosLitProp = [ (a,b)  | (a,b,_)  <- flatten litsRelevant, 
                            isPositiveProp b ]
  --
  witMap = brWitnesses br
  witMapRelevant = I.filterWithKey (\k _ -> isInTheModel br k) witMap
  prefWitPositive = [ (a,b)  | (a,b,_)  <- flatten witMapRelevant,
                               isPositiveProp b ]

completeTrans :: Model -> Model
completeTrans m
 = m{M.succs = \rs@(S.RelSymbol r) w
                 -> if isTransitive relI r
                     then getTransClos (M.succs m) rs w
                     else M.succs m rs w}

getTransClos :: (Ord w) => (r -> w -> Set w) -> r -> w -> Set w
getTransClos succs_ r_ w_
 = go Set.empty Set.empty succs_ r_ w_
 where
  go seen todo succs r w
   = case Set.minView todo1 of
      Nothing                -> seen
      Just (nextWorld,todo2) -> go (Set.insert nextWorld seen) todo2 succs r nextWorld
     where todo1  = (succs r w `Set.union` todo) `Set.difference` seen

get :: a -> Int -> I.IntMap a -> a
get = I.findWithDefault

-}
