module Deftab.RuleId(RuleId(..))

where

import Control.DeepSeq ( NFData, rnf )

data RuleId = R_Dia     -- Diamond <r>
            | R_Disj    -- Disjunction
            | R_At      -- Satisfaction operator (@) rule
            | R_Down    -- Down-arrow rule
            | R_Exist   -- Existential modality
            | R_DiscardDown
            | R_DiscardDiaDone
            | R_DiscardDiaBlocked
            | R_Merge   -- Equivalence classes merge
            -- Intuitionistic rules:
            | R_Neg     -- false negation
            | R_FImp    -- false implication
            | R_TImp    -- true implication
            | R_FDimp   -- false double implication
            | R_TDimp   -- true double implication
            | R_Default
        deriving(Eq, Ord, Show)

instance NFData RuleId where
 rnf ruleId = ruleId `seq` ()
