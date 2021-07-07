-- | Defeasible logic module
module DefeasibleLogic where

import           DefeasibleHelpers
import           DefeasibleTypes
import           PropositionalTypes

-- | The 'materialise' function returns the corresponding 'KnowledgeBase' for
-- the given 'DefeasibleKnowledgeBase'.
materialise :: DefeasibleKnowledgeBase -> KnowledgeBase
materialise []                    = []
materialise (Typically p q : dkb) = Implies p q : materialise dkb

-- | The 'baseRank' function returns the 'RankedModels' for the given 'JointKnowledgeBase'.
baseRank :: JointKnowledgeBase -> RankedModels
baseRank (kb, dkb) = baseRankAux (materialise dkb) kb []

-- | The 'entailsRC' function returns whether or not the given 'DefeasibleInterpretation' is
-- defeasibly entailed (using Rational Closure) by the given 'JointKnowledgeBase'.
entailsRC :: JointKnowledgeBase -> DefeasibleImplication -> Bool
entailsRC jkb p = entailsRCAux (baseRank jkb) (head (materialise [p]))
