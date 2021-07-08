-- | Defeasible logic module
module DefeasibleLogic where

import           DefeasibleHelpers
import           DefeasibleTypes
import           PropositionalTypes

-- | The 'twiddle' function takes in two propositional 'Formula's
-- and returns a 'DefeasibleImplication'.
-- NOTE: This is a temporary function until a parser is built to handle this
typically :: Formula -> DefeasibleImplication
typically (Implies p q) = Typically p q

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

-- | The 'entailsLC' function returns whether or not the given 'DefeasibleInterpretation' is
-- defeasibly entailed (using Lexicographic Closure) by the given 'JointKnowledgeBase'.
-- NOTE: This is a highly experimental function and its correctness has not been validated.
entailsLC :: JointKnowledgeBase -> DefeasibleImplication -> Bool
entailsLC jkb p = entailsLCAux (baseRank jkb) (head (materialise [p]))
