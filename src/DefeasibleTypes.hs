-- | Defeasible logic types module
module DefeasibleTypes where

import           PropositionalTypes

-- | The 'DefeasibleImplication' type encodes a typical consequence relation.
data DefeasibleImplication = Typically Formula Formula

-- | The 'State' type encodes a preferential 'State'.
type State = [Formula]

-- | The 'RankedModels' type encodes preferentially ranked models.
type RankedModels = [State]

-- | The 'DefeasibleKnowledgeBase' type encodes a defeasible knowledge base.
type DefeasibleKnowledgeBase = [DefeasibleImplication]

-- | The 'JointKnowledgeBase' type encodes a knowledge base /
-- defeasible knowledge base relation.
type JointKnowledgeBase = (KnowledgeBase, DefeasibleKnowledgeBase)

-- | The 'Show' instance for a 'DefeasibleImplication'.
instance Show DefeasibleImplication where
  show (Typically p q) = show p ++ "~>" ++ show q
