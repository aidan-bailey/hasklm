-- | Defeasible logic module
module DefeasibleLogic where

import           DefeasibleTypes
import           PropositionalLogic
import           PropositionalTypes

getAntecedant :: Formula -> Formula
getAntecedant (Not p      ) = Not p
getAntecedant (And     p _) = p
getAntecedant (Or      p _) = p
getAntecedant (Implies p _) = p
getAntecedant (Iff     p _) = p

materialise :: DefeasibleKnowledgeBase -> KnowledgeBase
materialise []                    = []
materialise (Typically p q : dkb) = Implies p q : materialise dkb

baseRankAux :: KnowledgeBase -> KnowledgeBase -> RankedModels -> RankedModels
baseRankAux []  kb r = r ++ [kb]
baseRankAux mkb kb r = baseRankAux result kb (r ++ [state])
 where
  state =
    [ p
    | p    <- mkb
    , ante <- [getAntecedant p]
    , not ((mkb ++ kb) `entails` (Not ante))
    ]
  result = filter (`notElem` state) mkb

baseRank :: JointKnowledgeBase -> RankedModels
baseRank (kb, dkb) = baseRankAux (materialise dkb) kb []

entailsRCAux :: RankedModels -> Formula -> Bool
entailsRCAux (s : se) p
  | null se = kb `entails` (Not ante)
  | otherwise = if kb `entails` (Not ante)
    then entailsRCAux se p
    else kb `entails` p
 where
  kb   = [ q | s <- (s : se), q <- s ]
  ante = getAntecedant p

entailsRC :: JointKnowledgeBase -> DefeasibleImplication -> Bool
entailsRC jkb p = entailsRCAux (baseRank jkb) (head (materialise [p]))
