-- | Defeasible helpers module
module DefeasibleHelpers where

import           DefeasibleTypes
import           PropositionalLogic
import           PropositionalTypes

---------------------
-- GENERAL HELPERS --
---------------------

-- | The 'antecedant' function returns the antecedant for a given 'Formula'.
antecedant :: Formula -> Formula
antecedant (Const b    ) = Const b
antecedant (Atom  p    ) = Atom p
antecedant (Not   p    ) = Not p
antecedant (And     p _) = p
antecedant (Or      p _) = p
antecedant (Implies p _) = p
antecedant (Iff     p _) = p

-----------------
-- AUXILIARIES --
-----------------

-- | The 'baseRankAux' function is the auxiliary function for the 'baseRank' function.
baseRankAux :: KnowledgeBase -> KnowledgeBase -> RankedModels -> RankedModels
baseRankAux []  kb r = r ++ [kb]
baseRankAux mkb kb r = baseRankAux result kb (r ++ [state])
 where
  state =
    [ p
    | p    <- mkb
    , ante <- [antecedant p]
    , not ((mkb ++ kb) `entails` Not ante)
    ]
  result = filter (`notElem` state) mkb

-- | The 'entailsRCAux' function is the auxiliary function for the 'entailsRC' function.
entailsRCAux :: RankedModels -> Formula -> Bool
entailsRCAux [] p = isValid p
entailsRCAux (s : se) p
  | null se = kb `entails` Not ante
  | otherwise = if kb `entails` Not ante
    then entailsRCAux se p
    else kb `entails` p
 where
  kb   = [ q | r <- s : se, q <- r ]
  ante = antecedant p
