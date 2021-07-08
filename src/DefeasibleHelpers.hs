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

-- | The 'combinations' function returns the combinatorial subsets of a list
-- of a specified size.
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n (x : xs) =
  map (x :) (combinations (n - 1) xs) ++ combinations n xs

-----------------
-- AUXILIARIES --
-----------------

-- | The 'baseRankAux' function is the auxiliary function for the 'baseRank' function.
baseRankAux :: KnowledgeBase -> KnowledgeBase -> RankedStates -> RankedStates
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
entailsRCAux :: RankedStates -> Formula -> Bool
entailsRCAux [] p = isValid p
entailsRCAux (s : se) p
  | null se = kb `entails` Not ante
  | otherwise = if kb `entails` Not ante
    then entailsRCAux se p
    else kb `entails` p
 where
  kb   = [ q | r <- s : se, q <- r ]
  ante = antecedant p

-- | The 'entailsLCAux' function is the auxiliary function for the 'entailsLC' function.
-- NOTE: This is a highly experimental function and its correctness has not been validated.
entailsLCAux :: RankedStates -> Formula -> Bool
entailsLCAux [] p = isValid p
entailsLCAux (s : se) p
  | null se = kb `entails` Not ante
  | otherwise = if null selectedRefinement
    then entailsLCAux se p
    else kb `entails` p
 where
  ante        = antecedant p
  stateLength = length s
  refinements =
    [ stateComb
    | i         <- reverse [1 .. stateLength]
    , stateComb <- combinations i s
    ]
  selectedRefinement = head
    [ state
    | state <- refinements
    , entails [ q | r <- state : se, q <- r ] (Not ante)
    ]
  noRefinement = null selectedRefinement
  kb           = [ q | r <- selectedRefinement : se, q <- r ]
