-- | Propositional helpers module
module PropositionalHelpers where

import           PropositionalTypes

int2bool :: Int -> Int -> [Bool]
int2bool _ 0 = [False]
int2bool 0 i = False : int2bool 0 (i - 1)
int2bool n i = if even n
  then False : int2bool (div n 2) (i - 1)
  else True : int2bool (div n 2) (i - 1)

-- | removeDuplicates function returns a list without duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates =
  foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

subValuation :: [Valuation] -> Valuation -> Bool
subValuation [v] superval = and
  [ or [ (supN == subN) && (supB == subB) | (supN, supB) <- superval ]
  | (subN, subB) <- v
  ]
subValuation subvals superval = or [ subValuation [v] superval | v <- subvals ]

subsumes :: [Valuation] -> [Valuation] -> Bool
subsumes subsumer subsumed = and [ subValuation subsumed v | v <- subsumer ]

subList :: Valuation -> Valuation -> Bool
subList [] [] = True
subList _  [] = False
subList [] _  = True
subList ((xn, xb) : xs) ((yn, yb) : ys) | xn == yn && xb == yb = subList xs ys
                                        | otherwise = subList ((xn, xb) : xs) ys
