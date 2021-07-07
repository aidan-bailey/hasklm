import           Test.QuickCheck

import           DefeasibleLogic
import           DefeasibleTypes
import           PropositionalLogic
import           PropositionalTypes

main :: IO ()
main = do
  quickCheck parserTest
  quickCheck rationalClosureTest

parserTest = do
  let form = str2form "A And B"
  form == And (Atom "A") (Atom "B")

rationalClosureTest = do
  let dkb =
        [ typically (str2form "b Implies f")
        , typically (str2form "p Implies Not f")
        , typically (str2form "b Implies w")
        ]
  let kb = [str2form "p Implies b", str2form "R Implies b"]
  let jkb   = (kb, dkb)
  let query = typically (str2form "p Implies b")
  entailsRC jkb query
