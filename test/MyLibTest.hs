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
        [ Typically (str2form "b") (str2form "f")
        , Typically (str2form "p") (str2form "Not f")
        , Typically (str2form "b") (str2form "w")
        ]
  let kb = [str2form "p Implies b", str2form "R Implies b"]
  let jkb    = (kb, dkb)
  let kb2    = materialise (dkb) ++ kb
  let query  = Typically (str2form "p") (str2form "b")
  let query1 = (str2form "p Implies Not p")
  let form   = head (materialise [query])
  let ante = getAntecedant (head (materialise [query]))
  entailsRC jkb query
