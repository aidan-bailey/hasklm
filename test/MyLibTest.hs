import Test.QuickCheck

import PropositionalLogic
import PropositionalTypes

main :: IO ()
main =
  quickCheck parserTest

parserTest = do
  let form = str2form "A And B"
  form == And (Atom "A") (Atom "B")
