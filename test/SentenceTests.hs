module SentenceTests() where

import Sentence
import TestUtils

allSentenceTests = do
  testSimpleEval

testSimpleEval =
  testFunction (evalSentence simpleTA) simpleCases
  
simpleTA = truthAssignment ["p", "q", "r", "s"] [True, False, True, True]

simpleCases =
  [(neg (val "p"), False),
   (neg (val "q"), True),
   (con (val "p") (val "q"), False),
   (con (val "p") (val "p"), True),
   (dis (val "p") (val "q"), True),
   (dis (val "q") (val "q"), False),
   (bic (val "p") (val "s"), True),
   (bic (val "p") (val "q"), False),
   (bic (val "q") (val "s"), False),
   (imp (val "q") (val "s"), True),
   (imp (val "p") (val "q"), False)]