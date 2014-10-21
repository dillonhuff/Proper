module Proper.BDDTests(allBDDTests) where

import Proper.BDD
import Proper.Sentence
import Proper.TestUtils

allBDDTests = do
  bddCheckTautTests
  
bddCheckTautTests =
  testFunction bddCheckTaut checkTautTests
  
checkTautTests =
  [(neg (val "a"), False),
   (dis (val "a") (neg (val "a")), True),
   (con (val "a") (val "b"), False),
   (imp (bic (val "a") (neg (val "b"))) (imp (neg (val "b")) (val "a")), True)]
