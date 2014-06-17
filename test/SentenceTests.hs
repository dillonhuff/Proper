module SentenceTests(allSentenceTests) where

import CNF
import Sentence
import TestUtils

allSentenceTests = do
  testSimpleEval
  testIsValidByTruthTable
  testToCNF

testSimpleEval =
  testFunction (evalSentence simpleTA) evalCases
  
simpleTA = truthAssignment ["p", "q", "r", "s"] [True, False, True, True]

evalCases =
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
  
testIsValidByTruthTable =
  testFunction isValidByTruthTable isValidTTCases
  
isValidTTCases =
  [(val "a", False),
   (dis (val "a") (neg (val "a")), True),
   (dis (val "a") (val "a"), False),
   (imp (con (val "a") (val "b")) (val "a"), True),
   (imp (val "q") (con (val "a") (val "q")), False),
   (bic (neg (neg (val "b"))) (val "b"), True),
   (imp (con (val "p") (val "q")) (val "p"), True),
   (imp (dis (val "p") (val "q")) (val "p"), False),
   (bic (neg (con (val "p") (val "q"))) (dis (neg (val "p")) (neg (val "q"))), True)]
  
testToCNF =
  testFunction toCNF toCNFCases
  
toCNFCases =
  [(val "a", cnf $ [clause [lit "a"]]),
   (neg (val "p"), cnf $ [clause [nLit "p"]]),
   (dis (dis (val "p") (dis (val "q") (neg (val "r")))) (val "s"),
    cnf $ [clause [lit "p", lit "q", nLit "r", lit "s"]]),
   (bic (val "p") (val "q"), cnf $ [clause [nLit "p", lit "q"], clause [nLit "q", lit "p"]]),
   (imp (val "p") (val "q"), cnf $ [clause [nLit "p", lit "q"]]),
   (neg (con (neg (val "p")) (val "q")), cnf $ [clause [lit "p", nLit "q"]])]