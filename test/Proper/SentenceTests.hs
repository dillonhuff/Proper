module Proper.SentenceTests(allSentenceTests) where

import Proper.CNF
import Proper.Sentence
import Proper.TestUtils

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
   (neg (con (neg (val "p")) (val "q")), cnf $ [clause [lit "p", nLit "q"]]),
   (neg (dis (val "p") (neg (val "q"))), cnf $ [clause [nLit "p"], clause [lit "q"]]),
   (neg (neg (neg (val "p"))), cnf $ [clause [nLit "p"]]),
   (dis (val "p") (con (val "q") (val "r")), cnf $ [clause [lit "p", lit "q"], clause [lit "p", lit "r"]]),
   (dis (con (val "p") (val "q")) (val "r"), cnf $ [clause [lit "p", lit "r"], clause [lit "q", lit "r"]]),
   (neg (bic (val "y") (neg (val "z"))),
    cnf $
    [clause [lit "y", nLit "z"],
     clause [lit "z", nLit "z"],
     clause [lit "y", nLit "y"],
     clause [lit "z", nLit "y"]]),
   (neg (con (val "x") (neg (bic (val "y") (neg (val "z"))))),
    cnf $ [clause [nLit "x", nLit "y", nLit "z"],
           clause [nLit "x", lit "z", lit "y"]]),
   (dis (val "p") (dis (val "q") (con (val "r") (val "s"))),
    cnf $ [clause [lit "p", lit "q", lit "r"], clause [lit "p", lit "q", lit "s"]]),
   (dis (dis (dis (con (dis (val "a") (val "b")) (val "c")) (val "d")) (val "e")) (val "f"),
    cnf $ [clause [lit "a", lit "b", lit "d", lit "e", lit "f"],
           clause [lit "c", lit "d", lit "e", lit "f"]])]