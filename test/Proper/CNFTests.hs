module Proper.CNFTests(allCNFTests) where

import Data.Map as M

import Proper.Clause
import Proper.CNF
import Proper.Sentence
import Proper.TestUtils

allCNFTests = do
  testNaiveSAT
  testNaiveSATComplicated
  testNaiveSATAssignments

testNaiveSATAssignments =
  testFunction naiveSAT satAssignCases

satAssignCases =
  [(cnf $ [clause [lit "a"], clause [nLit "a"]], Nothing),
   (cnf $ [clause [lit "a", nLit "a"]], Just $ M.fromList [("a", True)]),
   (cnf $ [clause [lit "p"]], Just $ M.fromList [("p", True)]),
   (cnf $ [clause [nLit "a", lit "p"]], Just $ M.fromList [("a", True), ("p", True)]),
   (cnf $ [clause [nLit "p"],
           clause [lit "p", nLit "s"],
           clause [lit "p"]], Nothing),
   (toCNF (imp (val "a") (val "a")), Just $ M.fromList [("a", True)]),
   (toCNF (bic (val "c") (val "d")), Just $ M.fromList [("c", True), ("d", True)]),
   (toCNF (neg (bic (val "c") (val "c"))), Nothing)]

  
testNaiveSAT =
  testFunction naiveSATBool satCases
  
satCases =
  [(cnf $ [clause [lit "a"], clause [nLit "a"]], False),
   (cnf $ [clause [lit "p"]], True),
   (cnf $ [clause [nLit "a", lit "p"]], True),
   (cnf $ [clause [nLit "p"],
           clause [lit "p", nLit "s"],
           clause [lit "p"]], False),
   (toCNF (imp (val "a") (val "a")), True),
   (toCNF (bic (val "c") (val "d")), True),
   (toCNF (neg (bic (val "c") (val "c"))), False)]
  
testNaiveSATComplicated =
  testFunction naiveSATBool complicatedCases
  
complicatedCases =
  [(toCNF (neg (bic (neg (con (val "p") (val "q"))) (dis (neg (val "p")) (neg (val "q"))))), False),
   (toCNF (bic (neg (dis (val "p") (val "q"))) (con (neg (val "p")) (neg (val "q")))), True),
   (toCNF
    (neg (bic
          (neg (dis (con (val "a") (neg (val "c"))) (dis (val "d") (val "f"))))
          (con (dis (neg (val "a")) (val "c")) (con (neg (val "d")) (neg (val "f")))))), False)]
