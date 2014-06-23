module Proper.CNFTests(allCNFTests) where

import Proper.Clause
import Proper.CNF
import Proper.Sentence
import Proper.TestUtils

allCNFTests = do
  testNaiveSAT
  testNaiveSATComplicated
  
testNaiveSAT =
  testFunction naiveSAT satCases
  
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
  testFunction naiveSAT complicatedCases
  
complicatedCases =
  [(toCNF (neg (bic (neg (con (val "p") (val "q"))) (dis (neg (val "p")) (neg (val "q"))))), False),
   (toCNF (bic (neg (dis (val "p") (val "q"))) (con (neg (val "p")) (neg (val "q")))), True),
   (toCNF
    (neg (bic
          (neg (dis (con (val "a") (neg (val "c"))) (dis (val "d") (val "f"))))
          (con (dis (neg (val "a")) (val "c")) (con (neg (val "d")) (neg (val "f")))))), False)]