module Proper.ParserTests(allParserTests) where

import Proper.Lexer
import Proper.Parser
import Proper.Sentence
import Proper.TestUtils
import Proper.Utils

allParserTests = do
  testParseFormula
  
testParseFormula =
  testFunction strToFormula formulaTestCases
  where
    strToFormula str = extractValue (toTokens str >>= parseFormula)
  
formulaTestCases =
  [("an", val "an"),
   ("~an12d", neg (val "an12d")),
   ("~(a | ~ p )", neg (dis (val "a") (neg (val "p")))),
   ("~(a | ~(~p)) <-> ~a & ~p",
    bic (neg (dis (val "a") (neg (neg (val "p"))))) (con (neg (val "a")) (neg (val "p"))))]