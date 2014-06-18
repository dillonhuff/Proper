module ParserTests(allParserTests) where

import Lexer
import Parser

allParserTests = do
  testParseFormula
  
testParseFormula =
  testFunction parseFormula formulaTestCases
  
formulaTestCases =
  []