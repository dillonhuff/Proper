module ParserTests(allParserTests) where

import Lexer
import Parser
import TestUtils
import Utils

allParserTests = do
  testParseFormula
  
testParseFormula =
  testFunction (extractValue . parseFormula) formulaTestCases
  
formulaTestCases =
  []