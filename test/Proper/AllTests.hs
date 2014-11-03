module Proper.AllTests() where

import Proper.BDDTests
import Proper.FormulaTests
import Proper.LexerTests
import Proper.ParserTests

allTests = do
  allBDDTests
  allFormulaTests
  allLexerTests
  allParserTests
