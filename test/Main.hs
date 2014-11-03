module Main(main) where

import Proper.BDDTests
import Proper.CNFTests
import Proper.LexerTests
import Proper.ParserTests
import Proper.FormulaTests

main = do
  allFormulaTests
  allCNFTests
  allLexerTests
  allParserTests
  allBDDTests
