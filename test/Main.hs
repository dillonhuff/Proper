module Main(main) where

import Proper.CNFTests
import Proper.LexerTests
import Proper.ParserTests
import Proper.SentenceTests

main = do
  allSentenceTests
  allCNFTests
  allLexerTests
  allParserTests