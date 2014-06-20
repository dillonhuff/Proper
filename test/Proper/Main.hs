module Main(main) where

import CNFTests
import LexerTests
import ParserTests
import SentenceTests

main = do
  allSentenceTests
  allCNFTests
  allLexerTests
  allParserTests