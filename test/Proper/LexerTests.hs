module LexerTests(allLexerTests) where

import Lexer
import TestUtils
import Utils

allLexerTests = do
  testLexerOneToken
  
testLexerOneToken =
  testFunction (extractValue . toTokens) oneTokenCases
  
oneTokenCases =
  [("p", [testId "p"]),
   ("p123", [testId "p123"]),
   ("aBRACADABRA", [testId "aBRACADABRA"]),
   ("AXIOMS:", [testRes "AXIOMS:"]),
   ("HYPOTHESIS:", [testRes "HYPOTHESIS:"]),
   ("(", [testSep "("]),
   (")", [testSep ")"]),
   ("&", [testOp "&"]),
   ("|", [testOp "|"]),
   ("~", [testOp "~"]),
   ("->", [testOp "->"]),
   ("<->", [testOp "<->"])]