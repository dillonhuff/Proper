module Proper.LexerTests(allLexerTests) where

import Proper.Lexer
import Proper.TestUtils
import Proper.Utils

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