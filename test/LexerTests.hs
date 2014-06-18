module LexerTests(allLexerTests) where

import Lexer
import TestUtils
import Utils

allLexerTests = do
  testLexerOneToken
  
testLexerOneToken =
  testFunction (extractValue . toTokens) oneTokenCases
  
oneTokenCases =
  [("p", [testTok "p"]),
   ("p123", [testTok "p123"]),
   ("aBRACADABRA", [testTok "aBRACADABRA"]),
   ("AXIOMS:", [testTok "AXIOMS:"]),
   ("HYPOTHESIS:", [testTok "HYPOTHESIS:"]),
   ("(", [testTok "("]),
   (")", [testTok ")"]),
   ("&", [testTok "&"]),
   ("|", [testTok "|"]),
   ("~", [testTok "~"]),
   ("->", [testTok "->"]),
   ("<->", [testTok "<->"])]