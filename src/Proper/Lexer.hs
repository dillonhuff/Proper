module Proper.Lexer(
  Token, name, pos,
  isId,
  toTokens,
  testId, testRes, testOp, testSep) where

import Text.Parsec.Pos
import Text.ParserCombinators.Parsec
import Proper.Utils

data Token = 
  Id String SourcePos  |
  Sep String SourcePos |
  Op String SourcePos  |
  Res String SourcePos

instance Show Token where
  show = showTok
                      
instance Eq Token where
  (==) = tokEqual
  
isId (Id _ _) = True
isId _ = False

name (Id n _) = n
name (Sep n _) = n
name (Op n _) = n
name (Res n _) = n

pos (Id _ p) = p
pos (Sep _ p) = p
pos (Op _ p) = p
pos (Res _ p) = p

testId s = Id s (newPos "DUMMY" 0 0)
testSep s = Sep s (newPos "DUMMY" 0 0)
testOp s = Sep s (newPos "DUMMY" 0 0)
testRes s = Res s (newPos "DUMMY" 0 0)

showTok :: Token -> String
showTok t = name t

tokEqual :: Token -> Token -> Bool
tokEqual t1 t2 = name t1 == name t2

toTokens :: String -> Error [Token]
toTokens str = case parse parseToks "LEXER" str of
  Left err -> Failed $ show err
  Right toks -> Succeeded $ toks

parseToks = endBy parseTok spaces

parseTok = try atomicLit
         <|> try reservedWord
         <|> try separator
         <|> operator
  
atomicLit = do
  pos <- getPosition
  firstChar <- lower
  rest <- many alphaNum
  return $ Id (firstChar:rest) pos
  
reservedWord = do
  pos <- getPosition
  name <- try (string "AXIOMS:") <|> (string "HYPOTHESIS:")
  return $ Res name pos

separator = do
  pos <- getPosition
  name <- choice $ map string ["(", ")"]
  return $ Sep name pos

operator = do
  pos <- getPosition
  name <- choice $ map string ["~", "|", "&", "<->", "->"]
  return $ Op name pos