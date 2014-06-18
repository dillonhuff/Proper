module Lexer(
  Token,
  toTokens,
  testTok) where

import Text.Parsec.Pos
import Text.ParserCombinators.Parsec
import Utils

data Token = Tok String SourcePos

instance Show Token where
  show = showTok
                      
instance Eq Token where
  (==) = tokEqual
  
testTok :: String -> Token
testTok s = Tok s (newPos "DUMMY" 0 0)

showTok :: Token -> String
showTok (Tok name _) = "<" ++ name ++ ">"

tokEqual :: Token -> Token -> Bool
tokEqual (Tok s1 _) (Tok s2 _) = s1 == s2

toTokens :: String -> Error [Token]
toTokens str = case parse parseToks "LEXER" str of
  Left err -> Failed $ show err
  Right toks -> Succeeded $ toks

parseToks = endBy parseTok spaces

parseTok = do
  pos <- getPosition
  tok <- try atomicLit
         <|> try reservedWord
         <|> try separator
         <|> operator
  return $ Tok tok pos
  
atomicLit = do
  firstChar <- lower
  rest <- many alphaNum
  return (firstChar:rest)
  
reservedWord = try (string "AXIOMS:") <|> (string "HYPOTHESIS:")

separator = choice $ map string ["(", ")"]

operator = choice $ map string ["~", "|", "&", "<->", "->"]