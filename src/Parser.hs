module Parser(
  parseFormula) where

import Lexer
import Sentence
import Text.Parsec.Expr
import Text.Parsec.Pos
import Text.Parsec.Prim
import Utils

parseFormula :: [Token] -> Error Sentence
parseFormula toks = case parse parseForm "PARSER" toks of
  Left err -> Failed $ show err
  Right formula -> Succeeded formula

parseForm = buildExpressionParser table parseTerm

table =
  [[negation],
   [conjunction],
   [disjunction],
   [implication],
   [bicondition]]

negation = Prefix parseNeg
conjunction = Infix parseCon AssocRight
disjunction = Infix parseDis AssocRight
implication = Infix parseImp AssocRight
bicondition = Infix parseBic AssocRight

parseParens e = do
  propTok "("
  expr <- e
  propTok ")"
  return expr

parseNeg :: (Monad m) => ParsecT [Token] u m (Sentence -> Sentence)
parseNeg = do
  propTok "~"
  return $ neg
  
parseCon = do
  propTok "&"
  return $ con
  
parseDis = do
  propTok "|"
  return $ dis
  
parseImp = do
  propTok "->"
  return $ imp
  
parseBic = do
  propTok "<->"
  return $ bic
  
parseTerm = parseParens parseForm <|> parseLit

parseLit :: (Monad m) => ParsecT [Token] u m Sentence
parseLit = do
  litTok <- literalTok
  return $ val (name litTok)

propTok :: (Monad m) => String -> ParsecT [Token] u m Token
propTok str = tokenPrim show updatePos hasNameStr
  where
    hasNameStr t = if (name t) == str then Just t else Nothing

literalTok :: (Monad m) => ParsecT [Token] u m Token
literalTok = tokenPrim show updatePos isLit
  where
    isLit t = if (isId t) then Just t else Nothing

axiomsTok c = tokenPrim show updatePos isAxiom
  where
    isAxiom t = if (name t) == "AXIOMS:" then Just t else Nothing

hypothesisTok c = tokenPrim show updatePos isAxiom
  where
    isAxiom t = if (name t) == "HYPOTHESIS:" then Just t else Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position