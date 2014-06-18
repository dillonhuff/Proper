module Parser(
  parseFormula) where

import Lexer
import Sentence
import Utils

parseFormula :: [Token] -> Error Sentence
parseFormula toks = Failed "NOT IMPLEMENTED"
{-
table =
  [[postfixOp "~"],
   [infixOp "&"],
   [infixOp "|"],
   [infixOp "->"],
   [infixOp "<->"]]

prefixOp opStr = Prefix $ doUnoperator opStr
postfixOp opStr = Postfix $ doUnoperator opStr
infixOp opStr = Infix (doBinop opStr) AssocLeft

doUnoperator opStr = do
opVal <- cmcTok (opTok opStr)
return $ unaryOp (operator opStr)

doBinop opStr = do
opVal <- cmcTok (opTok opStr)
return $ binaryOp (operator opStr)

tokOfType :: (Monad m) => (Tok -> Bool) -> ParsecT [PosTok] u m PosTok
tokOfType isTokOfType = tokenPrim show updatePos idTok
where
idTok pt = if isTokOfType (tok pt) then Just pt else Nothing

cmcTok :: (Monad m) => Tok -> ParsecT [PosTok] u m PosTok
cmcTok x = tokenPrim show updatePos testTok
where
testTok pt = if (tok pt) == x then Just pt else Nothing

updatePos :: SourcePos -> PosTok -> [PosTok] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position
-}