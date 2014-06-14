module Sentence(
               neg, con, dis, val, bic, imp,
               truthAssignment,
               evalSentence) where

import Data.Map as M

type Name = String

data Sentence =
  Val Name              |
  Neg Sentence          |
  Con Sentence Sentence |
  Dis Sentence Sentence |
  Bic Sentence Sentence |
  Imp Sentence Sentence
  deriving (Eq, Ord, Show)
           
neg sent = Neg sent
con s1 s2 = Con s1 s2
dis s1 s2 = Dis s1 s2
bic s1 s2 = Bic s1 s2
imp s1 s2 = Imp s1 s2
val name = Val name

type TruthAssignment = Map Sentence Bool

truthAssignment :: [Name] -> [Bool] -> TruthAssignment
truthAssignment constNames constVals = M.fromList $ zip constants constVals
  where
    constants = Prelude.map val constNames

evalSentence :: TruthAssignment -> Sentence -> Bool
evalSentence a (Neg s) = not $ evalSentence a s
evalSentence a (Con s1 s2) = (evalSentence a s1) && (evalSentence a s2)
evalSentence a (Dis s1 s2) = (evalSentence a s1) || (evalSentence a s2)
evalSentence a (Imp s1 s2) = (not s1Eval) || s2Eval
  where
    s1Eval = evalSentence a s1
    s2Eval = evalSentence a s2
evalSentence a (Bic s1 s2) = (s1Eval && s2Eval) || ((not s1Eval) && (not s2Eval))
  where
    s1Eval = evalSentence a s1
    s2Eval = evalSentence a s2
evalSentence a c@(Val name) = case M.lookup c a of
  Just boolVal -> boolVal
  Nothing -> error $ "Constant with not truth value " ++ show c