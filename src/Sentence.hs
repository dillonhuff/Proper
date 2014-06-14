module Sentence(
               neg, con, dis, val, bic, imp,
               truthAssignment,
               evalSentence,
               isValidByTruthTable) where

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

constants :: Sentence -> [Sentence]
constants (Val n) = [(Val n)]
constants (Neg s) = constants s
constants (Con s1 s2) = constants s1 ++ constants s2
constants (Dis s1 s2) = constants s1 ++ constants s2
constants (Bic s1 s2) = constants s1 ++ constants s2
constants (Imp s1 s2) = constants s1 ++ constants s2

type TruthAssignment = Map Sentence Bool

truthVal :: Sentence -> TruthAssignment -> Bool
truthVal s tt = case M.lookup s tt of
  Just val -> val
  Nothing -> error $ "Sentence not in truth table " ++ show s

truthAssignment :: [Name] -> [Bool] -> TruthAssignment
truthAssignment constNames constVals = M.fromList $ zip consts constVals
  where
    consts = Prelude.map val constNames

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
evalSentence a constant = truthVal constant a
  
type TruthTable = [TruthAssignment]

containsSentence :: Sentence -> TruthTable -> Bool
containsSentence s [] = False
containsSentence s tt = M.member s (head tt)

truthTable :: [Sentence] -> TruthTable
truthTable sents = Prelude.foldl addSentence [] sents

addSentence :: TruthTable -> Sentence -> TruthTable
addSentence tt s = if (containsSentence s tt)
  then tt
  else addNewSentence tt s

addNewSentence :: TruthTable -> Sentence -> TruthTable
addNewSentence [] c@(Val n) = [truthAssignment [n] [True], truthAssignment [n] [False]]
addNewSentence tt c@(Val n) = ttFalse ++ ttTrue
 where
   ttFalse = Prelude.map (\ta -> M.insert c False ta) tt
   ttTrue = Prelude.map (\ta -> M.insert c True ta) tt
addNewSentence tt s = Prelude.map (addCompoundSentence s) tt

addCompoundSentence :: Sentence -> TruthAssignment -> TruthAssignment
addCompoundSentence s ta = insert s sval ta
  where
    sval = evalSentence ta s

truthTableForSentence :: Sentence -> TruthTable
truthTableForSentence s = truthTable $ (constants s) ++ [s]
                          
isValidByTruthTable :: Sentence -> Bool
isValidByTruthTable s = and sTruthVals
  where
    sTruthTable = truthTableForSentence s
    sTruthVals = Prelude.map (truthVal s) sTruthTable