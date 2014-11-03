module Proper.Formula(
  Formula, checkTheorem,
  neg, con, dis, val, bic, imp,
  truthAssignment,
  evalFormula,
  isValidByTruthTable,
  toCNF, theorem,
  bddCheckTaut) where

import Data.Foldable
import Data.Monoid
import Data.Map as M
import Proper.BDD
import Proper.Clause
import Proper.CNF
import Proper.Utils

data Formula s =
  Val s                         |
  Neg (Formula s)              |
  Con (Formula s) (Formula s) |
  Dis (Formula s) (Formula s) |
  Bic (Formula s) (Formula s) |
  Imp (Formula s) (Formula s)
  deriving (Eq, Ord)

instance Functor Formula where
  fmap f (Val v) = Val (f v)
  fmap f (Neg s) = Neg (fmap f s)
  fmap f (Con s1 s2) = Con (fmap f s1) (fmap f s2)
  fmap f (Dis s1 s2) = Dis (fmap f s1) (fmap f s2)
  fmap f (Bic s1 s2) = Bic (fmap f s1) (fmap f s2)
  fmap f (Imp s1 s2) = Imp (fmap f s1) (fmap f s2)

instance Foldable Formula where
  foldMap f (Val v) = f v
  foldMap f (Neg s) = foldMap f s
  foldMap f (Con s1 s2) = mappend (foldMap f s1) (foldMap f s2)
  foldMap f (Dis s1 s2) = mappend (foldMap f s1) (foldMap f s2)
  foldMap f (Imp s1 s2) = mappend (foldMap f s1) (foldMap f s2)
  foldMap f (Bic s1 s2) = mappend (foldMap f s1) (foldMap f s2)
  
instance Show s => Show (Formula s) where
  show = showSent
  
showSent :: (Show s) => (Formula s) -> String
showSent (Val name) = show name
showSent (Neg s) = "~(" ++ show s ++ ")"
showSent (Con s1 s2) = "(" ++ show s1 ++ " & " ++ show s2 ++ ")"
showSent (Dis s1 s2) = "(" ++ show s1 ++ " | " ++ show s2 ++ ")"
showSent (Bic s1 s2) = "(" ++ show s1 ++ " <-> " ++ show s2 ++ ")"
showSent (Imp s1 s2) = "(" ++ show s1 ++ " -> " ++ show s2 ++ ")"
           
neg sent = Neg sent
con s1 s2 = Con s1 s2
dis s1 s2 = Dis s1 s2
bic s1 s2 = Bic s1 s2
imp s1 s2 = Imp s1 s2
val name = Val name

constantsF :: Formula s -> [Formula s]
constantsF s = foldMap (\n -> [Val n]) s

constants :: Formula s -> [Formula s]
constants s = foldMap (\n -> [Val n]) s

type TruthAssignment s = Map (Formula s) Bool

truthVal :: (Ord s, Show s) => Formula s -> TruthAssignment s -> Bool
truthVal s tt = case M.lookup s tt of
  Just val -> val
  Nothing -> error $ "Formula not in truth table " ++ show s

truthAssignment :: (Ord s) => [s] -> [Bool] -> TruthAssignment s
truthAssignment constNames constVals = M.fromList $ zip consts constVals
  where
    consts = Prelude.map val constNames

evalFormula :: (Ord s, Show s) => TruthAssignment s -> Formula s -> Bool
evalFormula a (Neg s) = not $ evalFormula a s
evalFormula a (Con s1 s2) = (evalFormula a s1) && (evalFormula a s2)
evalFormula a (Dis s1 s2) = (evalFormula a s1) || (evalFormula a s2)
evalFormula a (Imp s1 s2) = (not s1Eval) || s2Eval
  where
    s1Eval = evalFormula a s1
    s2Eval = evalFormula a s2
evalFormula a (Bic s1 s2) = (s1Eval && s2Eval) || ((not s1Eval) && (not s2Eval))
  where
    s1Eval = evalFormula a s1
    s2Eval = evalFormula a s2
evalFormula a constant = truthVal constant a
  
type TruthTable s = [TruthAssignment s]

containsFormula :: (Ord s) => Formula s -> TruthTable s -> Bool
containsFormula s [] = False
containsFormula s tt = M.member s (head tt)

truthTable :: (Ord s, Show s) => [Formula s] -> TruthTable s
truthTable sents = Prelude.foldl addFormula [] sents

addFormula :: (Ord s, Show s) => TruthTable s -> Formula s -> TruthTable s
addFormula tt s = if (containsFormula s tt)
  then tt
  else addNewFormula tt s

addNewFormula :: (Ord s, Show s) => TruthTable s -> Formula s -> TruthTable s
addNewFormula [] c@(Val n) = [truthAssignment [n] [True], truthAssignment [n] [False]]
addNewFormula tt c@(Val n) = ttFalse ++ ttTrue
 where
   ttFalse = Prelude.map (\ta -> M.insert c False ta) tt
   ttTrue = Prelude.map (\ta -> M.insert c True ta) tt
addNewFormula tt s = Prelude.map (addCompoundFormula s) tt

addCompoundFormula ::(Ord s, Show s) => Formula s -> TruthAssignment s -> TruthAssignment s
addCompoundFormula s ta = insert s sval ta
  where
    sval = evalFormula ta s

truthTableForFormula :: (Ord s, Show s) => Formula s -> TruthTable s
truthTableForFormula s = truthTable $ (constants s) ++ [s]
                          
isValidByTruthTable :: (Ord s, Show s) => Formula s -> Bool
isValidByTruthTable s = Prelude.and sTruthVals
  where
    sTruthTable = truthTableForFormula s
    sTruthVals = Prelude.map (truthVal s) sTruthTable
    
-- Format conversion functions
toCNF :: (Ord s, Show s) => Formula s -> CNF s
toCNF = cnf .
        cnfClauses .
        distributeDisjunction .
        pushNegation .
        removeImplication .
        removeBiconditional

cnfClauses :: (Ord s, Show s) => Formula s -> [Clause s]
cnfClauses (Con s1 s2) = cnfClauses s1 ++ cnfClauses s2
cnfClauses s = [disjunctiveClause s]

disjunctiveClause :: (Ord s, Show s) => Formula s -> Clause s
disjunctiveClause (Dis s1 s2) = concatClause (disjunctiveClause s1) (disjunctiveClause s2)
disjunctiveClause (Val name) = clause [lit name]
disjunctiveClause (Neg (Val name)) = clause [nLit name]
disjunctiveClause s = error $ "Disjunctive clause contains " ++ show s

removeImplication :: Formula s -> Formula s
removeImplication (Neg s) = Neg $ removeImplication s
removeImplication (Con s1 s2) = Con (removeImplication s1) (removeImplication s2)
removeImplication (Dis s1 s2) = Dis (removeImplication s1) (removeImplication s2)
removeImplication (Imp s1 s2) = Dis (Neg p) q
  where
    p = removeImplication s1
    q = removeImplication s2
removeImplication s = s

removeBiconditional :: Formula s -> Formula s
removeBiconditional (Neg s) = Neg $ removeBiconditional s
removeBiconditional (Con s1 s2) = Con (removeBiconditional s1) (removeBiconditional s2)
removeBiconditional (Dis s1 s2) = Dis (removeBiconditional s1) (removeBiconditional s2)
removeBiconditional (Imp s1 s2) = Imp (removeBiconditional s1) (removeBiconditional s2)
removeBiconditional (Bic s1 s2) = Con noBic1 noBic2
  where
    noBic1 = Imp p q
    noBic2 = Imp q p
    p = removeBiconditional s1
    q = removeBiconditional s2
removeBiconditional (Val name) = (Val name)

pushNegation :: Formula s -> Formula s
pushNegation (Neg (Neg s)) = pushNegation s
pushNegation (Neg (Con s1 s2)) = Dis (pushNegation (Neg s1)) (pushNegation (Neg s2))
pushNegation (Neg (Dis s1 s2)) = Con (pushNegation (Neg s1)) (pushNegation (Neg s2))
pushNegation (Con s1 s2) = Con (pushNegation s1) (pushNegation s2)
pushNegation (Dis s1 s2) = Dis (pushNegation s1) (pushNegation s2)
pushNegation s = s

distributeDisjunction :: Formula s -> Formula s
distributeDisjunction (Con p q) = Con (distributeDisjunction p) (distributeDisjunction q)
distributeDisjunction (Dis p (Con q r)) = Con pdq pdr
 where
   pdq = distributeDisjunction $ Dis p q
   pdr = distributeDisjunction $ Dis p r
distributeDisjunction (Dis (Con p q) r) = Con pdr qdr
 where
   pdr = distributeDisjunction $ Dis p r
   qdr = distributeDisjunction $ Dis q r
distributeDisjunction (Dis p q) = case pd of
  (Con r s) -> distributeDisjunction (Dis pd qd)
  other -> case qd of
    (Con r s) -> distributeDisjunction (Dis pd qd)
    other -> Dis pd qd
  where
    pd = distributeDisjunction p
    qd = distributeDisjunction q
distributeDisjunction s = s

-- Theorem code
data Theorem s = Thm [Formula s] (Formula s)
               deriving (Eq)
                        
instance Show s => Show (Theorem s) where
  show = showThm
  
showThm :: (Show s) => Theorem s -> String
showThm (Thm axioms hyp) = "THEOREM\n" ++ axiomStr ++ "\n|=\n\n" ++ hypString
  where
    axiomStr = Prelude.concat $ Prelude.map (\a -> (show a) ++ "\n") axioms
    hypString = show hyp

theorem :: [Formula s] -> Formula s -> Theorem s
theorem axioms hypothesis = Thm axioms hypothesis

checkTheorem :: (Ord s, Show s) => Theorem s -> Bool
checkTheorem (Thm axioms hypothesis) = case naiveSAT cnfFormNegThm of
  Just asg -> False
  Nothing -> True
  where
    cnfAxioms = Prelude.map toCNF axioms
    cnfNotHypothesis = toCNF (neg hypothesis)
    cnfFormNegThm = mergeCNFFormulas (cnfNotHypothesis:cnfAxioms)
    
-- BDD conversion code
    
bddCheckTaut :: (Ord s) => Formula s -> Bool
bddCheckTaut sent = isTaut (toBDD sent)

toBDD :: (Ord s) => Formula s -> BDD s
toBDD (Val n) = singletonBDD n
toBDD (Neg sent) = negBDD (toBDD sent)
toBDD (Dis f1 f2) = disBDD (toBDD f1) (toBDD f2)
toBDD (Con f1 f2) = conBDD (toBDD f1) (toBDD f2)
toBDD (Imp f1 f2) = impBDD (toBDD f1) (toBDD f2)
toBDD (Bic f1 f2) = bicBDD (toBDD f1) (toBDD f2)
