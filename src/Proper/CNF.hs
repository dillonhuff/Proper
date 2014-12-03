module Proper.CNF(
  CNF, SatisfyingAssignment,
  cnf, mergeCNFFormulas, addClause,
  naiveSAT, naiveSATBool) where

import Control.Monad
import Data.Generics.Aliases
import Data.List as L
import Data.Map as M
import Data.Set as S
import Proper.Clause
import Proper.Utils

type SatisfyingAssignment l = Map l Bool
type CNF c = Set (Clause c)
  
cnf :: (Ord c) => [Clause c] -> CNF c
cnf clauses = S.fromList clauses

addClause :: (Ord c) => Clause c -> CNF c -> CNF c
addClause toAdd cnfFormula = S.insert toAdd cnfFormula

mergeCNFFormulas :: (Ord c) => [CNF c] -> CNF c
mergeCNFFormulas formulas = S.foldl S.union S.empty (S.fromList formulas)

literals :: (Ord c) => CNF c -> Set c
literals formula = S.map atom $ S.foldl S.union S.empty (S.map (S.map literal) formula)

naiveSATBool :: (Ord c) => CNF c -> Bool
naiveSATBool formula = case naiveSAT formula of
  Just asg -> True
  Nothing -> False
  
-- A very naive implementation of DPLL
naiveSAT :: (Ord c) => CNF c -> Maybe (SatisfyingAssignment c)
naiveSAT formula = nSat formula allLits
  where
    allLits = literals formula
    
nSat :: (Ord c) => CNF c -> Set c -> Maybe (SatisfyingAssignment c)
nSat formula lits = case S.member S.empty formula of
  True -> Nothing
  False -> case S.size formula of
    0 -> Just M.empty
    _ -> case S.size unitClauses of
      0 -> pickLitAndSplit formula lits
      _ -> unitSimplify unitClause formula lits
      where
        unitClauses = S.filter (\c -> S.size c == 1) formula
        unitClause = S.findMin unitClauses

pickLitAndSplit :: (Ord c) => CNF c -> Set c -> Maybe (SatisfyingAssignment c)
pickLitAndSplit formula lits = orElse trueAsg falseAsg
  where
    nextLit = S.findMin lits
    nextLits = S.delete nextLit lits
    trueRes = nSat (S.insert (clause [lit nextLit]) formula) nextLits
    falseRes = nSat (S.insert (clause [negation $ lit nextLit]) formula) nextLits
    trueAsg = liftM (M.insert nextLit True) trueRes
    falseAsg = liftM (M.insert nextLit False) falseRes

unitSimplify :: (Ord c) =>
                Clause c ->
                CNF c ->
                Set c ->
                Maybe (SatisfyingAssignment c)
unitSimplify unitClause formula lits = satResWithUnitAsg
  where
    simplifiedFormula = removeUnitClause formula unitClause
    litToSimplify = S.findMin unitClause
    tVal = assignTruthVal litToSimplify
    satRes = nSat simplifiedFormula lits
    satResWithUnitAsg = liftM (M.insert (atom litToSimplify) tVal) satRes
    
removeUnitClause :: (Ord c) => CNF c -> Clause c -> CNF c
removeUnitClause formula c = remainingClauses
  where
    elemC = S.findMin c
    removeNegC = S.map (S.delete (negation elemC)) formula
    remainingClauses = S.filter (\s -> (not $ S.member elemC s)) removeNegC
