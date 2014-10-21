module Proper.CNF(
  CNF, SatisfyingAssignment,
  cnf, mergeCNFFormulas,
  naiveSAT, naiveSATBool) where

import Data.Generics.Aliases
import Data.List as L
import Data.Map as M
import Data.Set as S
import Proper.Clause
import Proper.Utils

type SatisfyingAssignment a = Map (Atom a) Bool
type CNF c = Set (Clause c)
  
cnf :: (Ord c) => [Clause c] -> CNF c
cnf clauses = S.fromList clauses

mergeCNFFormulas :: (Ord c) => [CNF c] -> CNF c
mergeCNFFormulas formulas = S.foldl S.union S.empty (S.fromList formulas)

literals :: (Ord c) => CNF c -> Set (Atom c)
literals formula = S.foldl S.union S.empty (S.map (S.map literal) formula)

naiveSATBool :: (Ord c) => CNF c -> Bool
naiveSATBool formula = case naiveSAT formula of
  Just asg -> True
  Nothing -> False
  
-- A very naive implementation of DPLL
naiveSAT :: (Ord c) => CNF c -> Maybe (SatisfyingAssignment c)
naiveSAT formula = nSat formula allLits
  where
    allLits = literals formula
    
nSat :: (Ord c) => CNF c -> Set (Atom c) -> Maybe (SatisfyingAssignment c)
nSat formula lits = case S.member S.empty formula of
  True -> Nothing
  False -> case S.size formula of
    0 -> Just $ M.empty
    _ -> orElse (nSat nextFormula nextLits) (nSat nextFormulaNeg nextLits)
    where
      (simplifiedFormula, unitAssignments, nextLits) = simplifyUnitClauses formula lits
      nextLit = S.findMin nextLits
      nextFormula = S.insert (clause [nextLit]) simplifiedFormula
      nextFormulaNeg = S.insert (clause [negation nextLit]) simplifiedFormula

simplifyUnitClauses :: (Ord c) =>
                       CNF c ->
                       Set (Atom c) ->
                       (CNF c, SatisfyingAssignment c, Set (Atom c))
simplifyUnitClauses formula lits = (newFormula, unitAssignments, remainingLiterals)
  where
    unitClauses = S.filter (\s -> S.size s == 1) formula
    unitLiterals = literals unitClauses
    litList = S.toList unitLiterals
    unitAssignments = M.fromList $ L.zip litList $ L.map assignTruthVal litList
    newFormula = S.foldl removeUnitClause formula unitClauses
    remainingLiterals = S.difference lits unitLiterals
      {-
      nextLit = S.findMin lits
      nextLits = S.delete nextLit lits
      unitClause = clause [nextLit]
      unitNegClause = clause [negation nextLit]
      nextFormula = unitClauseSimplify (S.insert unitClause formula)
      nextFormulaNeg = unitClauseSimplify (S.insert unitNegClause formula)

unitClauseSimplify :: (Ord c) => CNF c -> CNF c
unitClauseSimplify formula = S.foldl removeUnitClause formula unitClauses
  where
    unitClauses = S.filter (\s -> S.size s == 1) formula-}

removeUnitClause :: (Ord c) => CNF c -> Clause c -> CNF c
removeUnitClause formula c = remainingClauses
  where
    elemC = S.findMin c
    removeNegC = S.map (S.delete (negation elemC)) formula
    remainingClauses = S.filter (\s -> (not $ S.member elemC s)) removeNegC
