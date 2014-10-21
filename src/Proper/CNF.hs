module Proper.CNF(
  CNF, SatisfyingAssignment,
  cnf, mergeCNFFormulas,
  naiveSAT, naiveSATBool) where

import Data.Generics.Aliases
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
naiveSAT formula = nSat simplifiedFormula allLits
  where
    simplifiedFormula = unitClauseSimplify formula
    allLits = literals simplifiedFormula
    
nSat :: (Ord c) => CNF c -> Set (Atom c) -> Maybe (SatisfyingAssignment c)
nSat formula lits = case S.member S.empty formula of
  True -> Nothing
  False -> case S.size formula of
    0 -> Just $ M.empty
    _ -> orElse (nSat nextFormula nextLits) (nSat nextFormulaNeg nextLits)
    where
      (simplifiedFormula, unitAssignments, newLits) = simplifyUnitClauses formula lits
      nextLit = S.findMin newLits
      nextFormula = S.insert (clause [nextLit]) simplifiedFormula
      nextFormulaNeg = S.insert (clause [negation nextLit]) simplifiedFormula

simplifyUnitClauses :: CNF c ->
                       Set (Atom c) ->
                       (CNF c, SimplifyingAssignment c, Set (Atom c))
simplifyUnitClauses formula lits = 
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
    unitClauses = S.filter (\s -> S.size s == 1) formula

removeUnitClause :: (Ord c) => CNF c -> Clause c -> CNF c
removeUnitClause formula c = remainingClauses
  where
    elemC = S.findMin c
    removeNegC = S.map (S.delete (negation elemC)) formula
    remainingClauses = S.filter (\s -> (not $ S.member elemC s)) removeNegC
-}
