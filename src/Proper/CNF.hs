module Proper.CNF(
  CNF, cnf, mergeCNFFormulas,
  naiveSAT)
       where

import Data.Map as M
import Data.Set as S
import Proper.Clause
import Proper.Utils

type CNF c = Set (Clause c)
  
cnf :: (Ord c) => [Clause c] -> CNF c
cnf clauses = S.fromList clauses

mergeCNFFormulas :: (Ord c) => [CNF c] -> CNF c
mergeCNFFormulas formulas = S.foldl S.union S.empty (S.fromList formulas)

literals :: (Ord c) => CNF c -> Set (Atom c)
literals formula = S.foldl S.union S.empty (S.map (S.map literal) formula)

naiveSAT :: (Ord c) => CNF c -> Bool
naiveSAT formula = nSat simplifiedFormula allLits
  where
    simplifiedFormula = unitClauseSimplify formula
    allLits = literals simplifiedFormula
    
nSat :: (Ord c) => CNF c -> Set (Atom c) -> Bool
nSat formula lits = case S.member S.empty formula of
  True -> False
  False -> case S.size formula of
    0 -> True
    _ -> (nSat nextFormula nextLits) || (nSat nextFormulaNeg nextLits)
    where
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