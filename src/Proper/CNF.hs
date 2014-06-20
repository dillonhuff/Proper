module Proper.CNF(
  lit, nLit,
  CNF, cnf, mergeCNFFormulas,
  Clause, clause, concatClause,
  naiveSAT)
       where

import Data.Map as M
import Data.Set as S

import Proper.Utils

data Atom =
  Lit Name |
  NLit Name
  deriving (Eq, Ord, Show)
  
negation :: Atom -> Atom
negation (Lit n) = NLit n
negation (NLit n) = Lit n

literal :: Atom -> Atom
literal (Lit n) = Lit n
literal (NLit n) = Lit n

lit name = Lit name
nLit name = NLit name

type Clause = Set Atom

concatClause :: Clause -> Clause -> Clause
concatClause c1 c2 = S.union c1 c2

clause :: [Atom] -> Clause
clause atoms = S.fromList atoms

type CNF = Set Clause
  
cnf :: [Clause] -> CNF
cnf clauses = S.fromList clauses

mergeCNFFormulas :: [CNF] -> CNF
mergeCNFFormulas formulas = S.foldl S.union S.empty (S.fromList formulas)

literals :: CNF -> Set Atom
literals formula = S.foldl S.union S.empty (S.map (S.map literal) formula)

naiveSAT :: CNF -> Bool
naiveSAT formula = nSat simplifiedFormula allLits
  where
    simplifiedFormula = unitClauseSimplify formula
    allLits = literals simplifiedFormula
    
nSat :: CNF -> Set Atom -> Bool
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

unitClauseSimplify :: CNF -> CNF
unitClauseSimplify formula = S.foldl removeUnitClause formula unitClauses
  where
    unitClauses = S.filter (\s -> S.size s == 1) formula

removeUnitClause :: CNF -> Clause -> CNF
removeUnitClause formula c = remainingClauses
  where
    elemC = S.findMin c
    removeNegC = S.map (S.delete (negation elemC)) formula
    remainingClauses = S.filter (\s -> (not $ S.member elemC s)) removeNegC