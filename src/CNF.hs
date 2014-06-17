module CNF(
  lit, nLit,
  CNF, cnf,
  Clause, clause, concatClause)
       where

import Utils

data Atom =
  Lit Name |
  NLit Name
  deriving (Eq, Show)
  
lit name = Lit name
nLit name = NLit name

type Clause = [Atom]

concatClause :: Clause -> Clause -> Clause
concatClause c1 c2 = c1 ++ c2

clause :: [Atom] -> Clause
clause atoms = atoms

type CNF = [Clause]
  
cnf :: [Clause] -> CNF
cnf clauses = clauses