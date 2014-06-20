module Proper.Clause(
  Atom, negation, lit, nLit, literal,
  Clause, clause, concatClause) where

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