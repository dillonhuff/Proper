module Proper.BDD(
  BDD, trueBDD, falseBDD, singletonBDD, negBDD,
  disBDD, conBDD, impBDD, bicBDD,
  isTaut) where

import Data.Map as M
import Data.List as L
import Data.Tuple as T

data BDD p =
  TrueNode            |
  FalseNode           |
  N p (BDD p) (BDD p)
  deriving (Eq, Show)
  
node :: (Eq p) => p -> BDD p -> BDD p -> BDD p
node v l r = case l == r of
  True -> l
  False -> N v l r
  
trueBDD = TrueNode
falseBDD = FalseNode
singletonBDD v = N v TrueNode FalseNode

negBDD :: (Eq p) => BDD p -> BDD p
negBDD TrueNode = FalseNode
negBDD FalseNode = TrueNode
negBDD (N p l r) = node p (negBDD l) (negBDD r)

disBDD :: (Ord p) => BDD p -> BDD p -> BDD p
disBDD TrueNode _ = TrueNode
disBDD _ TrueNode = TrueNode
disBDD FalseNode n = n
disBDD n FalseNode = n
disBDD l@(N p ll lr) r@(N q rl rr) = case compare p q of
  LT -> node q (disBDD l rl) (disBDD l rr)
  GT -> node p (disBDD ll r) (disBDD lr r)
  EQ -> node p (disBDD ll rl) (disBDD lr rr)
  
conBDD :: (Ord p) => BDD p -> BDD p -> BDD p
conBDD l r = negBDD (disBDD (negBDD l) (negBDD r))

impBDD :: (Ord p) => BDD p -> BDD p -> BDD p
impBDD l r = (disBDD (negBDD l) r)

bicBDD :: (Ord p) => BDD p -> BDD p -> BDD p
bicBDD l r = (conBDD (impBDD l r) (impBDD r l))

isTaut TrueNode = True
isTaut _ = False
{-
data BDD p = BDD (Map (BDDNode p) Int) (Map Int (BDDNode p)) Int
             deriving (Eq, Show)
                      
data BDDNode p =
  TrueNode  |
  FalseNode |
  N p Int Int
  deriving (Eq, Ord, Show)
           
negateNode :: BDDNode p -> BDDNode p
negateNode (N p l r) = N p r l
negateNode n = n

root :: (Ord p) => BDD p -> BDDNode
root (BDD nToInt _ _) = fst $ M.findMax nToInd

leftChild :: (Ord p) => BDD p -> BDD p
leftChild bdd@(BDD nToInt intToN n) = deleteTreeFrom (rightNode r) (deleteNode r bdd)
 where
   r = root bdd
   
deleteTreeFrom :: (Ord p) => BDDNode p -> BDD p -> BDD p
deleteTreeFrom (N p l r) bdd = deleteTreeFrom 
   
deleteNode :: (Ord p) => BDDNode p -> BDD p -> BDD p
deleteNode n bdd@(BDD nToInt intToN m) = BDD (M.delete n nToInt) (M.delete nInd intToN) m
  where
    nInd = case M.lookup n nToInt of
      Just ind -> ind
      Nothing -> m+1

trueBDD :: (Ord p) => BDD p
trueBDD = BDD (M.fromList [(TrueNode, 1)]) (M.fromList [(1, TrueNode)]) 2

falseBDD :: (Ord p) => BDD p
falseBDD = BDD (M.fromList [(FalseNode, 0)]) (M.fromList [(0, FalseNode)]) 2

singletonBDD :: (Ord p) => p -> BDD p
singletonBDD val = BDD (M.fromList sl) (M.fromList (L.map swap sl)) 3
  where
    sl = [(TrueNode, 1), (FalseNode, 0), (N val 1 0, 2)]
    
negBDD :: (Ord p) => BDD p -> BDD p
negBDD (BDD nToInt intToN n) = BDD negNToInt negIntToN n
  where
    negNToInt = mapKeys negateNode nToInt
    negIntToN = M.fromList $  L.map swap $ M.toList negNToInt
    
disBDD :: (Ord p) => BDD p -> BDD p -> BDD p
disBDD left right = case left == trueBDD || right == trueBDD of
  True -> trueBDD
  False -> case left == falseBDD && right == falseBDD of
    True -> falseBDD
    False -> disMerge left right
    
disMerge :: (Ord p) => BDD p -> BDD p -> BDD p
disMerge left right = case leftRoot == rightRoot of
  True -> makeBDD leftRoot (disBDD lrlc rrlc) (disBDD lrrc rrrc)
  False -> case leftRoot > rightRoot of
    True -> makeBDD leftRoot (disBDD lrlc rightRoot) (disBDD lrrc rightRoot)
    False -> makeBDD rightRoot (disBDD leftRoot rrlc) (disBDD leftRoot rrrc)
  where
    leftRoot = root left
    rightRoot = root right
    lrlc = leftChild leftRoot
    lrrc = rightChild leftRoot
    rrlc = leftChild rightRoot
    rrrc = rightChild rightRoot

isTaut :: (Ord p) => BDD p -> Bool
isTaut b = b == trueBDD

newBDD :: (Ord p) => BDD p
newBDD = BDD M.empty M.empty 2

addNode :: (Ord p) => BDD p -> BDDNode p -> (BDD p, Int)
addNode bdd@(BDD nToInt intToN n) newNode = case M.lookup newNode nToInt of
  Just nodeInd -> (bdd, nodeInd)
  Nothing -> (BDD (M.insert newNode (n+1) nToInt) (M.insert (n+1) newNode intToN) (n+1), n)
-}