module Proper.Utils(
  Name,
  Error(..), extractValue) where

import Control.Applicative
import Control.Monad

type Name = String

data Error a =
  Succeeded a |
  Failed String
  deriving (Show)

instance Functor Error where
  fmap = liftM

instance Applicative Error where
  pure = return
  (<*>) = ap

instance Monad Error where
  return a = Succeeded a
  (Succeeded a) >>= f = f a
  (Failed errMsg) >>= f = (Failed errMsg)

extractValue :: Error a -> a
extractValue (Succeeded val) = val
extractValue (Failed errMsg) = error $ "Computation Failed: " ++ errMsg