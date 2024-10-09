module Nat where

import Prelude hiding (Num(..))

data Nat where
    O :: Nat
    S :: Nat -> Nat
   --deriving (Eq, Show)

