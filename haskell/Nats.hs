    module Nat where       

    import Prelude hiding (Num(..))

    data Nat where
        O :: Nat
        S :: Nat -> Nat
         deriving (Eq, Show)


    --operations

    plus :: Nat -> Nat -> Nat

    plus n O = n
    plus n (S m) = S (plus n m)

    times :: Nat -> Nat -> Nat

    times _ O = O
    times n (S m) = plus n (times n m)

