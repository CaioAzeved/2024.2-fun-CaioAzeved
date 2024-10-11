    module Nat where       

    import Prelude hiding (Num(..))
    import Prelude hiding (Float(..))

    data Nat where
        O :: Nat
        S :: Nat -> Nat
         deriving (Eq, Show)

    --useful sugars

    o, so, sso, ssso, sssso, ssssso, sssssso, ssssssso, sssssssso, ssssssssso :: Nat
    o    = O
    so   = S o
    sso  = S so
    ssso = S sso
    sssso = S ssso
    ssssso = S sssso
    sssssso = S ssssso
    ssssssso = S sssssso
    sssssssso = S ssssssso
    ssssssssso = S sssssssso

    --operations

    plus :: Nat -> Nat -> Nat

    plus n O = n
    plus n (S m) = S (plus n m)

    times :: Nat -> Nat -> Nat

    times _ O = O
    times n (S m) = plus n (times n m)

    expo :: Nat -> Nat -> Nat

    expo _ O = so
    expo n (S m) = times n (expo n m)

    double :: Nat -> Nat

    double n = times n n