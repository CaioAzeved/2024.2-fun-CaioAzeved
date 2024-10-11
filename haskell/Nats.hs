    module Nats where       

    import Prelude hiding (Num(..), exp)

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

    exp :: Nat -> Nat -> Nat

    exp _ O = so
    exp n (S m) = times n (exp n m)

    double :: Nat -> Nat

    double n = times n n

    fact :: Nat -> Nat

    fact O = so
    fact (S n) = times (S n) (fact n)

    fib :: Nat -> Nat

    fib O = so
    fib (S O) = so
    fib (S (S n)) = plus (fib (S n)) (fib n)
