    module Function where
    import Nats
    import Prelude hiding (min, max, pred)

    double :: Nat -> Nat

    double n = times n n

    fact :: Nat -> Nat

    fact O = so
    fact (S n) = times (S n) (fact n)

    fib :: Nat -> Nat

    fib O = so
    fib (S O) = so
    fib (S (S n)) = plus (fib (S n)) (fib n)

    min :: (Nat,Nat) -> Nat

    min (_,O) = O
    min (O,_) = O
    min (S n,S m) = S (min (n, m))

    max :: (Nat,Nat) -> Nat

    max (O,n) = n
    max (n,O) = n
    max (S n, S m) = S (max (n, m))

    pred :: Nat -> Nat

    pred O = O
    pred (S n) = n


