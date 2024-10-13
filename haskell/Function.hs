    module Function where
    import Nat
    import Prelude hiding (min, max, pred, quot, rem, div)

    double :: Nat -> Nat
    double n = times n n

    fact :: Nat -> Nat
    fact O = so
    fact (S n) = times (S n) (fact n)

    fib :: Nat -> Nat
    fib O = O
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

    quot :: (Nat, Nat) -> Nat
    quot (_,O) = error"Indeterminado"
    quot (n, m) = 
        if max(n,m) == n
        then S (quot (sub n m, m))
        else O

    rem :: (Nat, Nat) -> Nat
    rem (_,O) = error"Indeterminado"
    rem (n,m) = 
        if max(n,m) == n
        then rem (sub n m, m)
        else n

    div :: (Nat, Nat) -> (Nat,Nat)
    div (n, m) = (quot(n,m),rem(n,m))
