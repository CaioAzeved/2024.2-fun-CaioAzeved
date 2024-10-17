    module List where
    import Nat 
    --import ListNat hiding (lenght)

    data List a where
        Nil :: List a
        Cons :: a -> List a -> List a
        deriving(Eq, Show)
    
    lenght :: List a -> Nat
    lenght Nil = O
    lenght (Cons x xs) = S (lenght xs)