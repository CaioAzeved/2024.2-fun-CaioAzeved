    module List where
    import Prelude hiding ((++))
    import Nat 
    --import ListNat hiding (lenght)

    data List a where
        Nil :: List a
        Cons :: a -> List a -> List a
        deriving(Eq, Show)
    
    lenght :: List a -> Nat
    lenght Nil = O
    lenght (Cons x xs) = S (lenght xs)

    (++) :: List a -> List a -> List a
    Nil ++ xs = xs
    (Cons x xs) ++ ys = Cons x (xs ++ ys)