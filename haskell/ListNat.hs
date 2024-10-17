    module ListNat where

    import Prelude (Eq, Show)
    import Nat
    import Bool
    import Function

    data ListNat where
        Nil :: ListNat
        Cons :: Nat -> ListNat -> ListNat
         deriving (Eq, Show)

    lenght :: ListNat -> Nat

    lenght Nil = O
    lenght (_ `Cons` xs) = S (lenght xs)

    pw :: (Nat -> Nat -> Nat) -> ListNat -> ListNat -> ListNat
    pw _ Nil xs = xs
    pw _ xs Nil = xs
    pw f (Cons x xs) (Cons y ys) = Cons (f x y) (pw f xs ys) 
