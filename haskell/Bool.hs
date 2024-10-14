    module Bool where       
    
    import Nat
    import Prelude hiding (Bool, True, False)

    data Bool where
        True :: Bool
        False :: Bool
         deriving (Eq, Show)

    band :: Bool -> Bool -> Bool

    band True True = True
    band _ _ = False

    bor :: Bool -> Bool -> Bool

    bor False False = False
    bor _ _ = True 

    bnot :: Bool -> Bool

    bnot True = False
    bnot _ = True

    if_then_else :: Bool -> a -> a -> a

    if_then_else True x _ = x
    if_then_else False _ x = x

    bxor :: Bool -> Bool -> Bool

    bxor True True = True
    bxor False False = True
    bxor _ _ = False

    bnand :: Bool -> Bool -> Bool

    bnand x y = comp bnot  (band x) y