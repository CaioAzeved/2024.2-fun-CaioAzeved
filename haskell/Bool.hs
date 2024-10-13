    module Bool where       

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