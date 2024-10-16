    module PolyFunctions where
    import Prelude hiding((.))
    
    comp :: (b -> c) -> (a -> b) -> a -> c
    comp g f x = g (f x)

    (°) :: (b -> c) -> (a -> b) -> (a -> c)
    (°) = comp

    id :: a -> a
    id x = x