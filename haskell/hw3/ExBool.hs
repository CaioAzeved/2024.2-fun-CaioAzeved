module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where

    show True = "True"
    show False = "False"

instance Enum Bool where

    toEnum 0 = True
    toEnum _ = False

    fromEnum True = 0
    fromEnum False = 1

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True 

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
True /|\ True = False
_ /|\ _ = True 

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
False \|/ False = True
_ \|/ _ = False 

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
True <=/=> True = False
False <=/=> False = False
_ <=/=> _ = True

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not False = True
not True = False

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ x = x

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
True ==> False = False
_ ==> _ = True

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
False <== True = False
_ <== _ = True

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
b <=> b' = not (b <=/=> b')

infixr 1 <=>