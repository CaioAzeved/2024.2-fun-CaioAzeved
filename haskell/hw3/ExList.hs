{-# LANGUAGE GADTs #-}

module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error"Não há cabeça no vazio" 
head (x : xs) = x

tail :: [a] -> [a]
tail [] = error"Não há cauda no vazio"
tail (x : xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
lenght (x : xs) = 1 + (lenght xs) 

sum :: Num a => [a] -> a
sum = fold 0 (+)

product :: Num a => [a] -> a
product = fold 1 (*)

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = (reverse xs) ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ xs = xs
(x : xs) ++ ys = x : (xs ++ ys) 

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc y (x : xs) = x : (snoc y xs) 

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm??)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error"Não há mínimo"
minimum [x] = x
minimum (x : xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [] = error"Não há máximo"
maximum [x] = x
maximum (x : xs) = max x (maximum xs)

take :: Int -> [a] -> [a]
take n (x : xs)
  | n <= 0 = []
  | otherwise = x : (take (n-1) xs)
take _ [] = []

drop :: Int -> [a] -> [a]
drop n (x : xs)
  | n <= 0 = x : xs
  | otherwise = drop (n-1) xs 
drop _ xs = xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x : xs) = 
  if f x
  then x : (takeWhile f xs)
  else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x : xs) = 
  if f x
  then dropWhile f xs
  else x : xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = [x : xs] ++ (tails xs)

init :: [a] -> [a]
init [] = error"Nada para pegar do início"
init [a] = []
init (x : xs) = x : (init xs)

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = (inits $ init $ x : xs) ++ [x : xs]

-- subsequences

toListBool :: (a -> Bool) -> [a] -> [Bool]
toListBool _ [] = []
toListBool p (x : xs) = 
  if p x
  then True : (toListBool p xs)
  else False : (toListBool p xs)

any :: (a -> Bool) -> [a] -> Bool
any p xs = or (toListBool p xs)

all :: (a -> Bool) -> [a] -> Bool
all p xs = and (toListBool p xs)

and :: [Bool] -> Bool
and = fold True (&&)

or :: [Bool] -> Bool
or = fold False (||)

concat :: [[a]] -> [a]
concat = fold [] (++)

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x xs = any (== x) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y : ys) = 
  if x == y
  then True
  else elem' x ys

(!!) :: [a] -> Int -> a
[] !! _ = error"Índice muito grande para esta lista"
(x:xs) !! n
  | n == 0 = x
  | n < 0 = error"Não há índice negativo"
  | n > 0 = xs !! (n-1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x: xs) = 
  if p x
  then x : (filter p xs)
  else filter p xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : (map f xs)

fold :: a -> (a -> a -> a) -> [a] -> a
fold i op [] = i
fold i op (x : xs) = op x (fold i op xs)

cycle :: [a] -> [a]
cycle [] = []
cycle (x:xs) = x:xs ++ cycle (x:xs)

repeat :: a -> [a]
repeat x = cycle [x]

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x =
  if n > 0
  then x:(replicate (n-1) x)
  else error"Tente um índice não negativo"

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
--palindrome :: String -> Bool
--palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."
-}