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

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char

-- You MUST NOT use ANY of these in your code
--data L a = E | C a (L a)
--           deriving (P.Show, Eq)

head :: [a] -> a
head []     = error "Lista Vazia"
head (x:xs) = x

tail :: [a] -> [a]
tail []     = error "Lista Vazia"
tail (x:xs) = xs

null :: (Eq a) => [a] -> Bool
null xs
  | xs == []  = True
  | otherwise = False

length :: Integral i => [a] -> i
length [] = 0
length (x:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
xs ++ [] = xs
[] ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x ys = ys ++ [x]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum []  = error "Lista Vazia"
minimum [x] = x
minimum (x:y:xs)
  | x < y = minimum (x:xs)
  | otherwise = minimum (y:xs)

maximum :: Ord a => [a] -> a
maximum []  = error "Lista Vazia"
maximum [x] = x
maximum (x:y:xs)
  | x > y     = maximum (x:xs)
  | otherwise = maximum (y:xs)

take :: (Eq a, Num a)=>a->[b]->[b]
take _ []     = []
take n (y:ys)
  | n == 0    = []
  | otherwise = y : take (n-1) ys

drop :: (Eq a, Num a)=> a->[b]->[b]
drop _ []        = []
drop n (y:ys)
  | n == 0       =  y:ys
  | otherwise    =  drop (n-1) ys

takeWhile :: (a->Bool)->[a]->[a]
takeWhile f (y:ys)
   | f y == True    = y : takeWhile f ys
   | otherwise      = []
-- dropWhile

tails:: [a]->[[a]]
tails []     = [] : []
tails (x:xs) = (x : xs) : (tails xs)

inits :: [a]->[[a]]
inits []      = []
inits xs  =  reverse (tails xs)

--[1,2,3]
--[[],[2],[3],[2,3],[4],[2,4],[3,4],[2,3,4]]
--subsequences :: [a]->[[a]]
--subsequences [] = []
--subsequences all@(x:xs) = : subsequences xs
--    where
--        aux :: [a] -> [a]
--        aux [] = []
--        aux [ys] = [ys]

foldr::(a->b->b)->b->[a]->b
foldr f v []     = v
foldr f v (x:xs) = x `f` foldr f v xs



any:: (a->Bool) -> [a] -> Bool
--any _ []     = False
any p (x:xs) = foldr f b xs
    where
        b = False
        f = \ x w -> p x || w


--[1,2,3]
--(>3)
--1:2:3:[]
--1 > 3 ( 2 > 3 (b > 3)


-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

filter :: (a->Bool) -> [a] -> [a]
filter _ []   = []
filter p (x:xs)
    | p x       =  x:filter p xs
    | otherwise =  filter p xs

map :: (a->b)->[a]->[b]
map f (x:xs) = foldr g b xs
    where
        b = []
        g = \ x w -> f x : w

{--
map _ []     = []
map f (x:xs) = f x : map f xs
--}

-- cycle
repeat :: a->[a]
repeat x = x:repeat x

--foldr::(a->b->b)->b->[a]->b
replicate:: Int->a->[a]
replicate n x = foldr f b [x | k <- [1..n]]
        where
            b = []
            f = \ x w -> x : w
{--
replicate 0 _ = []
replicate n x = x:replicate (n-1) x
--}
-- isPrefixOf
-- isInfixOf
-- isSuffixOf

zip :: [a]->[b]->[(a,b)]
zip _ []          = []
zip [] _          = []
zip (x:xs) (y:ys) = (x,y):zip xs ys

zipWith :: (a->b->c)->[a]->[b]->[c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

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
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

