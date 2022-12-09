module Origami where

import Prelude hiding
    ( foldl , foldl1 , foldr , foldr1
    , sum , product
    , length
    , concat
    , filter
    , map
    , any , all
    , and , or
    , takeWhile , dropWhile
    , scanl, scanr
    , minimum, maximum
    , reverse
    )

import qualified Prelude as P

--
-- define the following folds:
--

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr :: (a->b->b)->b->[a]->b
foldr f v []     = v
foldr f v (x:xs) = x `f` (foldr f v xs)

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (b->a->b)->b->[a]->b
foldl f v []     = v
foldl f v (x:xs) = foldl f ( v `f` x) xs
-- foldl (+) ( 7 + 1) [2,3]
-- foldl (+) ( ( 7 + 1) + 2) [3]
-- foldl (+) ( ( ( 7 + 1) + 2) + 3) [] =
--           ( ( ( 7 + 1) + 2) + 3) = 13


-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a->a->a)->[a]->a
foldr1 f [x] = x
foldr1 f (x:xs) = (x `f` (foldl1 f xs))

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a->a->a)->[a]->a
foldl1 f (x:xs) =  foldl f x xs

-- define the following scans:
-- (scans are like folds but return all intermediate calculations)
--
-- foldl (+) 0 [12,25,16,24] = ((((0 + 12) + 25) + 16) + 24)
-- scanl (+) 0 [12,25,16,24] = [   0 , 12  , 37  , 53  , 77]
{-
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f v xs= foldl g b xs
    where
        b = [v]
        g = \ ys (w:ws) ->  ys ++ [f (last ys) w]
--}
-- foldr (+) 0 [12,25,16,24] = (12 + (25 + (16 + (24 + 0))))
-- scanr (+) 0 [12,25,16,24] = [77 ,  65 ,  40 ,  24 , 0   ]
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f v xs= foldr g b xs
    where
        b = [v]
        --g::(a->)a->[b]->[b]
        g = (\ x (w:ws) -> (x `f` w):w:ws)

--
-- Define all of the following functions as folds:
--

sum :: Num a => [a] -> a
sum xs = let b = 0 in foldr (+) b xs

product :: Num a => [a] -> a
product xs = foldr (*) 1 xs

concat :: [[a]] -> [a]
concat xss = foldr f b xss
    where
        b = []
        f = \ ws -> (++) ws

any :: (a -> Bool) -> [a] -> Bool
any p xs = foldr f b xs
    where
        b = False
        f = \ x y -> (p x) || y

all :: (a -> Bool) -> [a] -> Bool
all p xs = foldr f b xs
    where
        b = True
        f = \ x y -> (p x) && y

and :: [Bool] -> Bool
and xs = foldr (&&) True xs

or :: [Bool] -> Bool
or xs = foldr (||) False xs

minimum :: Ord a => [a] -> a
minimum xs = foldr1 f xs
    where
        f = \ x y -> min x  y

maximum :: Ord a => [a] -> a
maximum xs = foldr1 f xs
    where
        f = \ x y -> max x  y


length :: Integral i => [a] -> i
length xs = foldr f b xs
    where
        b = 0
        f = \ x -> (+)1

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = foldr f b xs
    where
        b = []
        f = \ y xs ->
            if p y then y:xs
                   else xs

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x :  map f xs

reverse :: [a] -> [a]
reverse xs = foldl f b xs
    where
        b = []
        f = \ ys x -> x:ys

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs = foldr f b xs
    where
        b = []
        f = \ x xs ->
            if p x then x:xs
                   else xs

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs = foldr f b xs
    where
        b = []
        f = \ x xs ->
            if p x then xs
                   else x:xs


-- sum of evens, safeMaximum of odds
-- e.g.:
-- semo [1..10] = (30, Just 9)
-- semo [2,4,6] = (12, Nothing)
    {-
semo :: Integral i => [i] -> (i, Maybe i)
semo xs = foldr f b xs
    where
        b = (0,Nothing)
        f = \ x xs ->
            if even x then ((+)1, case x of )
-}
-- removes adjacent duplicates
-- e.g.:
-- remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]
remdups :: Eq a => [a] -> [a]
remdups = undefined

safeLast :: [a] -> Maybe a
safeLast = undefined

-- dec2int [1,9,9,2] = 1992
dec2int :: Integral i => [i] -> i
dec2int = undefined

