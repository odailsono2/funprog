module Funtores where


data Tree a = L a
            | No a (Tree a) (Tree a)
            deriving (Show, Eq)

instance Functor Tree where
    fmap f (L a)      = L (f  a)
    fmap f (No x y z) = No (f x) ( fmap f y ) (fmap f z)
