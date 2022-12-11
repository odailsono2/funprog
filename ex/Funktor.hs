module Funktor where

import Prelude hiding ( fmap , (<$) )

data Pair a = Pair a a
    deriving (Show, Eq)

data Tree a = L a
            | N a (Tree a) (Tree a)
    deriving (Show, Eq)

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const


instance Funktor [] where
    fmap = map

instance Funktor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

-- what about Either?
instance Funktor (Either a) where
    --fmap f  = Nothing
    fmap f (Left x)  = Left x
    fmap f (Right x) = Right (f x)

-- what about pairs?
instance Funktor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

-- what about functions?
instance Funktor ((->)r) where
    fmap f g= \ x -> f (g (x))

-- what about Trees?
instance Funktor Tree where
    fmap f (L r) = L (f r)
    fmap f (N a b c ) = N (f a) (fmap f b) (fmap f c)

-- ...define Functor instances of as many * -> * things as you can think of!

