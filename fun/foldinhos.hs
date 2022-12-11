 {-# LANGUAGE GADTs #-}
module Fld  where

data Nat where
  Z :: Nat
  S :: Nat->Nat
  deriving (Show, Eq)

data L a  where
    E:: L a
    L:: a->L a->L a
    deriving (Show, Eq)

data T k a where
    Lf :: a->T k a
    No :: k->T k a-> T k a-> T k a
    deriving (Show, Eq)

data Tr where
    Lf2 :: Int->Tr
    No2 :: Tr->Tr->Tr
    deriving (Show, Eq)


data Mb a where
    Nt :: Mb a
    Jt :: a->Mb a
    deriving (Show, Eq)

lenLf2::Tr->Int
lenLf2 (Lf2 _ ) = 1
lenLf2 (No2 x y) = (lenLf2 x) + (lenLf2 y)

lenNo2::Tr->Int
lenNo2 ( No2 (Lf2 _) (Lf2 _) ) = 1
lenNo2 (No2 x y)               = 1 + lenNo2 x + lenNo2 y
  -- addition
  --
(<+>) :: Nat -> Nat -> Nat
x <+>   Z   =  x
x <+>  (S y) = (S x ) <+> y

   -- {--
instance Functor (T a) where
    fmap f (Lf a)     = Lf (f a)
    fmap f (No k l r) = No k (fmap f l) (fmap f r)

(<$)::Functor f => b -> f a -> f b
a <$ b = (fmap.const) a b

($>)::Functor f => f a -> b -> f b
a $> b = (fmap.const) b a

--}
   -- {--
instance Functor Mb  where
    fmap f Nt     = Nt
    fmap f (Jt x) = Jt (f x)

--}
instance Functor L where
    fmap f  E  = E
    fmap f  (L x y)  = L (f x) (fmap f y)

instance Applicative Mb where
    pure = Jt
    Nt  <*> _ = Nt
    (Jt f) <*> x = fmap f x

instance Applicative L where
    pure x= L x E
    L f fs  <*> L x xs = L (f x) (fs <*> xs)


-- define the following folds:
--
foldrN:: (Nat->b->b)->b->Nat->b
foldrN f b Z     = b
foldrN f b (S n) = f n (foldrN f b n)

foldrN':: b->(b->b)->Nat->b
foldrN' b f Z     = b
foldrN' b f (S n) = f $ (foldrN' b f n)

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr' :: (a->b->b)->b->[a]->b
foldr' f v []     = v
foldr' f v (x:xs) = x `f` (foldr f v xs)

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl' :: (b->a->b)->b->[a]->b
foldl' f v []     = v
foldl' f v (x:xs) = foldl f ( v `f` x) xs
-- foldl (+) ( 7 + 1) [2,3]
-- foldl (+) ( ( 7 + 1) + 2) [3]
-- foldl (+) ( ( ( 7 + 1) + 2) + 3) [] =
--           ( ( ( 7 + 1) + 2) + 3) = 13


-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1' :: (a->a->a)->[a]->a
foldr1' f [x] = x
foldr1' f (x:xs) = (x `f` (foldl1 f xs))

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1' :: (a->a->a)->[a]->a
foldl1' f (x:xs) =  foldl f x xs

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
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f v xs= foldr g b xs
    where
        b = [v]
        --g::a->[b]->[b]
        g = (\ x wts@(w:ws) -> (x `f` w):wts)


