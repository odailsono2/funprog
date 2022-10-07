module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
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

data Nat = Zero | Succ Nat

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show  Zero                  = "O"
  --  show  (Succ Zero)           = "S" ++ show Zero
  --  show  (Succ Succ Zero)      = "S" ++ "S" ++ show Zero
  --  show  (Succ Succ Succ Zero) = "S" ++ show (Succ (Succ Zero))
    show  (Succ x)              = "S" ++ show  x

instance Eq Nat where

    (==) Zero        Zero        = True
    (==) (Succ Zero) (Succ Zero) = True
    (==) (Succ x ) (Succ y) =  (==) x y
    (==) _ _  = False



instance Ord Nat where
    (<) Zero Zero   = False
    (<) Zero  _     = True
    (<)  (Succ x) y = (<) x y


    (<=) Zero Zero   = True
    (<=) Zero _      = True
    (<=) (Succ x)  y = (<) x y


    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.




    --min (Succ x) y = min x y
    min x y
      | x <-> y == Zero = x
      | otherwise = y

   -- min Zero      y = y


    max x y
      | min x y == x = y
      | otherwise = x

isZero :: Nat -> Bool
isZero x
  | x == Zero = True
  | otherwise = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred x
  |  x == Zero = Zero
  |  otherwise = x <-> (Succ Zero)


even :: Nat -> Bool
even = undefined

odd :: Nat -> Bool
odd = undefined

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>)   x    Zero   =  x
(<+>)   x  (Succ y) = (<+>) (Succ x ) y

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) Zero      y       = Zero
(<->) x       Zero      = x
(<->) (Succ x) (Succ y) = (<->) x y

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) x   (Succ Zero) = x
(<*>)  x   (Succ y) = x <+> (x <*> y)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) = undefined

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) = undefined

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) = absDiff

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = undefined
        | x == 0    = undefined
        | otherwise = undefined

