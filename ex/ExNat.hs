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

    (/=) x y = not (x == y)



instance Ord Nat where
    _        <    Zero   = False
    Zero     <    _      = True
    (Succ x) < (Succ y ) =  x < y

    Zero <= Zero  = True
    Zero <=  _    = True
    _    <=  Zero = False
    (Succ x) <= (Succ y) = x <= y

    (>) x y = not ((<=) x y)

    (>=) x y = not ((<) x y)


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
even x | x <%> (Succ(Succ Zero)) == Zero = True
       | otherwise =False

odd :: Nat -> Bool
odd x = not (even x)

-- addition
(<+>) :: Nat -> Nat -> Nat
x <+>   Zero   =  x
x <+>  (Succ y) = (Succ x ) <+> y

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
Zero <->    y      = Zero
x    <->   Zero    = x
(Succ x) <-> (Succ y) = (<->) x y

-- multiplication
(<*>) :: Nat -> Nat -> Nat
_ <*> Zero = Zero
x <*> (Succ Zero) = x
x <*>   (Succ y)   = x <+> (x <*> y)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
_ <^>    Zero       = Succ Zero
x <^>   (Succ y)    = x <*> ( x <^> y )

-- quotient
(</>) :: Nat -> Nat -> Nat
_  </>   Zero        =  error "N??o posso dividir por Zero"
x  </>   y  | x <  y = Zero
x  </>   y  = Succ  ( (x<->y) </> y  )


-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> Zero     = error "Resto de divis??o por zero n??o est?? definido"
x <%>  y       =  x <-> ( y <*> (x </> y ) )

-- divides
(<|>) :: Nat -> Nat -> Bool
x <|> y | (x <%> y) == Zero = True
        | otherwise      = False

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff x y
  | x >= y = x <-> y
  | otherwise = y <-> x

(|-|) = absDiff

factorial :: Nat -> Nat
factorial 0 = 1
factorial x = x<*>( factorial (x<->1))

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg x | x > Zero = Succ Zero
     | x == Zero = Zero
     | otherwise = Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo b a | b ==  0 = error "N??o posso calcular para b=0"
       | a ==  0 = 0
       | a == b  = 1
       | otherwise = aux <+> lo b (a </> b)
                      where aux
                              | b > a = 0
                              | otherwise = 1


--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat 0 = Zero
toNat x = Succ (toNat (x -1))

fromNat :: Integral a => Nat -> a
fromNat Zero = 0
fromNat x    =  1 + fromNat (x<->Succ Zero)


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = error "N??o definido para os negativos"
        | x == 0    = Zero
        | otherwise = Succ (fromInteger (x-1))

