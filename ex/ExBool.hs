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
    show True  = "Verdadeiro"
    show False = "Falso"

instance Enum Bool where

    toEnum 0 = False
    toEnum 1 = True
    toEnum _ = error "extrapolou meu amigo, so tem 0 ou 1"
    fromEnum  False = 0
    fromEnum  True = 1


-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ False   = False
(&&) False _   = False


infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
(||) True _ = True
(||) _ True = True
(||) _ _ = False



infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) x y = not $ (&&) x y

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) x y = not $ (||) x y

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) False False = False
(<=/=>) True  True  = False
(<=/=>) _    _      = True


infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x y = x
ifThenElse False x y = y


-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) True True   = True
(==>) False False = True
(==>) _      _    = False



infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) True True   = True
(<==) False False = True
(<==) _      _    = False

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) x y = (x <== y) && (x ==> y)

infixr 1 <=>


