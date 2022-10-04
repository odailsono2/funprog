module MyBool where

data Boolean = T
             | F
             deriving (Show, Eq)

lnot :: Boolean -> Boolean
lnot F = T
lnot _ = F

land :: Boolean -> Boolean -> Boolean
land T T = T
land _ _ = F

lor :: Boolean -> Boolean -> Boolean
lor _ T = T
lor T _ = T
lor _ _ = F

-- operador ternário, recebe um valor Boolean e as respostas caso verdadeiro e uma caso falso, o retorno precisa ser do mesmo tipo
ifthenelse :: Boolean -> a -> a -> a
ifthenelse T x _ = x
ifthenelse _ _ y = y
