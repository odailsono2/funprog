module Tipos where
    {--
    typerclasse: Num, Fractional, INtegral;

    primitive types: Char, Integer, Float, Double;

    type:
    [Char]; (Int,Int); Int->Int

    :kind ou :k informa a reação entrada e saida de um função existente no typerclasse

    :t ou :type informa a relação entrada saida de uma função existente no universo 'dados'

    :k []
    [] :: * -> *  é um type que espera um type para virar um type.

    :k [] ::Int ou :k [In]
    [] Int :: *  nessa caso tem um tipo definido por "*"

    :k [Char] idem
    :k [Int] idem
    [char] :: *   pode ser entendido como tipo
    :k (,) é um tipo que espera dois tipos para virar um tipo.
    (,):: * -> * -> *  aridade 2

    :k (->)
    (->) :: * -> * -> *

    :k (Int, Int) é um tipo definido


    --}
    --

data Weekday = Mon
             | Tue
             | Wed
             | Thu
             | Fri
             | Sat
             | Sun
          -- deriving(Show, Eq)

instance Show Weekday where
    show Mon  = "Monday"
    show Tue  = "Tuesday"
    show Wed  = "Wednesday"
    show Thu  = "Thursday"
    show Fri  = "Friday"
    show Sat  = "Saturday"
    show Sun  = "Sunday"


instance Eq Weekday where
     Mon  == Mon  =  True
     Tue  == Tue  =  True
     Wed  == Wed  =  True
     Thu  == Thu  =  True
     Fri  == Fri  =  True
     Sat  == Sat  =  True
     Sun  == Sun  =  True
     _    == _    =  False



nextday :: Weekday -> Weekday
nextday  Mon  =  Tue
nextday  Tue  =  Wed
nextday  Wed  =  Thu
nextday  Thu  =  Fri
nextday  Fri  =  Sat
nextday  Sat  =  Sun
nextday  Sun  =  Mon

nextWorkday :: Weekday -> Weekday
nextWorkday Mon  =  Tue
nextWorkday Tue  =  Wed
nextWorkday Wed  =  Thu
nextWorkday Thu  =  Fri
nextWorkday Fri  =  Mon
nextWorkday Sat  =  Mon
nextWorkday Sun  =  Mon

nextWorkday' :: Weekday -> Weekday
nextWorkday' Mon  =  Tue
nextWorkday' Tue  =  Wed
nextWorkday' Wed  =  Thu
nextWorkday' Thu  =  Fri
nextWorkday'   _  =  Mon

nextWorkday'' :: Weekday -> Weekday
nextWorkday'' Fri  =  Mon
nextWorkday'' Sat  =  Mon
nextWorkday'' Sun  =  Mon
nextWorkday''  x   =  nextday x
