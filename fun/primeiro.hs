module Primeiro where

errado :: [Char]
errado = "not good"

five :: Integer
five = 5

addfive :: Integer -> Integer
addfive 0    = 100
addfive 1    = 10 + five
addfive five = five + 5

sempreSix :: Integer -> Integer
sempreSix _ = 6

average :: Fractional a => a -> a -> a
average x y = ( x + y ) / 2


