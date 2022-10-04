{--
   :type "Hello"++"there"
   "Hello"++"there" :: [Char]

   :tyoe (++)
   (++):: [a] -> [a] -> [a]   //lista de alpha concat com lista de alpha, retonando um lista de alpha, alpha é um tipo genérico: int, char, float, etc.

    :t reverse
    reverse: [a]->[a]

    :t reverse "Hi"
    reverse "Hi" :: [Char]    // já está definido que não alpha e sim um argumento do tipo char.

    :t ("Hello","There")
    ("Hello","There") :: ([Char],[Char])

    :t 5::Int
    t 5::Int :: Integer

    :t 5
    5 :: Num p=>p  // o gchi informa a forma mais geral tipo Num que pode ser Integer, Int, float.

    :t (+)
    + :: Num a => a -> a -> a

exemplo: (+) 5 8 = 13
    :t (+) 5
    (+) 5:: Num a => a -> a
    --}


qsort :: Ord a => [a]->[a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a |a <- xs, a< x]
        larger  = [b |b <- xs, b> x]

revsort :: Ord a => [a]->[a]
revsort []     = []
revsort (x:xs) = revsort larger ++ [x] ++ revsort smaller
    where
        smaller = [a |a <- xs, a<= x]
        larger  = [b |b <- xs, b> x]


somarlista :: Num a => [a]->a
somarlista []     = 0
somarlista (x:xs) = x + somarlista xs

prodlista :: Num a => [a]->a
prodlista []     = 1
prodlista (x:xs) = x * prodlista xs

dobro :: Num a=> a->a
dobro x = x + x

factorial :: (Num a, Enum a) => a->a
factorial n = product[1..n]

average :: Foldable a=> a Int ->Int
average ns  = sum ns `div` length ns

nN = a `div` length xs
     where
        a = 10
        xs = [1,2,3,4,5]


ult :: Num a => [a]->a
ult xs = xs !! (length xs - 1)

inic :: Num a => [a]->[a]
inic [_]    = []
inic (x:xs) = [x] ++ inic xs

    {--remover os elementos iniciais de uma lista de duas formas
init2 :: Num a => [a]->[a]
init2 [_]    = []
init2 (xs) = [xs!!(1)] ++ init2 xs
fim --}

-- função soma com dois argumentos, retornando 1, usando tupla
soma     :: Num a => (a,a)->a
soma (x, y) = x + y

-- função soma cm dois argumentos, retorna um função
-- soma é associativa a esquerda a->(a->a) == a->a->associativa
-- mut é associativa a direita (a->a)->a
soma2     :: Num a=> a->a->a
soma2 x y = x + y

zeroTo   ::(Num a, Enum a) => a -> [a]
zeroTo n = [0..n]

absol :: Int -> Int
absol n = if n>= 0 then n else -n

absol2 :: Int -> Int
absol2 n | n>=0      = n
         | otherwise = -n

sgn :: Int -> Int
sgn x = if x < 0 then -1 else
            if x == 0 then 0 else 1

sgn2 :: Int -> Int
sgn2 n| n<0 = -1
      | n==0 = 0
      | otherwise = 1

factors :: Int -> [Int]
factors n = [x |x<-[1..n],n`mod`x==0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int->[Int]
primes n = [x | x<- [2..n], prime x]

find :: Eq a=> a-> [(a,b)]->[b]
find k t = [v | (k',v)<-t,k==k']

pairs :: [a]->[(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a]->Bool
sorted xs = and  [x<=y| (x,y)<- pairs xs]

positions  :: Eq a => a->[a]->[Int]
positions x xs = [i | (x',i)<-zip xs [0..n], x==x']
    where n = length xs-1

concate :: [[a]]->[a]
concate xss = [x | xs<-xss, x<-xs]

firsts :: [(a,a)] -> [a]
firsts ps = [x |(x,_)<-ps]

lengths ::Eq a => [a]->Int
lengths xs = sum [1 | _ <-xs]

isLower :: Char -> Bool
isLower c = or [ c == c' | c' <- ['a'..'z']]

    {--
let2int :: Char -> Int
let2int c = Ord c - Ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c+n) `mod` 26)
  | otherwise = c
--}
