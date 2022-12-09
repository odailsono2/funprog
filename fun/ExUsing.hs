module ExUsing where

import Prelude hiding
    ( filter
    )

type Pred a = (a -> Bool)

-- using concat
filter :: Pred a -> [a] -> [a]
filter = undefined

-- using zipWith
sorted :: Ord a => [a] -> Bool
sorted = undefined

-- using zipWith
fibs :: Integral i => [i]
fibs = undefined


