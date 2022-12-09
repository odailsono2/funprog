module ExVArith where

-- modify Arith to allow for variables

-- decide how to represent Assignments:
type Assignment = ()

data VArith

instance (Show VArith) where
  show = undefined

-- val evaluates an expression and returns its value
val :: Assignment -> VArith -> Integer
val = undefined

