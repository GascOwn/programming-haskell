module Chapter8.TypesAndClasses where 

-- type simply introduces a new name for an existing type
type Bit = Int
type Pos = (Int, Int)

-- functions can also be declared as types
type Trans = Pos -> Pos

-- type declarations cannot be recursive type -> type Tree = (Int, [Tree]) 
-- Types declaration can be parametrised, and even have more than one parameter 

type Pair a = (a, a)
type Assoc k v = [(k, v)] -- dictionary build from a list of tuples

-- A function that returns the first value associated with a given key:

find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k']

-- Completely new datatypes can be created with the "data" keyword

data Move = North | South | East | West 

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1) 
move South (x, y) = (x, y - 1)
move East (x, y) =  (x + 1, y)
move West (x, y) = (x - 1, y)

moves :: Pos -> [Move] -> Pos
moves = foldr move  

revMove :: Move -> Move
revMove North = South
revMove South = North
revMove East = West
revMove West = East

-- Data constructors can also have arguments 

data Shape = Circle Float | Rect Float Float 

square :: Float -> Shape 
square n = Rect n n

area :: Shape -> Float 
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- And they can be parametrized, like Maybe a 

safediv :: Int -> Int -> Maybe Int 
safediv _ 0 = Nothing 
safediv m n = Just (m `div` n)

-- Newtypes are a way of constructing a single constructor with a single argument
newtype Nat = N Int

