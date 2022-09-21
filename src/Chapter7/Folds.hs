{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Chapter7.Folds where 
import Data.Bifoldable (Bifoldable)

-- Folds basically apply expresses a pattern of recursion for defining functions on list
-- f [] = v / f (x:xs) = x # f xs => foldr f v xs 

recSum :: [Int] -> Int
recSum [] = 0
recSum (x:xs) = x + recSum xs

foldSum :: [Int] -> Int
foldSum xs = foldr (+) 0 xs 

recProd :: [Int] -> Int
recProd [] = 1 
recProd (x:xs) = x + recProd xs

foldProd :: [Int] -> Int
foldProd xs = foldr (*) 1 xs 

recAnd :: [Bool] -> Bool 
recAnd [] = True
recAnd (x:xs) = x && and xs 

foldAnd :: [Bool] -> Bool 
foldAnd xs = foldr (&&) True xs

recOr :: [Bool] -> Bool 
recOr [] = False
recOr (x:xs) = x || and xs 

foldOr :: [Bool] -> Bool 
foldOr xs = foldr (||) False xs

-- Folds replace the cons operator with the function and the accumulator with the empty list
-- True : True : True : True : [] => True && (True && (True && (True && True))) 

-- foldr maps the empty list to v and non-empty list to the function f applied
-- to the head of the list and the recursively processed tail.
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ v [] = v
myFoldr f v (x:xs) = f x (foldr f v xs)

-- foldr folds to the right, appending the value at the end of the list
-- 1 : 2 : 3 : 4 : [] => 1 + (2 + (3 + (4 + 0)))
-- other functions can be defined through folds

recLength :: [a] -> Int
recLength [] = 0
recLength (_:xs) = 1 + length xs

foldLength :: [a] -> Int
foldLength = foldr (\_ n -> 1 + n) 0

-- 1 + (2 + (3 + (4 + []))) becomes 1 + (1 + (1 + (1 + 0)))

foldReverse :: [a] -> [a]
foldReverse = foldr (\x xs -> xs ++ [x]) []