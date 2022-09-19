{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Chapter6.RecursiveFunctions where 

fac :: Int -> Int 
fac 0 = 1 
fac n 
    | n > 0 = n * fac (n - 1)
    | otherwise = 0

prod :: Num a => [a] -> a
prod [] = 1
prod (n:ns) = n * product ns

reverseRec :: [a] -> [a]
reverseRec [] = []
reverseRec (x:xs) = reverseRec xs ++ [x]

-- Insertion sort can be defined recursively

insert :: Ord t => t -> [t] -> [t]
insert x [] = [x]
insert x (y:ys) 
    | x <= y = x : y : ys
    | otherwise = y : insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- recursion can also work on more than one argument at the same time
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : zip xs ys

-- Mutual recursion defines two functions recursively in terms of each other
myEven :: Int -> Bool 
myEven 0 = True 
myEven n = myOdd (n - 1)

myOdd :: Int -> Bool 
myOdd 0 = False 
myOdd n = myEven (n - 1)

{- Defining recursive functions in steps: 
- Define the type
- Enumerate the cases 
- Define the simple cases
- Defines the other cases 
-}


drop :: Integral b => b -> [a] -> [a]
drop 0 [] = []
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = Chapter6.RecursiveFunctions.drop (n - 1) xs  

sumdown :: (Eq p, Num p) => p -> p
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)   


myElem :: Eq a => a -> [a] -> Bool 
myElem _ [] = False 
myElem a (x:xs) 
    | a == x = True 
    | otherwise = myElem a xs 

myAnd :: [Bool] -> Bool
myAnd [] = True 
myAnd list = foldr (&&) True list

