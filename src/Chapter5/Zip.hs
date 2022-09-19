module Chapter5.Zip where

import Data.Char 
import Chapter5.ListComprehensions (factors)

-- zip creates a tuple of lists from two lists, stopping when the shorter one ends
zipTwo :: [(Char, Integer)]
zipTwo = zip ['a', 'b', 'c'] [1,2,3,4]

pairs :: [b] -> [(b, b)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char 
shift n c 
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

squareSum :: Integer
squareSum = sum [x^2 | x <- [1..100]]

grid :: Int -> Int -> [(Int, Int)] 
grid a b = [(x, y) | x <- [0..a], y <- [0..b]]

square :: Int -> [(Int, Int)]
square n =  [ (x, y) | (x, y) <- grid n n, x /= y] 

replicate :: (Num t, Enum t) => t -> a -> [a]
replicate n f = [ f  | _ <- [1..n] ]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], 
                       y <- [1..n], 
                       z <- [1..n], 
                       x^2 + y^2 == z^2 ]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x ]