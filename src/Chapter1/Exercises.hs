module Chapter1.Exercises where
import Chapter1.Intro

double :: Num a => a -> a
double n = n * 2

quadruple :: Num a => a -> a
quadruple n = double n * 2

myProduct :: [Integer] -> Integer
myProduct = foldr (*) 1

qsortReverse :: Ord a => [a] -> [a]
qsortReverse [] = []
qsortReverse (x:xs) = qsortReverse larger ++ [x] ++ qsortReverse smaller
    where 
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]