{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Chapter4.DefiningFunctions where

-- Functions can be combined into new functions

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1/n

-- Haskell has conditional expressions, which MUST have an else statement

abs :: (Ord a, Num a) => a -> a
abs n = if n >= 0 then n else (-n)

signum :: Int -> Int
signum n = if n < 0 then -1 else if n == 0 then 0 else 1

-- Guarded equations more functional ways to express conditional logic

absWithGuards :: (Ord a, Num a) => a -> a
absWithGuards n
    | n >= 0 = n
    | otherwise = -n

signumWithGuards :: (Ord a1, Num a1, Num a2) => a1 -> a2
signumWithGuards n
    | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1

-- Lambda expressions are anonymous functions, they can be used to avoid writing helpers

odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0..n-1]

halveEven :: [a] -> ([a], [a])
halveEven list
    | Prelude.even $ length list = Prelude.splitAt ((length list `div` 2) - 1 ) list
    | otherwise = ([], [])

thirdHeadTail :: [a] -> a
thirdHeadTail list
    | length list >= 3 = head . tail . tail $ list
    | otherwise =  undefined

thirdIndexing :: [a] -> a
thirdIndexing = (!! 3)

thirdMatching :: [a] -> a
thirdMatching (_ : _ : x : _) = x

safeTailGuard :: [a] -> [a]
safeTailGuard list
    | null list = []
    | otherwise = tail list

safeTailCond :: [a] -> [a]
safeTailCond list = if null list then [] else tail list

safeTailMatching :: [a] -> [a]
safeTailMatching list = case list of
    [] -> []
    xs -> tail xs

multLambda :: Int -> Int -> Int -> Int
multLambda = (\ x y z -> x * y * z)

luhnDouble :: Int -> Int
luhnDouble digit
    | digit * 2 > 9 = digit - 9
    | otherwise = digit

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (d + luhnDouble c + luhnDouble b + luhnDouble a) `rem` 10 == 0

luhnList :: [Int] -> Bool
luhnList list = (foldr ((+) . luhnDouble)  0 list) `rem` 10 == 0