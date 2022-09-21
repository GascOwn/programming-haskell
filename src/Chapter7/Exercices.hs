{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use curry" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Use list literal pattern" #-}
module Chapter7.Exercises where

import Chapter4.DefiningFunctions(luhnDouble)
import Chapter7.BinaryStringTransmitter(Bit)

filterMap :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filterMap f p = map f . filter p

myAll :: (a -> Bool) -> [a] -> Bool
myAll p = and . map p

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = or . map p

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs) 
    | p x = x : myTakeWhile p xs 
    | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile p l@(x:xs) 
    | p x = dropWhile p xs 
    | otherwise = l

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y ) 0 

luhnList :: [Int] -> Bool
luhnList list = foldr ((+) . luhnDouble)  0 list `rem` 10 == 0

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x 
    | p x = []
    | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f = \x y -> f (x, y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f = \(x, y) -> f x y 

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ (x:[]) = f x : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs

