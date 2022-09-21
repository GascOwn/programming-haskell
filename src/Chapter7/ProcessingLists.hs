module Chapter7.ProcessingLists where

-- Prelude has a lot of useful functions for processing lists, one of which is map
-- Map applies a function to each member of a list

mapp :: (a -> b) -> [a] -> [b]
mapp f xs = [f x | x <- xs]

rMap :: (a -> b) -> [a] -> [b]
rMap _ [] = []
rMap f (x:xs) = f x : rMap f xs

-- Filter returns a list of all the values in a list that satisfy a condition

filterr :: (a -> Bool) -> [a] -> [a]
filterr p xs = [x | x <- xs, p x]

rFilter :: (a -> Bool) -> [a] -> [a]
rFilter _ [] = []
rFilter p (x:xs) 
    | p x = x : rFilter p xs
    | otherwise = rFilter p xs

-- map and filter can be combined 
-- eg: a function that sums the squares of all the even numbers in a list
sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))
