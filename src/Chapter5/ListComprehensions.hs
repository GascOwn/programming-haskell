module Chapter5.ListComprehensions where 


{- This comes from set notation: 
- | means "such that"
- <- means "draw from",
x <- [1..5] is a generator -}

exampleComprehension :: [Integer]
exampleComprehension = [ x^2 | x <- [1..5] ]

-- multiple generators are possible

doubleGenerator :: [(Integer, Integer)]
doubleGenerator = [(x, y) | x <- [1,2,3], y <-[4,5]]

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

-- In this case the generator is simply a counter
length :: [a] -> Int
length xs = sum [1 | _ <- xs]

-- Comprehensions have guards after generators, separated by commas
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]